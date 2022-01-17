################################################################################
# Functions #
################################################################################

#' @title get_map_plot creates median or mean maps of germany and federal states
#'
#' @description get_map_plot interactive plotly 
#'              median or mean maps for variable of germany and federal states 
#'
#' @param table aggregated table name (e.g. "pglabnet_year_bula_h.csv") as character
#' @param syear survey year of aggregated table as numeric
#' @param variable variable for output as character
#' @param statistic statistic definition ("mean" or "median")
#' @param diffvar group variable (e.g. "sex")
#' 
#' @return plotly map with medians or means
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_map_plot
#'  
#' @examples
#'             get_map_plot(table = table, 
#'                          syear = syear,
#'                          variable = variable, 
#'                          statistic = "mean",
#'                          diffvar = diffvar)
library(tidyverse)###DB added to create function###

get_map_plot <- function(table, syear, variable, statistic, diffvar){
  title <- paste0(meta$label_de[meta$variable==variable], " nach Bundesland")
  
  dataset <- data  %>%
    filter(year == syear)
  
  colnames(dataset)[colnames(dataset)=="bula_h"] <- "NAME_1"
  state_level_map <- raster::getData("GADM", country = "Germany", level = 1) %>%
    st_as_sf()
  
  state_level_map$NAME_1 <- str_replace(state_level_map$NAME_1, "ü", "ue")
  state_level_map$HASC_1 <- str_replace(state_level_map$HASC_1, "DE.", "")
  
  germanydata <- left_join(state_level_map, dataset, by = "NAME_1") 
  germanydata <- cbind(germanydata, st_coordinates(st_centroid(germanydata)))
  germanydata$Y[germanydata$HASC_1=="BR"] <- germanydata$Y[germanydata$HASC_1=="BR"] + 0.4
  germanydata$X[germanydata$HASC_1=="BR"] <- germanydata$X[germanydata$HASC_1=="BR"] + 0.4
  
  if (statistic == "mean") {
    if (diffvar == "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = mean) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="Gewichteter Mittelwert")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")
    }
    if (diffvar != "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = mean) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="Gewichteter Mittelwert")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")+
        facet_wrap(~eval(parse(text = diffvar)))+
        theme(strip.background = element_blank())
    } 
  }
  
  if (statistic == "median") {
    if (diffvar == "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = median) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="Gewichteter Median")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")
    }
    if (diffvar != "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = median) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="Gewichteter Median")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")+
        facet_wrap(~eval(parse(text = diffvar)))+
        theme(strip.background = element_blank())
    } 
  }
  
  ggplotly(germanmedianplot)      
}

################################################################################

# Test Center

# library(raster) ##Geographic data analysis __ DB##
# library(sf) ##Special features for spatial feature data __ DB##
# library(rgeos) ##Spacial geometries...will be retired 2023: https://cran.r-project.org/web/packages/rgeos/index.html __ DB##
# library(dplyr)
# library(tools)
# library(stringr)
# library(ggplot2)
# library(plotly) ##For interactive graphs __ DB##
# 
# 
# ###PACMAN optional, must install pacman first __ DB###
# pacman::p_load(raster, sf, regeos, dplyr, tools, stringr, ggplot2, plotyly)
# # 
# # plh0182 pglabnet pgvebzeit
# 
# #metapath <- "C:/git/soep-transfer/meta/p_platform/variables.csv"
# metapath <- "metadata/p_data/variables.csv"
# variable <- "plh0182"
# diffvar <- "sex"
# syear <- "2018"
# 
# if (diffvar == "") {
#   table <- paste0(variable,"_year_bula_h.csv")
#   }
# if (diffvar != "") {
#   table <-paste0(variable,"_year_bula_h_", diffvar, ".csv")
# }
# if (diffvar == "alter_gr") {
#   table <-paste0(variable,"_year_", diffvar, "_bula_h.csv")
# }
# 
# #data <- read.csv(file = paste0("C:/git/soep-transfer/numerical/", variable, "/", table),
# #                 encoding = "UTF-8")
# 
# data <- read.csv(file = paste0("tables/numerical/", variable, "/", table),
#                  encoding = "UTF-8")
# meta <- read.csv(file = metapath, encoding = "UTF-8")
# 
# get_map_plot(table = table,
#              syear = syear,
#              variable = variable,
#              statistic = "median",
#              diffvar = diffvar)
