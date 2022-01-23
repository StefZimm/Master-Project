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

#' @title get_boxplot creates boxplot
#' 
#' @description get_boxplot creates boxplot with a maximum of three grouping 
#'              variables
#'
#' @param table aggregated table name (e.g. "pglabnet_year_bula_h.csv") as character
#' @param variable variable for output as character
#' @param diffcount number of grouping variables as numeric
#' @param diffvar2 group variable (e.g. "bula_h")
#' @param diffvar3 group variable (e.g. "sex")
#' 
#' @return plot = boxplot with grouping
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_boxplot
#'  
#' @examples
#'       get_boxplot(table = data, 
#'                   variable = "year", 
#'                   diffcount = 2,
#'                   diffvar2 = "alter_gr",
#'                   diffvar3 = "")

get_boxplot <- function(table, variable, diffcount, diffvar2, diffvar3){
  
  title <- meta$label_de[meta$variable==variable]
  
  if (diffcount == 1) {
    data <- subset(table, select=c(year, min, max, median, 
                                   ptile10, ptile25, ptile75, ptile90, ptile99))
    
    plot <- ggplot(data, aes(factor(year))) +  
      geom_boxplot(data = data,
                   aes(ymin = min, lower = ptile25 , 
                       middle = median, upper = ptile90, ymax = ptile99),
                   stat = "identity") +
      theme(strip.background = element_blank(), axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90), axis.title.y=element_blank())+
      labs(title = title, 
           caption = "Data: SOEP-Core v.36")
  }
  
  if (diffcount == 2) {
    data <- subset(table, select=c(year, eval(parse(text = diffvar2)), min, max, median, 
                                   ptile10, ptile25, ptile75, ptile90, ptile99)) 
    
    plot <- ggplot(data, aes(factor(year), fill = eval(parse(text = diffvar2)))) +  
      geom_boxplot(data = data,
                   aes(ymin = min, lower = ptile25 , 
                       middle = median, upper = ptile90, ymax = ptile99),
                   stat = "identity") + 
      theme(strip.background = element_blank(), axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90), axis.title.y=element_blank(),
            legend.title=element_blank())+
      labs(title = title, 
           caption = "Data: SOEP-Core v.36")
  }
  
  if (diffcount == 3) {
    data <- subset(table, select=c(year, eval(parse(text = diffvar2)), 
                                   eval(parse(text = diffvar3)), min, max, median, 
                                   ptile10, ptile25, ptile75, ptile90, ptile99)) 
    
    plot <- ggplot(data, aes(factor(year), fill = eval(parse(text = diffvar2)))) +  
      geom_boxplot(data = data,
                   aes(ymin = min, lower = ptile25 , 
                       middle = median, upper = ptile90, ymax = ptile99),
                   stat = "identity") +
      theme(strip.background = element_blank(), axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90), axis.title.y=element_blank(),
            legend.title=element_blank())+
      labs(title = title, 
           caption = "Data: SOEP-Core v.36")+
      facet_wrap(~eval(parse(text = diffvar3)))
  }
  
  return(plot)
}

################################################################################

# # Test Center
# 
# variable <- "pgtatzeit"
# diffvar2 <- "sampreg"
# diffvar3 <- "sex"
# path <- paste0("C:/git/Master-Project/tables/numerical/", variable, "/")
# 
# if (diffvar2 == "" & diffvar3=="") {
#   diffcount <- 1
#   csv  <- paste0(variable, "_year.csv")
# }
# if (diffvar2 != "" & diffvar3=="" ) {
#   diffcount <- 2
#   csv  <- paste0(variable, "_year_", diffvar2, ".csv")
# }
# if (diffvar3 != "" & diffvar3!="" ) {
#   diffcount <- 3
#   csv  <- paste0(variable, "_year_", diffvar2, "_", diffvar3, ".csv")
# }
# 
# data <- read.csv(file = paste0(path, csv))
# meta <- read.csv(paste0(metapath, "variables.csv") , header = TRUE,
#                  colClasses = "character")
# 
# get_boxplot(table = data, variable = variable, diffcount = diffcount, 
#             diffvar2 = diffvar2, diffvar3= diffvar3)


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
