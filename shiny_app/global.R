# This code includes dependencies, functions, and data sets that will be used for the app

###########################
## Install dependencies
###########################
required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate ='2022-03-27')
library(wesanderson)
library(randomcoloR)
library(raster)
library(sf)
library(rgeos) 
library(dplyr)
library(tools)
library(stringr)
library(ggplot2)
library(randomcoloR) 
library(tidyverse)
library(randomcoloR)
library(shiny)
library(plotly)
library(broom)
library(haven)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(rintrojs)
library(tidyr)
library(shinyBS)
library(shinythemes)
library(DT)
library(shinycssloaders)
#library(shinyLP) #use if customizing theme with HTML
#library(bslib) #use if customizing theme

####################################
##I am commenting this out for now, but if we chose to use a separate file to source dependencies, we can use this

#source('scripts/dependencies.R')
#load packages
#lapply(required_packages, require, character.only=TRUE)
####################################


# will need this for variable button
# all variables with meantable yes/no must be selectable
# labels should be read as var name
# variables should fall into topic categories

variables <- read.csv("../metadata/p_data/variables.csv")
var_cat <- read.csv("../metadata/p_data/variable_categories.csv")

var_links <- variables %>%
  select(label_de, topic, variable, var_link, paper_link) %>%
  arrange((topic), label_de) %>%
  rename(Variable_Label = label_de,
         Study_Topic = topic,
         Variable_Name = variable,
         Variable_Information = var_link,
         Variable_Paper = paper_link)

# path for tables, not sure how to load in only certain tables or if it can even be done this way. It seems like all tables will need to be loaded
tables <- "tables/"

#topics
top_inc     <- variables %>% filter(topic == "Income, Taxes, and Social Security") %>% select(label_de)
top_inc_cat <- variables %>% filter(topic == "Income, Taxes, and Social Security" & meantable == "No") %>% select(label_de)
top_inc_num <- variables %>% filter(topic == "Income, Taxes, and Social Security" & meantable == "Yes") %>% select(label_de)
top_att     <- variables %>% filter(topic == "Attitudes, Values, and Personality") %>% select(label_de)
top_health  <- variables %>% filter(topic == "Health and Care") %>% select(label_de)
top_health_cat <- variables %>% filter(topic == "Health and Care" & meantable == "No") %>% select(label_de)
top_health_num <- variables %>% filter(topic == "Health and Care" & meantable == "Yes") %>% select(label_de)
top_home    <- variables %>% filter(topic == "Home, Amenities, and Contributions of Private HH") %>% select(label_de)
top_time    <- variables %>% filter(topic == "Time Use and Environmental Behavior") %>% select(label_de)
top_emp     <- variables %>% filter(topic == "Work and Employment") %>% select(label_de)

top_demo <- variables %>%
  filter(meantable == "demo") 

 

################################################################################
# Functions #
################################################################################

# pages

# this function creates the boxes for the main page, these will be used for topics

lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
  )
}

# this function creates smaller boxes on the landing page, these will be optional if we want to include more info on the landing page

lp_about_box <- function(title_box, image_name, button_name, description) {
  
  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      (actionButton(button_name, NULL,
                    class="landing-page-button",
                    icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}

## graphs

# Color Definition
color.palette <- unique(c(wes_palette("Zissou1"), wes_palette("GrandBudapest1"), 
                          wes_palette("Moonrise1"), wes_palette("Moonrise2"),
                          wes_palette("GrandBudapest2"), wes_palette("Cavalcanti1"), 
                          wes_palette("Royal1"), wes_palette("Royal2"),
                          # 2000 random colors
                          distinctColorPalette(1999)))
################################################################################
# Functions #
################################################################################
# Color Definition
color.palette <- unique(c(wes_palette("Zissou1"), wes_palette("GrandBudapest1"), 
                          wes_palette("Moonrise1"), wes_palette("Moonrise2"),
                          wes_palette("GrandBudapest2"), wes_palette("Cavalcanti1"), 
                          wes_palette("Royal1"), wes_palette("Royal2"),
                          # 2000 random colors
                          distinctColorPalette(1999)))

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


get_map_plot <- function(table, syear, variable, statistic, diffvar){
  
  
  title <- paste0(meta$label_de[meta$variable==variable], " in federal states", " in ", syear)
  
  dataset <- data  %>%
    filter(year == syear)
  
  state_level_map <- raster::getData("GADM", country = "Germany", level = 1) %>%
    st_as_sf()
  
  if("NAME_1" %in% colnames(state_level_map)){
    state_level_map$NAME_1 <-state_level_map$NAME_1 %>%
      gsub("Berlin", "Berlin", .) %>%
      gsub("Brandenburg", "Brandenburg", .) %>%
      gsub("Mecklenburg-Vorpommern", "Mecklenburg-Western Pomerania", .) %>%
      gsub("Sachsen", "Saxony", .) %>%
      gsub("Sachsen-Anhalt", "Saxony-Anhalt", .) %>%
      gsub("Thüringen", "Thuringia", .) %>% 
      gsub("Schleswig-Holstein", "Schleswig-Holstein", .) %>%
      gsub("Hamburg", "Hamburg", .) %>%
      gsub("Niedersachsen", "Lower Saxony", .) %>%
      gsub("Bremen", "Bremen", .) %>%
      gsub("Nordrhein-Westfalen", "North Rhine-Westphalia", .) %>%
      gsub("Hessen", "Hesse", .) %>%
      gsub("Rheinland-Pfalz", "Rhineland-Palatinate,Saarland", .) %>% 
      gsub("Baden-Württemberg", "Baden-Württemberg", .) %>% 
      gsub("Bayern", "Bavaria", .)
  }
  
  colnames(dataset)[colnames(dataset)=="bula_h"] <- "NAME_1"
  state_level_map$HASC_1 <- str_replace(state_level_map$HASC_1, "DE.", "")
  
  germanydata <- left_join(state_level_map, dataset, by = "NAME_1") 
  germanydata <- cbind(germanydata, st_coordinates(st_centroid(germanydata)))
  germanydata$Y[germanydata$HASC_1=="BR"] <- germanydata$Y[germanydata$HASC_1=="BR"] + 0.4
  germanydata$X[germanydata$HASC_1=="BR"] <- germanydata$X[germanydata$HASC_1=="BR"] + 0.4
  
  germanydata <- germanydata %>%
    filter(NAME_1 != "Saarland") 
  
  if (statistic == "mean") {
    if (diffvar == "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = mean) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="weighted mean")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")
    }
    if (diffvar != "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = mean) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="weighted mean")+ 
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
             caption = "Data: SOEP-Core v.36", fill="weighted median")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")
    }
    if (diffvar != "") {
      germanmedianplot <- germanydata %>%
        ggplot() +  
        ggplot2::geom_sf() +
        aes(fill = median) +
        labs(title = title, 
             caption = "Data: SOEP-Core v.36", fill="weighted median")+ 
        ggpubr::theme_transparent()+
        geom_text(aes(X, Y, label = HASC_1), size = 3, colour="white")+
        facet_wrap(~eval(parse(text = diffvar)))+
        theme(strip.background = element_blank())
    } 
  }
  
  ggplotly(germanmedianplot)      
}

#' ################################################################################
#' 
#' #' @title get_boxplot creates boxplot
#' #' 
#' #' @description get_boxplot creates boxplot with a maximum of three grouping 
#' #'              variables
#' #'
#' #' @param table aggregated table name (e.g. "pglabnet_year_bula_h.csv") as character
#' #' @param variable variable for output as character
#' #' @param diffcount number of grouping variables as numeric
#' #' @param diffvar2 group variable (e.g. "bula_h")
#' #' @param diffvar3 group variable (e.g. "sex")
#' #' 
#' #' @return plot = boxplot with grouping
#' #'
#' #' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' #' @keywords get_boxplot
#' #'  
#' #' @examples
#' #'       get_boxplot(table = data, 
#' #'                   variable = "year", 
#' #'                   diffcount = 2,
#' #'                   diffvar2 = "alter_gr",
#' #'                   diffvar3 = "")
#' 
#' get_boxplot <- function(table, variable, diffcount, diffvar2, diffvar3){
#'   
#'   title <- meta$label_de[meta$variable==variable]
#'   
#'   if (diffcount == 1) {
#'     data <- subset(table, select=c(year, min, max, median, 
#'                                    ptile10, ptile25, ptile75, ptile90, ptile99))
#'     
#'     plot <- ggplot(data, aes(factor(year))) +  
#'       geom_boxplot(data = data,
#'                    aes(ymin = min, lower = ptile25 , 
#'                        middle = median, upper = ptile90, ymax = ptile99),
#'                    stat = "identity") +
#'       theme(strip.background = element_blank(), axis.title.x=element_blank(),
#'             axis.text.x = element_text(angle = 90), axis.title.y=element_blank())+
#'       labs(title = title, 
#'            caption = "Data: SOEP-Core v.36")
#'   }
#'   
#'   if (diffcount == 2) {
#'     data <- subset(table, select=c(year, eval(parse(text = diffvar2)), min, max, median, 
#'                                    ptile10, ptile25, ptile75, ptile90, ptile99)) 
#'     
#'     plot <- ggplot(data, aes(factor(year), fill = eval(parse(text = diffvar2)))) +  
#'       geom_boxplot(data = data,
#'                    aes(ymin = min, lower = ptile25 , 
#'                        middle = median, upper = ptile90, ymax = ptile99),
#'                    stat = "identity") + 
#'       theme(strip.background = element_blank(), axis.title.x=element_blank(),
#'             axis.text.x = element_text(angle = 90), axis.title.y=element_blank(),
#'             legend.title=element_blank())+
#'       labs(title = title, 
#'            caption = "Data: SOEP-Core v.36")
#'   }
#'   
#'   if (diffcount == 3) {
#'     data <- subset(table, select=c(year, eval(parse(text = diffvar2)), 
#'                                    eval(parse(text = diffvar3)), min, max, median, 
#'                                    ptile10, ptile25, ptile75, ptile90, ptile99)) 
#'     
#'     plot <- ggplot(data, aes(factor(year), fill = eval(parse(text = diffvar2)))) +  
#'       geom_boxplot(data = data,
#'                    aes(ymin = min, lower = ptile25 , 
#'                        middle = median, upper = ptile90, ymax = ptile99),
#'                    stat = "identity") +
#'       theme(strip.background = element_blank(), axis.title.x=element_blank(),
#'             axis.text.x = element_text(angle = 90), axis.title.y=element_blank(),
#'             legend.title=element_blank())+
#'       labs(title = title, 
#'            caption = "Data: SOEP-Core v.36")+
#'       facet_wrap(~eval(parse(text = diffvar3)))
#'   }
#'   plot <- plot + coord_flip()
#'   return(plot)
#' }

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
#'                   diffvar2 = "alter_gr",
#'                   diffvar3 = "")

get_boxplot <- function(table, meta, variable, diffvar2, diffvar3){
  
  title <- meta$label_de[meta$variable==variable]
  
  if (diffvar2 != "" & diffvar3 != "") { 
    groupdata <- table %>%
      unite(combined_group, diffvar2,  diffvar3, sep=", ")
  }
  
  if (diffvar2 != "" & diffvar3 == "") { 
    groupdata <- table %>%
      unite(combined_group, diffvar2, sep=", ")
  }
  
  if (diffvar2 == "" & diffvar3 == "") { 
    groupdata <- table 
    
    plot <- plot_ly(data = groupdata,
                    x = as.factor(groupdata$year),
                    colors = color.palette) %>% 
      add_trace(lowerfence = ~min, q1 = ~ptile25 , median = ~median, 
                q3 = ~ptile75, upperfence = ~ptile99, type = "box") %>% 
      layout(boxmode = "group", title = title,
             xaxis = list(tickangle=90),
             yaxis = list(range = list(0,max(groupdata$ptile99)))) %>%
      rangeslider()
  }
  else{  plot <- plot_ly(data = groupdata, 
                         color = ~combined_group,
                         x = as.factor(groupdata$year),
                         colors = color.palette) %>% 
    add_trace(lowerfence = ~min, q1 = ~ptile25 , median = ~median, 
              q3 = ~ptile75, upperfence = ~ptile99, type = "box") %>% 
    layout(boxmode = "group", title = title,
           xaxis = list(tickangle=90),
           yaxis = list(range = list(0,max(groupdata$ptile99)))) %>%
    rangeslider()
  }
  
  return(plot)
}

################################################################################

get_lineplot <- function(data, meta, variable, diffvar1, diffvar2, diffcount, 
                         start, end, ci){
  
  title <- meta$label_de[meta$variable==variable]
  
  if (diffcount == 1) {  
    
    data <- data %>%
      mutate(sd = mean - lowerci_mean)
    
    if (ci == TRUE) {
      
      plot <- plot_ly(
        data = data,
        x = ~year, 
        y = ~mean,
        colors = color.palette,
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 4, dash = "dot"),
        error_y = ~list(array = sd)) %>% 
        layout(title = title, 
               xaxis = list(title = 'year', range = list(start,end), 
                            tickvals = as.list(seq(1984,2019)),
                            tickangle=90, tickfont = list(family='Rockwell', 
                                                          size=14)),
               hovermode = "x unified") %>%
        rangeslider(start, end)
      
    } 
    
    if (ci == FALSE) {
      
      plot <-   plot_ly(
        data = data,
        x = ~year, 
        y = ~mean,
        colors = color.palette,
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 4, dash = "dot")) %>% 
        layout(title = title, 
               xaxis = list(title = 'year', range = list(start,end), 
                            tickvals = as.list(seq(1984,2019)),
                            tickangle=90, tickfont = list(family='Rockwell', 
                                                          size=14)),
               hovermode = "x unified") %>%
        rangeslider(start, end)
    }  
  }
  
  if (diffcount == 2) {  
    colnames(data)[which(names(data) == diffvar1)] <- "combined_group"
    data <- data  %>%
      mutate(sd = mean - lowerci_mean) %>%
      filter(is.na(sd)!=1) 
  }
  
  if (diffcount == 3) {
    data <- data %>%
      unite(combined_group, diffvar1, diffvar2, sep="; ") %>%
      mutate(sd = mean - lowerci_mean) %>%
      filter(is.na(sd)!=1) 
  }
  
  if ((diffcount == 2 | diffcount == 3) & ci == FALSE) {
    
    plot <-  plot_ly(
      data = data,
      x = ~year, 
      y = ~mean,
      color = ~combined_group,
      colors = color.palette,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 4, dash = "dot")) %>% 
      layout(title = title, 
             xaxis = list(title = 'year', range = list(start,end), 
                          tickvals = as.list(seq(1984,2019)),
                          tickangle=90, tickfont = list(family='Rockwell', 
                                                        size=14)),
             hovermode = "x unified") %>%
      rangeslider(start, end)
    
  }  
  
  if ((diffcount == 2 | diffcount == 3) & ci == TRUE) {
    
    data$sd[is.na(data$sd)] <- 0
    data %>%
      group_by(year, combined_group) %>%
      ungroup()
    
    data2 <- data[c("year", "combined_group", "mean", "sd")] %>%
      gather(key, value, -c(year, combined_group)) %>%
      mutate(ref = paste0(combined_group, ifelse(key == 'sd', '_sd', ''))) %>%
      select(-combined_group, -key) %>%
      spread(ref, value)
    
    # set layout
    plot <- plot_ly(data2, type = 'scatter', 
                    mode = "lines+markers") %>%
      layout(title = title, 
             xaxis = list(title = 'year', range = list(start,end), 
                          tickvals = as.list(seq(1984,2019)),
                          tickangle=90, tickfont = list(family='Rockwell', 
                                                        size=14)),
             hovermode = "x unified") %>%
      rangeslider(start, end)
    
    # create plot
    for (g in unique(data$combined_group)) {
      print(g)
      print(is.na(data2[[g]]))
      
      if (is.na(data2[[g]]) == FALSE) {
        plot <- add_trace(plot, x = data2[['year']], 
                          y = data2[[g]], 
                          name = g, 
                          error_y = list(array = data2[[paste0(g, '_sd')]]))
      }
      
      print(head(data2))
      if (is.na(data2[[g]]) == TRUE) {
        data2 <- data2 %>%
          filter(is.na(data2[[g]])!=1) 
        
        plot <- add_trace(plot, x = data2[['year']], 
                          y = data2[[g]], 
                          name = g, 
                          error_y = list(array = data2[[paste0(g, '_sd')]]))
      }
    }
    
  }  
  return(plot)
}

################################################################################

get_percent_lineplot <- function(data, meta, variable, diffvar1, diffvar2, diffcount, 
                                 start, end, ci){
  
  title <- meta$label_de[meta$variable==variable]
  
  if (diffcount == 1) {  
    
    data <- data %>%
      mutate(sd = percent - lower_confidence)
    colnames(data)[which(names(data) == variable)] <- "combined_group"
    
  } 
  
  if (diffcount == 2) {  
    
    data <- data %>%
      unite(combined_group, variable, diffvar1, sep="; ") %>%
      mutate(sd = percent - lower_confidence) %>%
      filter(is.na(sd)!=1) 
    
  } 
  
  if (diffcount == 3) {  
    
    data <- data %>%
      unite(combined_group, variable, diffvar1, diffvar2, sep=", ") %>%
      mutate(sd = percent - lower_confidence) %>%
      filter(is.na(sd)!=1) 
    
  } 
  
  if (ci == FALSE) {  
    
    plot <-  plot_ly(
      data = data,
      x = ~year, 
      y = ~percent,
      color = ~combined_group,
      colors = color.palette,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 4, dash = "dot")) %>% 
      layout(title = title, 
             xaxis = list(title = 'year', range = list(start,end), 
                          tickvals = as.list(seq(1984,2019)),
                          tickangle=90, tickfont = list(family='Rockwell', 
                                                        size=14)),
             hovermode = "x unified") %>%
      rangeslider(start, end)
    
  }
  
  if (ci == TRUE) {  
    
    data$sd[is.na(data$sd)] <- 0
    data %>%
      group_by(year, combined_group) %>%
      ungroup()
    
    data2 <- data[c("year", "combined_group", "percent", "sd")] %>%
      gather(key, value, -c(year, combined_group)) %>%
      mutate(ref = paste0(combined_group, ifelse(key == 'sd', '_sd', ''))) %>%
      dplyr::select(-combined_group, -key) %>%
      spread(ref, value)
    
    # set layout
    plot <- plot_ly(data2, type = 'scatter', 
                    mode = "lines+markers") %>%
      layout(title = title, 
             xaxis = list(title = 'year', range = list(start,end), 
                          tickvals = as.list(seq(1984,2019)),
                          tickangle=90, tickfont = list(family='Rockwell', 
                                                        size=14)),
             hovermode = "x unified") %>%
      rangeslider(start, end)
    
    # create plot
    for (g in unique(data$combined_group)) {
      print(g)
      print(is.na(data2[[g]]))
      
      if (is.na(data2[[g]]) == FALSE) {
        plot <- add_trace(plot, x = data2[['year']], 
                          y = data2[[g]], 
                          name = g, 
                          error_y = list(array = data2[[paste0(g, '_sd')]]))
      }
      
      if (is.na(data2[[g]]) == TRUE) {
        data2 <- data2 %>%
          filter(is.na(data2[[g]])!=1) 
        
        plot <- add_trace(plot, x = data2[['year']], 
                          y = data2[[g]], 
                          name = g, 
                          error_y = list(array = data2[[paste0(g, '_sd')]]))
      }
    }
  }
  return(plot)
}  


get_user_table <- function(meta, variable, diffvar1, diffvar2, heatmap){
  
  var_vector <- c(variable, "year")
  
  if (heatmap == FALSE) {  
    diffvar_vector<- sort(c(diffvar1[diffvar1 != ""], diffvar2[diffvar2 != ""]))
    vector <- c(var_vector, diffvar_vector[diffvar_vector != ""])
    vector <- paste(vector, collapse = "_")  
    table_csv <- paste0(vector, ".csv")
  }
  
  if (heatmap == TRUE) {
    diffvar_vector <- sort(c("bula_h", diffvar1[diffvar1 != ""]))
    vector <- c(var_vector, diffvar_vector[diffvar_vector != ""])
    vector <- paste(vector, collapse = "_")  
    table_csv <- paste0(vector, ".csv")
  }
  
  return(table_csv)
}

get_barplot <- function(data, meta, variable, diffvar1, diffvar2, plottype, ci, 
                        start, end){
  
  title <- meta$label_de[meta$variable==variable]
  
  if (diffvar1 != "" & diffvar2 != "") { 
    groupdata <- data %>%
      unite(combined_group, variable, diffvar1,  diffvar2, sep=", ")
    
    groupdata2 <- data %>%
      unite(combined_group2, diffvar1,  diffvar2, sep=", ")
  }
  
  if (diffvar1 != "" & diffvar2 == "") { 
    groupdata <- data %>%
      unite(combined_group, variable, diffvar1, sep=", ")
    
    groupdata2 <- data %>%
      unite(combined_group2, diffvar1,  sep=", ")
  }
  
  if (diffvar1 == "" & diffvar2 == "") { 
    groupdata <- data %>%
      unite(combined_group, variable,  sep=", ")
    
    groupdata2 <- data %>% 
      mutate(combined_group2 = variable)
  }
  
  groupdata <- groupdata %>%
    mutate(sd = round((percent - lower_confidence)*100, digits = 2)) %>%
    mutate(percent = round(percent*100, digits = 2)) %>%
    mutate(lower_confidence = round(percent-sd, digits = 2)) %>%
    mutate(upper_confidence = round(percent+sd, digits = 2)) %>%
    filter(is.na(sd)!=1) 
  
  groupdata2 <- groupdata2 %>%
    mutate(sd = round((percent - lower_confidence)*100, digits = 2)) %>%
    filter(is.na(sd)!=1) 
  
  data <- cbind(groupdata, groupdata2[c(variable, "combined_group2")])
  
  if (plottype == "dodge") { 
    # dodged barplot
    plot <- plot_ly(data, 
                    x = ~year, 
                    y = ~percent, 
                    color = ~combined_group,
                    colors = color.palette,
                    type = 'bar') %>% 
      layout(title = title, 
             xaxis = list(title = 'year', range = list(start,end), 
                          tickvals = as.list(seq(1984,2019)),
                          tickangle=90, tickfont = list(family='Rockwell', 
                                                        size=14)),
             hovermode = "x unified") %>%
      rangeslider(start, end)
    
    if (ci == TRUE) { 
      data$sd[is.na(data$sd)] <- 0
      data %>%
        group_by(year, combined_group) %>%
        ungroup()
      
      data2 <- data[c("year", "combined_group", "percent", "sd")] %>%
        gather(key, value, -c(year, combined_group)) %>%
        mutate(ref = paste0(combined_group, ifelse(key == 'sd', '_sd', ''))) %>%
        dplyr::select(-combined_group, -key) %>%
        spread(ref, value)
      
      # set layout
      plot <- plot_ly(data2, type = 'bar',
                      colors = color.palette) %>%
        layout(title = title, 
               xaxis = list(title = 'year', range = list(start,end), 
                            tickvals = as.list(seq(1984,2019)),
                            tickangle=90, tickfont = list(family='Rockwell', 
                                                          size=14)),
               hovermode = "x unified") %>%
        rangeslider(start, end)
      
      
      for (g in unique(data$combined_group)) {
        
        if (is.na(data2[[g]]) == FALSE) {
          plot <- add_trace(plot, x = data2[['year']], 
                            y = data2[[g]], 
                            name = g, 
                            error_y = list(array = data2[[paste0(g, '_sd')]]))
        }
        
        if (is.na(data2[[g]]) == TRUE) {
          data2 <- data2 %>%
            filter(is.na(data2[[g]])!=1) 
          
          plot <- add_trace(plot, x = data2[['year']], 
                            y = data2[[g]], 
                            name = g, 
                            error_y = list(array = data2[[paste0(g, '_sd')]]))
        }
      }
    }
  }  
  
  if (plottype == "stack") { 
    
    if (diffvar1 == "" & diffvar2 == "") { 
      # stacked barplot
      plot <- ggplot(data, aes(fill=eval(parse(text = variable)), 
                               y=percent, x=as.character(year),
                               text = paste('Year: ', year,
                                            '<br>Percent:', percent, 
                                            '<br>Variable:',eval(parse(text = variable))))) + 
        geom_bar(position="fill", stat="identity")+
        scale_y_continuous(labels=scales::percent) +
        theme(legend.title=element_blank()) +
        theme(strip.background = element_blank(), axis.title.x=element_blank(),
              axis.text.x = element_text(angle = 90), axis.title.y=element_blank())+
        labs(title = title, 
             caption = "Data: SOEP-Core v.36")+
        guides(fill=guide_legend(title=""))
      
    }
    
    # stacked barplot
    else { 
      plot <- ggplot(data, aes(fill=eval(parse(text = variable)), 
                               y=percent, x=as.character(year),
                               text = paste('Year: ', year,
                                            '<br>Percent:', percent, 
                                            '<br>Variable', eval(parse(text = variable))))) + 
        geom_bar(position="fill", stat="identity")+
        facet_wrap(~combined_group2) + 
        scale_y_continuous(labels=scales::percent) +
        theme(legend.title=element_blank()) +
        theme(strip.background = element_blank(), axis.title.x=element_blank(),
              axis.text.x = element_text(angle = 90), axis.title.y=element_blank())+
        labs(title = title, 
             caption = "Data: SOEP-Core v.36")+
        guides(fill=guide_legend(title=""))
    }
    
    plot <-  ggplotly(plot, tooltip = "text" )
  }
  return(plot)
}
