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


get_map_plot <- function(table, syear, variable, statistic, diffvar){
  
  
  title <- paste0(meta$label_de[meta$variable==variable], " in federal states")
  
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

get_lineplot <- function(table, meta, variable, diffvar1, diffvar2, diffcount, 
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

get_percent_lineplot <- function(table, meta, variable, diffvar1, diffvar2, diffcount, 
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
# testcenter
# 
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
# metapath <- "C:/git/Master-Project/metadata/p_data/variables.csv"
# tables <- "C:/git/Master-Project/tables/"
# tabletype <- "numerical"
# variable <- "pgtatzeit"
# 
# table <- "pgtatzeit_year.csv"
# table <- "pgtatzeit_year_sampreg.csv"
# table <- "pgtatzeit_year_sampreg_sex.csv"
# 
# 
# data <- read.csv(file = paste0(tables, tabletype, "/", variable, "/", table),
#                  encoding = "UTF-8")
# 
# meta <- read.csv(file = metapath, encoding = "UTF-8")
# 
# get_lineplot(table = data, 
#              meta = meta, 
#              variable = variable, 
#              diffvar1 = "sampreg",
#              diffvar2 = "sex",
#              diffcount = 3,
#              start = 1992,
#              end = 2000,
#              ci = FALSE)   
# 
# 
# get_boxplot(table = data, variable = variable, diffcount = 3,
#             diffvar2 = "sampreg", diffvar3 = "sex")
# 
# metapath <- "C:/git/Master-Project/metadata/p_data/variables.csv"
# tables <- "C:/git/Master-Project/tables/"
# tabletype <- "numerical"
# variable <- "pgtatzeit"
# 
# table <- "pgtatzeit_year_bula_h.csv"
# table <- "pgtatzeit_year_bula_h_sex.csv"
# table <- "pgtatzeit_year_age_gr_bula_h.csv"
# 
# data <- read.csv(file = paste0(tables, tabletype, "/", variable, "/", table),
#                  encoding = "UTF-8")
# 
# get_map_plot(table = data,
#              syear = "2017",
#              variable = variable,
#              statistic = "mean",
#              diffvar = "age_gr")
#
# get_percent_lineplot(table = data, 
#                      meta = meta, 
#                      variable = variable, 
#                      diffvar1 = "sampreg", 
#                      diffvar2 = "sex", 
#                      diffcount = 3, 
#                      start = 2005, 
#                      end = 2015, 
#                      ci = FALSE)

