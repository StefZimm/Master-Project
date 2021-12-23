################################################################################
# Functions #
################################################################################

#' @title loadpackage loads required R-packages
#'
#' @description loadpackage should install needed packages if necessary and load them into library
#'
#' @param x Names of the required R-packages as vector (e.g. c("foreign", "dplyr", "tidyverse", "readstata13"))
#' 
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords loadpackage
#'  
#' @examples
#'       loadpackage(c("foreign", "dplyr", "tidyverse", "readstata13")) 

# install and load packages
loadpackage <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
    }
    #  Load package (after installing)
    library( i , character.only = TRUE )
  }
}


#' @title get_data creates subset of a data set
#'
#' @description get_data to get the output dataset in long format 
#' limited to certain variables (variable, year, weight, diffvars) and
#' contain only valid values.
#'
#' @param datasetnum data.frame with numeric variables (e.g. platform_data)
#' @param datasetfac data.frame with factor variables (e.g. platform_data)
#' @param variable name analysis variable as string (e.g. "pglabnet" )
#' @param year Name survey year variable as string (e.g. "syear" )
#' @param weight Name weight variable as string (e.g. "phrf")
#' @param diffcount Number of desired differentiations (e.g. 2) (range 0-3)
#' @param diffvars Vector with differentiation variables (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' @param vallabel Valuelabel should be used (e.g.: vallabel = TRUE) (TRUE/FALSE)
#' 
#' @return variable.values.valid is a data set with valid values of the 
#' variables (variable, year, weight, diffvars)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_data
#'  
#' @examples
#'       get_data(datasetnum =  data.file.num, 
#'       datasetfac = data.file.fac,
#'       variable = "pglabnet", 
#'       year = "syear", 
#'       weight = "phrf",
#'       diffcount = diffcount,
#'       diffvars = c("alter_gr", "sex", "Bildungsniveau"),
#'       vallabel = TRUE)

get_data <- function(datasetnum, datasetfac, variable, year, weight, diffcount, diffvars, vallabel){
  
  if (diffcount > 0) {
    
    if (vallabel == FALSE) {
      variable.values <- subset(datasetnum,
                                select=c(variable, year, weight, diffvars)) 
      names(variable.values) <- c("usedvariablenum", "year", "weight", diffvars)
    }
    if (vallabel == TRUE) {
      variable.values <- subset(datasetnum, select=variable) 
      factorvar <- subset(datasetfac, select=c(variable, year, weight, diffvars))
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <- c("usedvariablenum", "usedvariable", "year", "weight", diffvars)
    }
  }
  
  if (diffcount == 0) {
    if (vallabel == FALSE) {
      variable.values <- subset(datasetnum,
                                select=c(variable, year, weight)) 
      names(variable.values) <- c("usedvariablenum", "year", "weight")
    }
    if (vallabel == TRUE) {
      variable.values <- subset(datasetnum, select=variable) 
      factorvar <- subset(datasetfac, select=c(variable, year, weight))
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <- c("usedvariablenum", "usedvariable", "year", "weight")
    }
  }
  
  if (any(variable.values$usedvariablenum >= 0)) { 
    if (vallabel == FALSE) {
      variable.values.valid <- subset(variable.values, usedvariablenum>= 0) 
      variable.values.valid <- variable.values.valid[order(variable.values.valid$usedvariablenum),]
      names(variable.values.valid)[names(variable.values.valid) == "usedvariablenum"] <- "usedvariable"
    } 
    if (vallabel == TRUE) {
      variable.values.valid <- subset(variable.values, usedvariablenum>= 0) 
      variable.values.valid <- variable.values.valid[order(variable.values.valid$usedvariablenum),]
      variable.values.valid <- variable.values.valid[2:length(variable.values.valid)]
    }
  }
  
  return(variable.values.valid)
}

################################################################################

#' @title get_mean_values creates mean/median tables with mean, median, n, percentiles, confidence interval
#' 
#' @description get_mean_values creates weighted mean/median tables with
#'              n, percentiles, confidence interval
#'
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param year year variable as string (e.g. "year")
#' @param diffcount number of desired differentiations (e.g. 2) (range 0-3)
#' @param diffvar1 name differentiation variable 1 as string ("" possible)
#' @param diffvar2 name differentiation variable 2 as string ("" possible)
#' @param diffvar3 Number of differentiation variable 3 as string ("" possible)
#' 
#' @return data = dataset with mean, median, n, percentiles, confidence interval
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_mean_values
#'  
#' @examples
#'       get_mean_values(dataset = data, 
#'                       year = "year", 
#'                       diffcount = 2,
#'                       diffvar1 = "sex",
#'                       diffvar2 = "alter_gr",
#'                       diffvar3 = "")

get_mean_values <- function(dataset, year, diffcount,
                            diffvar1, diffvar2, diffvar3) {
  
  if (diffcount == 0) {
    columns = year
  }
  
  if (diffcount == 1) {
    columns = c(year, diffvar1)
  }
  
  if (diffcount == 2) {
    columns = c(year, diffvar1, diffvar2)
  }
  
  if (diffcount == 3) {
    columns = c(year, diffvar1, diffvar2, diffvar3)
  }
  mean.values <- dataset[complete.cases(dataset), ] %>%
    group_by_at(vars(one_of(columns))) %>%
    mutate(mean = round(weighted.mean(usedvariable, weight),2))  %>%
    mutate(median = round(weighted.median(usedvariable, weight),2))  %>%
    add_count(year, wt = NULL)  %>%
    mutate(sd = sd(usedvariable/sqrt(n))) %>%
    mutate(lower = mean - qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd,
           upper = mean + qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd) %>%
    mutate(lowerci_mean = round((lower),2))  %>%
    mutate(upperci_mean = round((upper),2))  %>%
    distinct(mean, .keep_all = TRUE)
  
  percentile.values <-  dataset[complete.cases(dataset), ] %>%
    group_by_at(vars(one_of(columns)))  %>% 
    summarise(ptile10 = wtd.quantile(usedvariable, weights = weight, 
                                     probs = .1, na.rm = TRUE),
              ptile25 = wtd.quantile(usedvariable, weights = weight, 
                                     probs = .25, na.rm = TRUE),
              ptile75 = wtd.quantile(usedvariable, weights = weight, 
                                     probs = .75, na.rm = TRUE),
              ptile90 = wtd.quantile(usedvariable, weights = weight, 
                                     probs = .90, na.rm = TRUE), .groups = 'drop')
  
  medianci.value<- dataset[complete.cases(dataset), ] %>% 
    nest(data = -columns) %>%
    mutate(ci = map(data, ~ MedianCI(.x$usedvariable, method = "exact")[2:3])) %>% 
    unnest_wider(ci)
  
  medianci.value$data <- NULL
  colnames(medianci.value) <- c(columns, "lowerci_median", "upperci_median")
  
  data <- merge(mean.values,percentile.values,by=columns)
  data <- merge(data,medianci.value,by=columns)
  
  selected.values <- c(columns, "mean", "lowerci_mean", "upperci_mean", 
                       "median", "lowerci_median", "upperci_median", 
                       "ptile10", "ptile25", "ptile75", "ptile90", "n")
  
  data <- data[,(names(data) %in% selected.values)]
  data <-  data %>% 
    arrange(desc(year), .by_group = TRUE)
  
  return(data)
}

################################################################################
#' @title function get_prop_values shall create weighted proportion tables with confidence intervals
#'
#' @description get_mean_values shall create weighted proportion value tables with confidence intervals
#' create with the information n = size of the subgroup, percent = weighted
#' proportion value, lower_confidence = lower confidence interval, upper_confidence = upper 95% confidence interval
#' 
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param groupvars vector with all variables in the dataset (e.g. c("usedvariable", "year", "sex"))
#' @param alpha Alpha for setting the confidence interval (e.g. 0.05)
#' 
#' @return data_prop_complete_ci = data set with n, percent, lower_confidence, upper_confidence
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'  
#' @examples
#'       get_prop_values(dataset = data, 
#'                       groupvars = c("usedvariable", "year", "sex"), 
#'                       alpha = 0.05)

get_prop_values <- function(dataset, groupvars, alpha) {
  
  data_prop1 <-  dataset[complete.cases(dataset), ] %>%
    group_by_at(vars(one_of(groupvars))) %>%
    summarise(count_w = sum(weight), .groups="drop_last")
  
  data_prop2 <-  data_prop1[complete.cases(data_prop1), ] %>% 
    group_by(eval(parse(text = groupvars[2])), eval(parse(text = groupvars[3])),
             eval(parse(text = groupvars[4]))) %>%
    mutate(sum_count_w = sum(count_w)) 
  
  data_prop3 <- dataset[complete.cases(dataset), ] %>% 
    group_by_at(vars(one_of(groupvars))) %>%
    summarise(n = n(), .groups="drop_last") 
  
  data_prop4 <- data_prop3[complete.cases(data_prop3), ] %>% 
    group_by(eval(parse(text = groupvars[2])), eval(parse(text = groupvars[3])),
             eval(parse(text = groupvars[4]))) %>%
    mutate(n_total = sum(n)) 
  
  data_prop <- cbind(data_prop1, data_prop2["sum_count_w"], data_prop3["n"], data_prop4["n_total"])
  data_prop <- data_prop[order(data_prop$year),]
  
  data_prop_complete <-  data_prop[complete.cases(data_prop1), ] %>% 
    mutate(percent = count_w/sum_count_w,)
  
  n_total <- data_prop_complete$n_total
  p_hat <- data_prop_complete$percent
  alpha <- alpha
  
  margin1 <- qnorm(1-alpha/2)*sqrt(p_hat*(1-p_hat)/n_total)
  
  # Compute the CI
  lower_confidence1 <- p_hat - margin1
  upper_confidence1 <- p_hat + margin1
  
  data_prop_complete_ci <- cbind(data_prop_complete, 
                                 lower_confidence=lower_confidence1, 
                                 upper_confidence=upper_confidence1)
  
  data_prop_complete_ci <- subset(data_prop_complete_ci, select=c(groupvars, "n", "percent", 
                                                                  "lower_confidence", "upper_confidence")) 
  
  return(data_prop_complete_ci)
}
