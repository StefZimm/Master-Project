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

