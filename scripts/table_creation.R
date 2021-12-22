#############################################################################
#  Create aggregated tables for Master Project
#############################################################################

### What needs to be defined:
# Paths
if (Sys.info()[["user"]] == "stefz") {
  datapath <- "C:/datasets/platform_data/"
  metapath <- "C:/git/Master-Project/metadata/p_data/"
  exportpath <- "C:/git/Master-Project/tables/"
}

### What needs to be defined:
# Paths
if (Sys.info()[["user"]] == "Deliverance") {
  datapath <- ""
  metapath <- ""
  exportpath <- ""
}

# Definition of objects
dataset <- "p_data"  # datasetname
cell.min <- 30 # cell minimum
year <- "syear" # survey year must be defined
weight <- "phrf" # weightingfactor must be defined
#############################################################################

## load packages
loadpackage(c("tidyverse", "readstata13")) 

## load dataset
# data without labels
data.file.num <- read.dta13(paste0(datapath,dataset,".dta"), 
                            convert.factors = FALSE, encoding = "UTF-8")

# Weights with 0 cause problems
data.file.num <- data.file.num %>%
  filter(phrf > 0) 

# data with labels
data.file.fac <- read.dta13(paste0(datapath,dataset,".dta"), 
                            convert.factors = TRUE, 
                            nonint.factors = TRUE, encoding = "UTF-8")

# Weights with 0 cause problems
data.file.fac <- data.file.fac %>%
  filter(phrf > 0) 

# read metainformation
meta <- read.csv(paste0(metapath, "variables.csv") , header = TRUE,
                 colClasses = "character")
