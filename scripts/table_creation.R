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
loadpackage(c("foreign", "dplyr", "tidyverse", "readstata13", "spatstat",
              "gsubfn", "rjson", "DescTools", "Hmisc"))  

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


################################################################################
# test center #
test_data <- get_data(datasetnum =  data.file.num, 
                      datasetfac = data.file.fac,
                      variable = "plh0218", 
                      year = "syear", 
                      weight = "phrf",
                      diffcount = 2,
                      diffvars = c("bula_h", "sex"),
                      vallabel = FALSE)

mean_data <- get_mean_values(dataset = test_data,
                             year = "year",
                             diffcount = 2,
                             diffvar1 = "bula_h",
                             diffvar2 = "sex",
                             diffvar3 = "")

test_data2 <- get_data(datasetnum =  data.file.num, 
                      datasetfac = data.file.fac,
                      variable = "plh0218", 
                      year = "syear", 
                      weight = "phrf",
                      diffcount = 2,
                      diffvars = c("bula_h", "sex"),
                      vallabel = TRUE)

prop_data <- get_prop_values(dataset = test_data2, 
                             groupvars = c("usedvariable", "year", "bula_h", "sex"), 
                             alpha = 0.05)

mean_data <-  get_protected_values(dataset = mean_data, cell.size = 30)

prop_data <-  create_table_lables(table = mean_data)

data.csv <- get_table_export(table = mean_data, variable = "plh0218", 
                             metadatapath = paste0(metapath, "variables.csv"),
                             exportpath = exportpath, diffcount = 2,
                             tabletype = "prop")
