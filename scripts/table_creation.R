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
mean_data <- get_data(datasetnum =  data.file.num, 
                      datasetfac = data.file.fac,
                      variable = "plh0218", 
                      year = "syear", 
                      weight = "phrf",
                      diffcount = 3,
                      diffvars = c("migback", "sampreg", "sex"),
                      vallabel = FALSE)

mean_data <- get_mean_values(dataset = mean_data,
                             year = "year",
                             diffcount = 3,
                             diffvar1 = "migback",
                             diffvar2 = "sampreg",
                             diffvar3 = "sex")

prop_data <- get_data(datasetnum =  data.file.num, 
                      datasetfac = data.file.fac,
                      variable = "plh0218", 
                      year = "syear", 
                      weight = "phrf",
                      diffcount = 3,
                      diffvars = c("migback", "sampreg", "sex"),
                      vallabel = TRUE)

prop_data <- get_prop_values(dataset = prop_data, 
                             groupvars = c("usedvariable", "year", "migback", "sampreg", "sex"), 
                             alpha = 0.05)

mean_data <-  get_protected_values(dataset = mean_data, cell.size = 30)

prop_data <-  get_protected_values(dataset = prop_data, cell.size = 30)

mean_data <- expand_table(table = mean_data, diffvar1 = "migback", 
                          diffvar2 = "sampreg", diffvar3 = "sex",
                          diffcount = 3, tabletype = "mean")

prop_data <- expand_table(table = prop_data, diffvar1 = "migback", 
                          diffvar2 = "sampreg", diffvar3 = "sex",
                          diffcount = 3, tabletype = "prop")


mean_data <-  create_table_lables(table = mean_data)

data.csv <- get_table_export(table = mean_data, variable = "plh0218", 
                             metadatapath = paste0(metapath, "variables.csv"),
                             exportpath = exportpath, diffcount = 3,
                             tabletype = "mean")

data.csv <- get_table_export(table = prop_data, variable = "plh0218", 
                             metadatapath = paste0(metapath, "variables.csv"),
                             exportpath = exportpath, diffcount = 3,
                             tabletype = "prop")
