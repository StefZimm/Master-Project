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

meta_varcat <- read.csv(paste0(metapath, "variable_categories.csv") , header = TRUE,
                        colClasses = "character")

################################################################################
################################################################################
### Code to create the aggregated tables in variables.csv
meta_demo <- meta %>%
  filter(meantable == "demo") 

meta_demo <- subset(data.file.num,
                    select=meta_demo$variable)

# Generate a list that represents all the differentiation possibilities of the users 
difflist <- c("",combn(sort(names(meta_demo)),1,simplify=FALSE, FUN = sort), 
              combn(sort(names(meta_demo)),2,simplify=FALSE))

# Generate a list that represents ne number of differentiations for each possibility 
diffcountlist <- difflist
diffcountlist[[1]] <- 0
diffcountlist[2:(1+length(combn(names(meta_demo),1,simplify=FALSE)))] <- 1
diffcountlist[(2+length(combn(names(meta_demo),1,simplify=FALSE))):length(diffcountlist)] <- 2

##############################################################################################
# Create aggregated data tables
for (var in 1:length(meta$variable)){
  
  if (meta$meantable[var] == "Yes" | meta$probtable[var] == "Yes") {
    variable <- meta$variable[var] 
    
    for(i in seq_along(difflist)){
      diffcount <- diffcountlist[[i]]
      diffvars <- difflist[[i]]
      
      if (!is.na(diffvars[1])) {
        diffvar1 <- diffvars[1]
      } else {
        diffvar1 <- ""
      }
      
      if (!is.na(diffvars[2]))  {
        diffvar2 <- diffvars[2]
      } else {
        diffvar2 <- ""
      }
      
      if (!is.na(diffvars[3]))  {
        diffvar3 <- diffvars[3]
      } else {
        diffvar3 <- ""
      }
      
      if (meta$meantable[var] == "Yes") {
        data <- get_data(datasetnum =  data.file.num, 
                         datasetfac = data.file.fac,
                         variable = variable, 
                         year = year, 
                         weight = weight,
                         diffcount = diffcount,
                         diffvars = diffvars,
                         vallabel = FALSE)
        
        
        table.values <- get_mean_values(dataset = data, 
                                        year = "year", 
                                        diffcount = diffcount,
                                        diffvar1 = diffvar1,
                                        diffvar2 = diffvar2,
                                        diffvar3 = diffvar3)
        
        
        table.values <- create_table_lables(table = table.values)
        
        protected.table <- get_protected_values(dataset = table.values, cell.size = 30)
        
        
        protected.table <- expand_table(table = protected.table, diffvar1 = diffvar1, 
                                        diffvar2 = diffvar2, diffvar3 = diffvar3,
                                        diffcount = diffcount, tabletype = "mean")
        
        data.csv <- get_table_export(table = protected.table, variable = variable, 
                                     metadatapath = paste0(metapath, "variables.csv"),
                                     exportpath = exportpath, diffcount = diffcount, 
                                     tabletype = "mean")
        
        json_create_lite(variable = variable, 
                         varlabel = meta$label_de[meta$variable==variable],
                         startyear = as.numeric(unique(data.csv$year)[1]), 
                         endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]), 
                         tabletype = "mean",
                         exportpath = paste0(exportpath, "/numerical/", variable, "/meta.json"))
        
        print(paste("The variable", variable, "is processed with grouping year,", 
                    paste(difflist[[i]],collapse=","), "as numeric mean table"))
      }
      
      if (meta$probtable[var] == "Yes") {
        data <- get_data(datasetnum =  data.file.num, 
                         datasetfac = data.file.fac,
                         variable = variable, 
                         year = year, 
                         weight = weight,
                         diffcount = diffcount,
                         diffvars = diffvars,
                         vallabel = TRUE)
        
        if (diffvars=="") {
          columns <- c("usedvariable", "year")
        } else {
          columns <- c("usedvariable", "year", diffvars)
        }
        
        prop.data <- get_prop_values(dataset = data, groupvars = columns, alpha = 0.05)
        
        protected.table <- get_protected_values(dataset = prop.data, cell.size = 30)
        
        protected.table <- expand_table(table = protected.table, diffvar1 = diffvar1, 
                                        diffvar2 = diffvar2, diffvar3 = diffvar3,
                                        diffcount = diffcount, tabletype = "prop")
        
        # var_cat <- subset(meta_varcat, variable==meta$variable[var] , select = c("value", "label_de"))
        # protected.table <- merge(protected.table, var_cat, by.x = "usedvariable", by.y = "label_de")
        # protected.table$value <- as.numeric(protected.table$value)
        
        # protected.table <- protected.table[with(protected.table, order(year)), ]
        
        data.csv <- get_table_export(table = protected.table, variable = variable, 
                                     metadatapath = paste0(metapath, "variables.csv"),
                                     exportpath = exportpath, diffcount = diffcount,
                                     tabletype = "prop")
        
        json_create_lite(variable = variable, 
                         varlabel = meta$label_de[meta$variable==variable],
                         startyear = as.numeric(unique(data.csv$year)[1]), 
                         endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]), 
                         tabletype = "prop",
                         exportpath = paste0(exportpath, "/categorical/", variable, "/meta.json"))
        
        print(paste("The variable", variable, "is processed with grouping year", 
                    paste(difflist[[i]],collapse=","), "as a categorical percentage table"))
      }
    } 
  }
}  

###############################################################################
# test center #
# numerical variables: pglabnet pgtatzeit bmi ple0007
# categorical variables: plh0182 plj0587

# for numeric variables
# mean_data <- get_data(datasetnum =  data.file.num,
#                       datasetfac = data.file.fac,
#                       variable = "plh0171",
#                       year = "syear",
#                       weight = "phrf",
#                       diffcount = 1,
#                       diffvars = c("migback"),
#                       vallabel = FALSE)
# 
# mean_data <- get_mean_values(dataset = mean_data,
#                              year = "year",
#                              diffcount = 1,
#                              diffvar1 = "migback",
#                              diffvar2 = "",
#                              diffvar3 = "")
# 
# mean_data <-  get_protected_values(dataset = mean_data, cell.size = 30)
# 
# mean_data <- expand_table(table = mean_data, diffvar1 = "migback",
#                           diffvar2 = "", diffvar3 = "",
#                           diffcount = 1, tabletype = "mean")
# 
# mean_data <-  create_table_lables(table = mean_data)
# 
# data.csv <- get_table_export(table = mean_data, variable = "mean_data",
#                              metadatapath = paste0(metapath, "variables.csv"),
#                              exportpath = exportpath, diffcount = 2,
#                              tabletype = "mean")
# 
# json_create_lite(variable = "ple0007",
#                  varlabel = meta$label_de[meta$variable=="ple0007"],
#                  startyear = as.numeric(unique(data.csv$year)[1]),
#                  endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]),
#                  tabletype = "mean",
#                  exportpath = paste0(exportpath, "/numerical/", "ple0007", "/meta.json"))
# 
# 
# ################################################################################
# # for categorical variables
# prop_data <- get_data(datasetnum =  data.file.num,
#                       datasetfac = data.file.fac,
#                       variable = "erwst",
#                       year = "syear",
#                       weight = "phrf",
#                       diffcount = 0,
#                       diffvars = "",
#                       vallabel = TRUE)
# 
# prop_data <- get_prop_values(dataset = prop_data,
#                              groupvars = c("usedvariable", "year"),
#                              alpha = 0.05)
# 
# prop_data <-  get_protected_values(dataset = prop_data, cell.size = 30)
# 
# 
# prop_data <- expand_table(table = prop_data, diffvar1 ="",
#                           diffvar2 = "", diffcount = 0, tabletype = "prop")
# 
# var_cat <- subset(meta_varcat, variable=="erwst" , select = c("value", "label_de"))
# prop_data <- merge(prop_data, var_cat, by.x = "usedvariable", by.y = "label_de")
# prop_data$value <- as.numeric(prop_data$value)
# 
# prop_data <- prop_data[with(prop_data, order(year, value)), ]
# 
# 
# data.csv <- get_table_export(table = prop_data, variable = "erwst",
#                              metadatapath = paste0(metapath, "variables.csv"),
#                              exportpath = exportpath, diffcount = 0,
#                              tabletype = "prop")
# 
# json_create_lite(variable = "hgowner",
#                  varlabel = meta$label_de[meta$variable=="hgowner"],
#                  startyear = as.numeric(unique(data.csv$year)[1]),
#                  endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]),
#                  tabletype = "prop",
#                  exportpath = paste0(exportpath, "/categorical/", "hgowner", "/meta.json"))

#####################################################################################################
