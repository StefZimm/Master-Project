All required scripts are stored here:

Prepare_Data.do
In the script, the raw SOEP data are combined into a single data set and the population is defined. Furthermore, some recoding and generation of additional variables is done. 

table_creation_FUN
All functions used to create aggregated data tables are defined here and must be executed before creating the aggregated data tables (table_creation)

table_creation
Creates for all defined variables in the variables.csv (metadata) an aggregated data table and a json documentation file. 
Moreover the prepared data file from the Prepare_Data.do must be generated.

graph_creation_FUN
All functions used to create graphic output from the aggregated tables are defined here and must be executed before starting the shiny app. 

