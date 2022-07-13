All required files to run the shiny application are stored here:

metadata
This folder includes all metainformation about the used variables.

rsconnect
Establishes connection to shiny.io but is not necessary for running the app.

www
Includes all png and css files for the front end of the app.

graph_creation_FUN
All functions used to create graphic output from the aggregated tables are defined here and must be executed before starting the shiny app. 

app.R
Is the script that starts the app.

gadm36_DEU_1_sp.rds
Is an R dataset which includes the coordinates and metadata for the federal states of germany that are used for the heatmaps

