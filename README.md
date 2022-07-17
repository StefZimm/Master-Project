**Title: Master Project Development of a data visualization platform for non-scientists with SOEP Data**

**Authors: Deliverance Bougie, Stefan Zimmermann**

**[Raw Data](https://www.diw.de/en/diw_01.c.601584.en/data_access.html) from [10.5684/soep.core.v36eu](https://www.diw.de/en/diw_01.c.814095.en/edition/soep-core_v36eu__data_1984-2019__eu_edition.html)**

In order to make the SOEP Data inventory of social science data and research results as widely accessible as possible 
and to establish a new culture of using scientific results in other areas of society as well, our master project aims to create 
a platform that allows non-scientists such as journalists or historians to work indirectly with SOEP data without needing direct data access.

The Master Project folder contains the code for a Shiny App, which is intended to provide small descriptive 
analyses of the [SOEP (Socio-economic Panel)](https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html).

**Organization of the Master Project:** 

GitHub was used as the working environment and collaboration platform for the project. The timeline and project structure were created as an accessible GitHub repository. Metadata, data preparation scripts, shiny scripts and programmed R functions were uploaded to this GitHub repository. This is where changes are documented, versions can be secured, and an issue tracker structure can be established that allows collaborative work on shared projects in a secure area. The various work packages could be created as individual issues in GitHub, which could be used to discuss and work on specific technical or conceptual problems. All issues could be assigned to a responsible person. Direct solutions to the problems (file changes) can be linked to the issue, so that work processes can be documented but also undone. All issues were also tagged with identification labels (work phases) and milestones to quickly find and prioritize work phases in specific months. Completed single issues or work phases could then be closed after completion of the work and are then documented in their entirety. If issues are identified again at a later point in time, the issue and thus the work packages can also be reopened. 
In addition to the valuable work organization, the documentation aspect, and the version control with the associated restoration of old project states, GitHub was an excellent way to advance the development of the dashboard in a structured manner. Since the project was heavily influenced by programming and the simultaneous work on specific scripts, the project team was able to quickly identify which changes were incorporated into scripts, since differences can be quickly and easily identified via the commit history. After the GitHub repository was created, the project team established the following folder structure:

- metadata
- report
- scripts
- shiny_app
- tables
- readme.md
- gitignore

The project goals and the folder structure is explained in the readme document. Documentation should be stored in tabular form (csv) in the metadata folder. The report folder should contain the in progress reports as well as the final report of the dashboard. Scripts for data management, table aggregation or data visualization are stored in the scripts folder. The shiny_app folder contains all information relevant for shiny to display the dashboard. Inside the shiny_app folder are two scripts global.R and app.R that make the dashboard UI appear functional. Image files for embedded logos, icons or hint graphics can also be found in the shiny_app folder. Standardized federal states coordinates and maps of Germany, which render graphic types like the heatmap as efficiently as possible, are loaded from this folder. The table folder contains all aggregated data tables in csv format of the analysis variables with all possible grouping options. Inside the table folder, the tables are stored in variable type folders (categorical, numerical) and SOEP variable name folders. With gitignore, file types can be defined that are not allowed to be uploaded to the git servers. For example ".dta" could be defined there. By using gitignore, the project team was able to ensure that no raw micro-data ended up on the GitHub servers. The project team's data security goal was to develop a dashboard that could run without the direct use of microdata. Therefore, raw data does not need to and should not be stored on external servers for the success of the dashboard. Preparing the microdata can be done in a local secured environment. When uploading changes to the GitHub repository, microdata is not uploaded as the gitignore file ignores this data when uploading. This ensures that the microdata remains in a secure area and the access of unauthorized persons can be prevented. The data would have to be located in a secured area and is only accessible to the project team. To get the app running on a server in the future, the repository including the shiny_app folder would have to be uploaded to a server and the global.R and app.R file would have to be executed there. 


### Master-Project Timeline

**Concept Phase**

**Deliverable 1: 3 February 2022**
- [x] Create [Proposal ](https://github.com/StefZimm/Master-Project/blob/main/report/MDM%20Master%20Project%20Proposal_Bougie_Zimmerman.pdf)for Master Project (https://github.com/StefZimm/Master-Project/issues/3)
- [x] Create [gitrepo ](https://github.com/StefZimm/Master-Project)and [Issuetracker ](https://github.com/StefZimm/Master-Project/issues)structure for collaborative work
- [x] Create and submit timeline for master project (https://github.com/StefZimm/Master-Project/issues/7)
- [x] Obtain access to the necessary SOEP survey data
- [x] Define first draft of [datasets ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/p_data/variables.csv) and [variables ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/Var_Info.xlsx)that will be used for the project (https://github.com/StefZimm/Master-Project/issues/1)
- [x] Create [Script ](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do)to create first draft of usable individual dataset for the project: Load, merge, data wrangling with selected variables
- [x] Define statistical indicators which should be part of the aggregated tables (https://github.com/StefZimm/Master-Project/issues/10)
      - mean, median, n (for line plot, boxplot and heatmap)
      - percentages (for line plot and stacked bar plot)
      - upper_meanci, lower_meanci (for line plot and heatmap) 
      - upper_medianci, lower_medianci (https://github.com/StefZimm/Master-Project/issues/2) (for line plot and heatmap)
      - upper_percentci, lower_percentci (for line plot and stacked bar plot)
      - percentiles10, percentiles 25, percentiles75, percentiles90, percentiles99 (for boxplot)
      - min, max (boxplot)
      - (corelation coefficients)
      - (regression coefficients)

##################################################################################################

**Table Creation Phase**

**Deliverable 2: 3 March 2022**
- [x] User analysis (Who would need platform?) (https://github.com/StefZimm/Master-Project/issues/11)
      - What [topics ](https://paneldata.org/soep-core/topics/en)should be prioritized
- [x] Final Variable selection concept (variables to analyse and variables for grouping) (https://github.com/StefZimm/Master-Project/issues/1)
      - variables need to be added in [Prep.do](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do) and in [Metadata ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/)
- [x] Define amount of datasets (individual level and household level) we need to create (https://github.com/StefZimm/Master-Project/issues/1)
- [x] Data Quality Checks for [Data Preparation](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do) https://github.com/StefZimm/Master-Project/issues/12
       - Check quality of dataset merges
       - Identify and resolve missing data
       - Review summary statistics and visualizations for potential data quality issues
       - More data wrangling if necessary
       - Define variablenames, labels and valuelabels as [Metadata ](https://github.com/StefZimm/Master-Project/tree/main/metadata/p_data)
- [x] [Create aggregated table functions](https://github.com/StefZimm/Master-Project/blob/main/scripts/table_creation_FUN.R) to transform input dataset to aggregated csv tables (https://github.com/StefZimm/Master-Project/issues/13)
- [x] [Create ](https://github.com/StefZimm/Master-Project/blob/main/scripts/table_creation.R) first [aggregated datatables ](https://github.com/StefZimm/Master-Project/tree/main/tables) (https://github.com/StefZimm/Master-Project/issues/14)


##################################################################################################

**Interactive Graphic Creation Phase**

**Deliverable 3: 31 March 2022**
- [x] https://github.com/StefZimm/Master-Project/issues/15
      - functions create always same results
      - mean, median, percentages always in the confidence interval range
      - define minimum cell grouping (n=30)
- [x] https://github.com/StefZimm/Master-Project/issues/4
       - heatmaps on federal state level for numerical variables
       - stackedbarplot for categorical variables
       - line plot for categorical variables and numerical variables
       - boxplots for numerical variables
       - (regression) or corelation plots
- [x] Create [graphical functions](https://github.com/StefZimm/Master-Project/blob/main/scripts/graph_creation_FUN.R) that use the [aggregated datatables ](https://github.com/StefZimm/Master-Project/tree/main/tables) to create the different interactive graphictypes with selectable grouping options (https://github.com/StefZimm/Master-Project/issues/16)
- [x] https://github.com/StefZimm/Master-Project/issues/17


##################################################################################################

**Shiny Interface Phase**

**Deliverable 4: 28 April 2022**
- [ ]  https://github.com/StefZimm/Master-Project/issues/18
       - colorblindness
       - same colorstyle for all graphics
       - understandable and standardized
- [x] https://github.com/StefZimm/Master-Project/issues/19
      - define selectable buttons (inputs for user)
      - user selection und functions should create interactive graphic on shiny App
- [x] https://github.com/StefZimm/Master-Project/issues/20
      - create layout
      - create topicbased navigation
      - create help texts
- [ ] (https://github.com/StefZimm/Master-Project/issues/21)
      - rmarkdown for reports
      - download of csv tables
 
##################################################################################################

**Testing and Wrtiting Project Report Phase** 

**Deliverable 5: 2 June 2022**
- [x] https://github.com/StefZimm/Master-Project/issues/22
      - efficiency check. How long does it take to load a graphic?
- [x] https://github.com/StefZimm/Master-Project/issues/23
     - usable URL for users
     - data protection regulations to put aggregated data on server?
- [x] https://github.com/StefZimm/Master-Project/issues/24
- [x] https://github.com/StefZimm/Master-Project/issues/25
      - Motivation
      - Description of the data source
      - Content of the dashboard
      - Covered Topics
      - Covered Indicators
      - Functionality
      - Description of the technical workflow
      - Preparation and selection of raw micro data
      - Processing of raw data into aggregated tables and graphs
      - Development of the interactive shiny interface
      - Data protection policy
      - Problems and challenges
      - Summary

##################################################################################################

```
sessionInfo()
R version 4.1.1 (2021-08-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.1252    

attached base packages:
[1] tools     stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forcats_0.5.1       purrr_0.3.4         readr_2.0.2         tidyr_1.1.4         tibble_3.1.4        tidyverse_1.3.1    
 [7] randomcoloR_1.1.0.1 wesanderson_0.3.6   plotly_4.10.0       stringr_1.4.0       dplyr_1.0.7         rgeos_0.5-8        
[13] sf_1.0-3            raster_3.5-11       sp_1.4-5            checkpoint_1.0.2    ggplot2_3.3.5      

loaded via a namespace (and not attached):
 [1] Rtsne_0.15         colorspace_2.0-2   ggsignif_0.6.3     ellipsis_0.3.2     class_7.3-19       rio_0.5.27         rsconnect_0.8.25  
 [8] fs_1.5.0           rstudioapi_0.13    proxy_0.4-26       farver_2.1.0       ggpubr_0.4.0       fansi_0.5.0        lubridate_1.7.10  
[15] xml2_1.3.2         codetools_0.2-18   cachem_1.0.6       jsonlite_1.7.2     broom_0.7.9        cluster_2.1.2      dbplyr_2.1.1      
[22] shiny_1.7.1        compiler_4.1.1     httr_1.4.2         backports_1.2.1    assertthat_0.2.1   fastmap_1.1.0      lazyeval_0.2.2    
[29] cli_3.0.1          s2_1.0.7           later_1.3.0        htmltools_0.5.2    gtable_0.3.0       glue_1.4.2         wk_0.5.0          
[36] V8_4.1.0           Rcpp_1.0.7         carData_3.0-4      cellranger_1.1.0   jquerylib_0.1.4    vctrs_0.3.8        crosstalk_1.1.1   
[43] openxlsx_4.2.4     rvest_1.0.1        mime_0.12          lifecycle_1.0.1    rstatix_0.7.0      terra_1.4-22       scales_1.1.1      
[50] hms_1.1.1          promises_1.2.0.1   yaml_2.2.1         curl_4.3.2         sass_0.4.0         stringi_1.7.4      e1071_1.7-9       
[57] zip_2.2.0          rlang_0.4.11       pkgconfig_2.0.3    lattice_0.20-44    fontawesome_0.2.2  labeling_0.4.2     htmlwidgets_1.5.4 
[64] tidyselect_1.1.1   magrittr_2.0.1     R6_2.5.1           generics_0.1.0     DBI_1.1.1          pillar_1.7.0       haven_2.4.3       
[71] foreign_0.8-81     withr_2.5.0        units_0.7-2        abind_1.4-5        modelr_0.1.8       crayon_1.5.1       car_3.0-11        
[78] KernSmooth_2.23-20 utf8_1.2.2         tzdb_0.1.2         grid_4.1.1         readxl_1.3.1       data.table_1.14.2  reprex_2.0.1      
[85] digest_0.6.28      classInt_0.4-3     xtable_1.8-4       httpuv_1.6.3       openssl_2.0.0      munsell_0.5.0      viridisLite_0.4.0 
[92] bslib_0.3.1        askpass_1.1  
```
