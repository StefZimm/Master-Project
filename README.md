**Master Project: Development of a data visualization platform for non-scientists with SOEP Data**

In order to make the SOEP Data inventory of social science data and research results as widely accessible as possible 
and to establish a new culture of using scientific results in other areas of society as well, our master project aims to create 
a platform that allows non-scientists such as journalists or historians to work indirectly with SOEP data without needing direct data access.

The Master Project folder contains the code for a Shiny App, which is intended to provide small descriptive 
analyses of the [SOEP (Socio-economic Panel)](https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html).

**Organization of the Master Project:** 

The repository of the Master Project is divided into the 3 main folders input and scripts. 

### Master-Project Timeline

**Concept Phase**

**Deliverable 1: 3 February 2022**
- [x] Create [Proposal ](https://github.com/StefZimm/Master-Project/blob/main/report/MDM%20Master%20Project%20Proposal_Bougie_Zimmerman.pdf)for Master Project (https://github.com/StefZimm/Master-Project/issues/3)
- [x] Create [gitrepo ](https://github.com/StefZimm/Master-Project)and [Issuetracker ](https://github.com/StefZimm/Master-Project/issues)structure for collaborative work
- [x] Create and submit timeline for master project (https://github.com/StefZimm/Master-Project/issues/7)
- [x] Obtain access to the necessary SOEP survey data
- [x] Define first draft of [datasets ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/p_data/variables.csv) and [variables ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/Var_Info.xlsx)that will be used for the project (https://github.com/StefZimm/Master-Project/issues/1)
- [x] Create [Script ](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do)to create first draft of usable individual dataset for the project: Load, merge, data wrangling with selected variables
- [ ] Define statistical indicators which should be part of the aggregated tables (https://github.com/StefZimm/Master-Project/issues/10)
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
- [ ] User analysis (Who would need platform?) (https://github.com/StefZimm/Master-Project/issues/11)
      - What [topics ](https://paneldata.org/soep-core/topics/en)should be prioritized
- [ ] Final Variable selection concept (variables to analyse and variables for grouping) (https://github.com/StefZimm/Master-Project/issues/1)
      - variables need to be added in [Prep.do](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do) and in [Metadata ](https://github.com/DeliveranceBougie/Master-Project/blob/main/metadata/)
- [ ] Define amount of datasets (individual level and household level) we need to create (https://github.com/StefZimm/Master-Project/issues/1)
- [ ] Data Quality Checks for [Data Preparation](https://github.com/DeliveranceBougie/Master-Project/blob/main/scripts/Prepare_Data.do) https://github.com/StefZimm/Master-Project/issues/12
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
- [ ] https://github.com/StefZimm/Master-Project/issues/15
      - functions create always same results
      - mean, median, percentages always in the confidence interval range
      - define minimum cell grouping (n=30)
- [ ] https://github.com/StefZimm/Master-Project/issues/4
       - heatmaps on federal state level for numerical variables
       - stackedbarplot for categorical variables
       - line plot for categorical variables and numerical variables
       - boxplots for numerical variables
       - (regression) or corelation plots
- [ ] Create [graphical functions](https://github.com/StefZimm/Master-Project/blob/main/scripts/graph_creation_FUN.R) that use the [aggregated datatables ](https://github.com/StefZimm/Master-Project/tree/main/tables) to create the different interactive graphictypes with selectable grouping options (https://github.com/StefZimm/Master-Project/issues/16)
- [ ] https://github.com/StefZimm/Master-Project/issues/17


##################################################################################################

**Shiny Interface Phase**

**Deliverable 4: 28 April 2022**
- [ ]  https://github.com/StefZimm/Master-Project/issues/18
       - colorblindness
       - same colorstyle for all graphics
       - understandable and standardized
- [ ] https://github.com/StefZimm/Master-Project/issues/19
      - define selectable buttons (inputs for user)
      - user selection und functions should create interactive graphic on shiny App
- [ ] https://github.com/StefZimm/Master-Project/issues/20
      - create layout
      - create topicbased navigation
      - create help texts
- [ ] (https://github.com/StefZimm/Master-Project/issues/21)
      - rmarkdown for reports
      - download of csv tables
 
##################################################################################################

**Testing and Wrtiting Project Report Phase** 

**Deliverable 5: 2 June 2022**
- [ ] https://github.com/StefZimm/Master-Project/issues/22
      - efficiency check. How long does it take to load a graphic?
- [ ] https://github.com/StefZimm/Master-Project/issues/23
     - usable URL for users
     - data protection regulations to put aggregated data on server?
- [ ] https://github.com/StefZimm/Master-Project/issues/24
- [ ] https://github.com/StefZimm/Master-Project/issues/25
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


