
##############################################################
# just commenting this out for now so I know where it goes if the global.R file doesn't work as I would like it to. 
# this can be uncommented if we do not use the global.R file
##############################################################

# library(shiny)
# library(plotly)
# library(broom)
# library(haven)
# library(shinydashboard)
# library(shinyWidgets)
# library(shinyjs)
# library(rintrojs)
# library(shinyBS)
# library(shinythemes)


#variables <- read.csv("variables.csv")
#var_cat <- read.csv("variable_categories.csv")

#numvars <- variables %>% filter(meantable == "Yes")

##topics
# top_inc     <- variables %>% filter(topic == "Income, Taxes, and Social Security") %>% select(label_de)
# top_att     <- variables %>% filter(topic == "Attitudes, Values, and Personality") %>% select(label_de)
# top_health  <- variables %>% filter(topic == "Health and Care") %>% select(label_de)
# top_home    <- variables %>% filter(topic == "Home, Amenities, and Contributions of Private HH") %>% select(label_de)
# top_time    <- variables %>% filter(topic == "Time Use and Environmental Behavior") %>% select(label_de)
# top_emp     <- variables %>% filter(topic == "Work and Employment") %>% select(label_de)
# top_demo    <- variables %>% filter(topic == "Demography") %>% select(label_de)




# this function creates the boxes for the main page

# lp_main_box <- function(title_box, image_name, button_name, description) {
#   div(class="landing-page-box",
#       div(title_box, class = "landing-page-box-title"),
#       div(description, class = "landing-page-box-description"),
#       div(class = "landing-page-icon", style= ("background-image: url();
#           background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
#       actionButton(button_name, NULL, class="landing-page-button")
#   )
# }
# 
# lp_about_box <- function(title_box, image_name, button_name, description) {
#   
#   div(class="landing-page-box-about",
#       div(title_box, class = "landing-page-box-title"),
#       div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
#           background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
#       (actionButton(button_name, NULL,
#                     class="landing-page-button",
#                     icon = icon("arrow-circle-right", "icon-lp"),title=description)))
# }


# Define UI for application 


ui <- 
  tagList( #needed for shinyjs
    useShinyjs(),  # Include shinyjs
    introjsUI(),   # Required to enable introjs scripts
    navbarPage(id = "intabset", #needed for landing page
               title = div(tags$a(img(src="soep_icon.png", height=40), href= "https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html"),
                           style = "position: relative; top: -5px;"), # Navigation bar
               windowTitle = "ScotPHO profiles", #title for browser tab
               theme = shinytheme("lumen"), #Theme of the app (blue navbar)
               collapsible = TRUE, #tab panels collapse into menu in small screens
               header = tags$head(includeCSS("www/styles.css"),
                                  HTML("<html lang='en'>"),
                                  tags$link(rel="shortcut icon", href="favicon_scotpho.ico"),
                                  HTML("<base target= '_blank'>")),
               
               
 ##################################################
 # landing page
 ##################################################
               
               tabPanel(
                 setSliderColor(c("red", "red", "red", "red", "red", "red"), c(1,2,3,4,5,6)),
                 title = " Home", icon = icon("home"),
                 mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
                           introBox(
                             fluidRow(column(7,(h3("Welcome to the SOEP data tool", style="margin-top:0px;"))),
                                      (column(4,actionButton("btn_landing",label="Help: How to use this tool",icon=icon('question-circle'))))),

                             data.position = "left"),
                           fluidRow( #summary box
                             column(6, class="landing-page-column",
                                    br(), #spacing
                                    introBox(
                                      lp_main_box(image_name= "landing_button_summary",
                                                  button_name = 'jump_to_summary', 
                                                  title_box = "Summary",
                                                  description = 'Information about SOEP survey data'),
                                      data.position = "bottom-right-aligned")),
                             #table box
                             column(6, class="landing-page-column",
                                    br(), #spacing
                                    introBox( # tour of the tool
                                      lp_main_box(image_name= "landing_button_data",
                                                  button_name = 'jump_to_table', 
                                                  title_box = "Report",
                                                  description = 'Generate a report with aggregated data and visualization from the tool.'),
                                      data.position = "bottom-right-aligned"))),
                           fluidRow(
                             br(), #spacing
                             column(8, style = "padding-left: 0px; padding-right: 0px;",
                                    introBox( #tour of the income and xxx tabs
                                      column(6, class="landing-page-column",
                                             lp_main_box(image_name= "landing_button_income",
                                                         button_name = 'jump_to_income', title_box = "Income, Taxes, and Social Security",
                                                         description = 'Explore how income has change over time')),
                                      column(6, class="landing-page-column",
                                             lp_main_box(image_name= "landing_button_health",
                                                         button_name = 'jump_to_health', 
                                                         title_box = "Health and Care",
                                                         description = 'Explore how health and care has changed over time.')))),
                             
                             #attitudes box
                             column(4, class="landing-page-column",
                                    introBox(
                                      lp_main_box(image_name= "landing_button_att",
                                                  button_name = 'jump_to_attitudes',
                                                  title_box = "Attitudes, Values, and Personality",
                                                  description = 'Explore how attitudes change over time'))
                                    ) #introBox close
                             ), # fluid row close
                           #home box
                           fluidRow(
                             br(), #spacing
                             column(8, style = "padding-left: 0px; padding-right: 0px;",
                                    introBox( 
                                      
                                      column(6, class="landing-page-column",
                                             lp_main_box(image_name= "landing_button_home",
                                                         button_name = 'jump_to_home', 
                                                         title_box = "Home, Amenities, and Contributions to Private HH",
                                                         description = 'Explore how home structure and tenure has changed over time')),
                                      column(6, class="landing-page-column",
                                             lp_main_box(image_name= "landing_button_time",
                                                         button_name = 'jump_to_time', 
                                                         title_box = "Time Use and Environmental Behavior",
                                                         description = 'Explore how time use has changed over time')))),
                             
                             #time box
                             column(4, class="landing-page-column",
                                    introBox(
                                      lp_main_box(image_name= "landing_button_emp",
                                                  button_name = 'jump_to_employment',
                                                  title_box = "Work and Employment",
                                                  description = 'Explore how time use changes over time'))
                             ) #introBox 7 close
                           ), # fluid row close

                           
                         ) #main panel bracket
               ), #tab panel bracket
               
#################################################
# topic panels
################################################


              # SUMMARY

               tabPanel("Summary", icon = icon("list-ul"), value = "summary",
                        sidebarPanel(width = 4,
                                     p("This is a placeholder for an optional summary page")
                                     )
                        ), #tabpanel close
               
              # INCOME

               tabPanel("Income", icon = icon("euro-sign"), value = "income",

                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group1", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = FALSE,
                                                        #options = list(maxItems = 1)
                                                        )),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group2", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("inc_variable", label = "Select an income variable",
                                                     choices = unique(as.character(top_inc$label_de)))),
                                     br(),
                                     p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = 1984,
                                       max = 2019,
                                       step = 1L,
                                       sep = ""
                                      
                                     ),
                                     
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                            label = "Select the plot you want to see",
                                                            choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                            selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_income", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_income_data', 'Download Income Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('inc_lineplot'),
                                     div(style = "margin-top: 30px",
                                         verbatimTextOutput("inc_text"),
                                         verbatimTextOutput("diffvar1"),
                                         verbatimTextOutput("diffvar2"),
                                         verbatimTextOutput("inc_table_text")),
                                         #tableOutput("table")
                                         #dataTableOutput prints 10 rows on screen, user can select more rows and search
                                         #uncomment tableOutput and renderTable to use prior table format
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("inc_table"))
                                         
                                         )
                        
                        
                        ), #tabpanel close

              # HEALTH

               tabPanel("Health", icon = icon("hospital"), value = "health",
                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group3", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = FALSE,
                                                        #options = list(maxItems = 1)
                                         )),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group4", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("health_variable", label = "Select a variable",
                                                     choices = unique(as.character(top_health$label_de)))),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = min(1984, na.rm = TRUE),
                                       max = max(2019, na.rm = TRUE),
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                    label = "Select the plot you want to see",
                                                    choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                    selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_health", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_health_data', 'Download Health Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('health_lineplot'),
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("health_table"))
                                     
                        )
                        
                        
                        ), #tabpanel close

              # ATTITUDES

               tabPanel("Attitudes", icon = icon("smile"), value = "att",
                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group5", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group6", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("att_variable", label = "Select a variable",
                                                     choices = unique(as.character(top_att$label_de)))),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = min(1984, na.rm = TRUE),
                                       max = max(2019, na.rm = TRUE),
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                           label = "Click in the box to select the plot you want to see",
                                                           choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                           selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_att", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_att_data', 'Download Attitudes Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('att_lineplot'),
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("att_table"))
                        )
                        ), #tabpanel close
               
              # HOME

               tabPanel("Home", icon = icon("door-open"), value = "home",
                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group7", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group8", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("home_variable", label = "Select a variable",
                                                     choices = unique(as.character(top_home$label_de)))),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = min(1984, na.rm = TRUE),
                                       max = max(2019, na.rm = TRUE),
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                            label = "Select the plot you want to see",
                                                            choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                            selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_home", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_home_data', 'Download Home Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('home_lineplot'),
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("home_table")))
                        ), #tabpanel close

              # TIME

               tabPanel("Time", icon = icon("clock"), value = "time",
                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group9", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group10", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("time_variable", label = "Select a variable",
                                                     choices = unique(as.character(top_time$label_de)))),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = min(1984, na.rm = TRUE),
                                       max = max(2019, na.rm = TRUE),
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                            label = "Select the plot you want to see",
                                                            choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                            selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_time", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_time_data', 'Download Time Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('time_lineplot'),
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("time_table")))
                        ), #tabpanel close

              # EMPLOYMENT

               tabPanel("Employment", icon = icon("briefcase"), value = "emp",
                        sidebarPanel(width = 4,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group11", "Select your first grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group12", "Select an optional second grouping variable",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1))),
                                     div(style = "margin-top: 30px",
                                         selectInput("emp_variable", label = "Select a variable",
                                                     choices = unique(as.character(top_emp$label_de)))),
                                     sliderInput(
                                       inputId = "yearInput",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = min(1984, na.rm = TRUE),
                                       max = max(2019, na.rm = TRUE),
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select what plot you want to see.",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         checkboxGroupInput("plot_select",
                                                            label = "Select the plot you want to see",
                                                            choices = c("Line", "Stacked Bar", "Side by Side Bar", "Heatmap", "Box Plot"), 
                                                            selected = "Line")),
                                     div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                                         awesomeCheckbox("ci_emp", label = "95% confidence intervals", value = FALSE, status="danger")),
                                     downloadButton('download_emp_data', 'Download Employment Data', class = "down")
                                     ),
                        sidebarPanel(width = 4,
                                     h2("Plots"),
                                     plotlyOutput('emp_lineplot'),
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("emp_table")))
                        ), #tabpanel close


tabPanel("Report", icon = icon("table"), value = "report",
          #Sidepanel for filtering data
          mainPanel(
            width = 12, style="margin-left:0.5%; margin-right:0.5%",
             
            fluidRow(
              p("Generate your report from the tool", 
                style = "font-weight: bold; color: black;")))
         ),


navbarMenu("Info", icon = icon("info-circle"),
           
           
           tabPanel("About SOEP data", 
                    value = "about",
                    sidebarPanel(width=1),
                    mainPanel(width=8,
                              h4("About", style = "color:black;"),
                              p("enter some info here about SOEP, here is a link to ", 
                                tags$a(href="https://paneldata.org/soep-core/", "SOEP page.",
                                       class = "externallink")))
                    ),
           br(),
           tabPanel("Metadata", 
                    value = "metadata",
                    fluidRow(style = "width:60%; margin-left: 2%; min-width: 350px",
                             h4("Some metadata information", style = "color:black;"),
                             h5(style = "color:black", "text, text, text")))
           ) #navbarmenu break
) #navbarpage break

) #final ui break


# Define server logic 
server <- function(input, output, session) {
  
  ## Landing page ----
  ###############################################.
  # Creating events that take you to different tabs
  # activated when pressing buttons from the landing page
  
  observeEvent(input$jump_to_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  })
  
  observeEvent(input$jump_to_table, {
    updateTabsetPanel(session, "intabset", selected = "report")
  })
  
  observeEvent(input$jump_to_income, {
    updateTabsetPanel(session, "intabset", selected = "income")
  })
  
  observeEvent(input$jump_to_health, {
    updateTabsetPanel(session, "intabset", selected = "health")
  })
  
  observeEvent(input$jump_to_attitudes, {
    updateTabsetPanel(session, "intabset", selected = "att")
  })
  
  observeEvent(input$jump_to_home, {
    updateTabsetPanel(session, "intabset", selected = "home")
  })
  
  observeEvent(input$jump_to_time, {
    updateTabsetPanel(session, "intabset", selected = "time")
  })
  
  
  observeEvent(input$jump_to_employment, {
    updateTabsetPanel(session, "intabset", selected = "emp")
  })
  
  ######## Attention: New Code from Stefan ##################
 
  ##################################
  ## Income Panel
  #################################
  
  
   # Variable
  inc_variable <- reactive({ 
    variables$variable[variables$label_de==input$inc_variable]
  })
  
  # grouping1
  diffvar1 <- reactive({ 
    variables$variable[variables$label_de==input$group1]
  })
  
  # grouping2
  diffvar2 <- reactive({ 
    variables$variable[variables$label_de==input$group2]
  })
  
  
  # Select Table Name
  inc_table <- reactive({ 
    get_user_table(meta = variables, variable = inc_variable(),
                   diffvar1 = diffvar1(), diffvar2 = diffvar2(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  inc_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$inc_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$inc_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , inc_variable(), "/", inc_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  
  output$inc_table <- renderDataTable(
    inc_data(), options = list(searching = FALSE))
  
  
  output$inc_text <- renderText(paste0("selected variable: ", inc_variable()))
  output$diffvar1 <- renderText(paste0("selected grouping 1: ", diffvar1()))
  output$diffvar2 <- renderText(paste0("selected grouping 2: ", diffvar2()))
  output$inc_table_text <- renderText(paste0("selected table: ", table()))
  


  
 ################################### 
 #### health panel
 ################################### 
  health_variable <- reactive({ 
    variables$variable[variables$label_de==input$health_variable]
  })
  # grouping3
  diffvar3 <- reactive({ 
    variables$variable[variables$label_de==input$group3]
  })
  
  # grouping4
  diffvar4 <- reactive({ 
    variables$variable[variables$label_de==input$group4]
  })
  
  health_table <- reactive({ 
    get_user_table(meta = variables, variable = health_variable(),
                   diffvar1 = diffvar3(), diffvar2 = diffvar4(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  health_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$health_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$health_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , health_variable(), "/", health_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  

  output$health_table <- renderDataTable(
    health_data(), options = list(searching = FALSE))
  
######################################
  # attitudes panel
#####################################
  
  att_variable <- reactive({ 
    variables$variable[variables$label_de==input$att_variable]
  })
  # grouping3
  diffvar5 <- reactive({ 
    variables$variable[variables$label_de==input$group5]
  })
  
  # grouping4
  diffvar6 <- reactive({ 
    variables$variable[variables$label_de==input$group6]
  })
  
  att_table <- reactive({ 
    get_user_table(meta = variables, variable = att_variable(),
                   diffvar1 = diffvar5(), diffvar2 = diffvar6(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  att_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$att_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$att_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , att_variable(), "/", att_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  
  
  output$att_table <- renderDataTable(
    att_data(), options = list(searching = FALSE))
  
###################################
  # home panel
###################################
  
  home_variable <- reactive({ 
    variables$variable[variables$label_de==input$home_variable]
  })
  # grouping3
  diffvar7 <- reactive({ 
    variables$variable[variables$label_de==input$group7]
  })
  
  # grouping4
  diffvar8 <- reactive({ 
    variables$variable[variables$label_de==input$group8]
  })
  
  home_table <- reactive({ 
    get_user_table(meta = variables, variable = home_variable(),
                   diffvar1 = diffvar7(), diffvar2 = diffvar8(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  home_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$home_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$home_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , home_variable(), "/", home_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  
  
  output$home_table <- renderDataTable(
    home_data(), options = list(searching = FALSE)) 
  

##############################################
  # Time
#############################################
  
  
  time_variable <- reactive({ 
    variables$variable[variables$label_de==input$time_variable]
  })
  # grouping3
  diffvar9 <- reactive({ 
    variables$variable[variables$label_de==input$group9]
  })
  
  # grouping4
  diffvar10 <- reactive({ 
    variables$variable[variables$label_de==input$group10]
  })
  
  time_table <- reactive({ 
    get_user_table(meta = variables, variable = time_variable(),
                   diffvar1 = diffvar9(), diffvar2 = diffvar10(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  time_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$time_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$time_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , time_variable(), "/", time_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  
  
  output$time_table <- renderDataTable(
    time_data(), options = list(searching = FALSE)) 
  
  
#############################
  # employment panel
#############################
  
  
  emp_variable <- reactive({ 
    variables$variable[variables$label_de==input$emp_variable]
  })
  # grouping3
  diffvar11 <- reactive({ 
    variables$variable[variables$label_de==input$group11]
  })
  
  # grouping4
  diffvar12 <- reactive({ 
    variables$variable[variables$label_de==input$group12]
  })
  
  emp_table <- reactive({ 
    get_user_table(meta = variables, variable = emp_variable(),
                   diffvar1 = diffvar11(), diffvar2 = diffvar12(),
                   heatmap = FALSE)
  })
  
  # Load selected Variable
  emp_data <- reactive({
    
    if (variables$meantable[variables$label_de==input$emp_variable] == "Yes") { 
      type <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$emp_variable] == "No") { 
      type <- "categorical"}
    
    tbl <- read.csv(paste0("../tables/", type, "/" , emp_variable(), "/", emp_table()), 
                    encoding = "UTF-8")
    return(tbl)
  })
  
  
  output$emp_table <- renderDataTable(
    emp_data(), options = list(searching = FALSE)) 
  
##################
  # plots
#################
 # line
################
  output$inc_lineplot <- renderPlotly({
      get_lineplot(table = inc_data, meta = variables, variable = inc_variable(),
                   diffvar1 = diffvar1(), diffvar2 = diffvar2(), diffcount = diffcount(),
                   start = 1984, end = 2019, ci = FALSE)
    })
  
  output$health_lineplot <- renderPlotly({
    get_lineplot(table = health_data, meta = variables, variable = health_variable(),
                 diffvar1 = diffvar3(), diffvar2 = diffvar4(), diffcount = diffcount(),
                 start = 1984, end = 2019, ci = FALSE)
  })
  
  output$att_lineplot <- renderPlotly({
    get_lineplot(table = att_data, meta = variables, variable = att_variable(),
                 diffvar1 = diffvar5(), diffvar2 = diffvar6(), diffcount = diffcount(),
                 start = 1984, end = 2019, ci = FALSE)
  })
  
  output$home_lineplot <- renderPlotly({
    get_lineplot(table = home_data, meta = variables, variable = home_variable(),
                 diffvar1 = diffvar7(), diffvar2 = diffvar8(), diffcount = diffcount(),
                 start = 1984, end = 2019, ci = FALSE)
  })
  
  output$time_lineplot <- renderPlotly({
    get_lineplot(table = time_data, meta = variables, variable = time_variable(),
                 diffvar1 = diffvar9(), diffvar2 = diffvar10(), diffcount = diffcount(),
                 start = 1984, end = 2019, ci = FALSE)
  })
  
  output$emp_lineplot <- renderPlotly({
    get_lineplot(table = emp_data, meta = variables, variable = emp_variable(),
                 diffvar1 = diffvar11(), diffvar2 = diffvar12(), diffcount = diffcount(),
                 start = 1984, end = 2019, ci = FALSE)
  })

  
#################
  # Downloads
################
  
  income_csv <- reactive({ format_csv(inc_data())})
  output$download_income_data <- downloadHandler(filename = 'income_data.csv',
                                            content = function(file) {write.csv(income_csv(), file, row.names = TRUE)})
  
  health_csv <- reactive({ format_csv(health_data())})
  output$download_health_data <- downloadHandler(filename = 'health_data.csv',
                                            content = function(file) {write.csv(health_csv(), file, row.names = TRUE)})
  
  att_csv <- reactive({ format_csv(att_data())})
  output$download_att_data <- downloadHandler(filename = 'attitudes_data.csv',
                                            content = function(file) {write.csv(att_csv(), file, row.names = TRUE)})
  
  home_csv <- reactive({ format_csv(home_data())})
  output$download_home_data <- downloadHandler(filename = 'home_data.csv',
                                            content = function(file) {write.csv(home_csv(), file, row.names = TRUE)})
  
  time_csv <- reactive({ format_csv(time_data())})
  output$download_time_data <- downloadHandler(filename = 'timee_data.csv',
                                            content = function(file) {write.csv(time_csv(), file, row.names = TRUE)})
  
  emp_csv <- reactive({ format_csv(emp_data())})
  output$download_emp_data <- downloadHandler(filename = 'emp_data.csv',
                                            content = function(file) {write.csv(emp_csv(), file, row.names = TRUE)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)