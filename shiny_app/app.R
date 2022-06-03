
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
######## start ##################



ui <- 
  tagList( #needed for shinyjs
    useShinyjs(),  # Include shinyjs
    introjsUI(),   # Required to enable introjs scripts
    navbarPage(id = "intabset", #needed for landing page
               title = div(tags$a(img(src="soep_icon.png", height=40), href= "https://www.diw.de/en/diw_01.c.678568.en/research_data_center_soep.html"),
                           style = "position: relative; top: -5px;"), # Navigation bar
               windowTitle = "SOEP data tool", #title for browser tab
               theme = shinytheme("lumen"), #Theme of the app (blue navbar)
               collapsible = TRUE, #tab panels collapse into menu in small screens
               header = tags$head(includeCSS("www/styles.css"),
                                  HTML("<html lang='en'>"),
                                  HTML("<base target= '_blank'>")),
               
               
######## landing page ##################
               
               tabPanel(
                 setSliderColor(c("red", "red", "red", "red", "red", "red", "red"), c(1,2,3,4,5,6,7)),
                 title = " Home", icon = icon("home"), value = 'home_page',
                 mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
                           introBox(
                             fluidRow(column(7,(h3("Welcome to the SOEP data tool", style="margin-top:0px;"))),
                                      (column(4,actionButton("btn_landing",label="Help: How to use this tool",icon=icon('question-circle'), class = "down")))),
                             

                             data.position = "left"),

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
                                                         button_name = 'jump_to_household', 
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
                           fluidRow( #summary box
                             # column(6, class="landing-page-column",
                             #        br(), #spacing
                             #        introBox(
                             #          lp_main_box(image_name= "landing_button_summary",
                             #                      button_name = 'jump_to_summary', 
                             #                      title_box = "Summary",
                             #                      description = 'Information about SOEP survey data'),
                             #          data.position = "bottom-right-aligned")),
                             #table box
                             column(4, class="landing-page-column",
                                    br(), #spacing
                                    introBox( # tour of the tool
                                      lp_main_box(image_name= "landing_button_data",
                                                  button_name = 'jump_to_table', 
                                                  title_box = "Heatmap",
                                                  description = 'Generate a report with aggregated data and visualization from the tool.'),
                                      data.position = "bottom-right-aligned"))),

                           
                         ) #main panel bracket
               ), #tab panel bracket
               
######## topic panels ##################
######## income panel ##################

               tabPanel("Income", icon = icon("euro-sign"), value = "income",

                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(title = "Select from the list of grouping variables.",
                                       style = "margin-top: 30px",
                                         selectizeInput("group1", "Grouping variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable')
                                         )),
                                     div(title = "You may select a second grouping variable from the list. Leave blank if you only wish to see one grouping variable. ",
                                       style = "margin-top: 30px",
                                         selectizeInput("group2", "Grouping variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select an optional second grouping variable'))),
                                     actionButton("reset_inc_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("inc_variable", label = "Select an income variable",
                                                     choices = list(`Categorical Variable` = as.list(top_inc_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_inc_num$label_de)))),
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
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a line plot, side by side bar plot, or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         awesomeRadio("plot_select",
                                                      label = "",
                                                      choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"),
                                                      selected = "Line",
                                                      status = "danger"
                                                            )),
                                           p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_income", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Income page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                         h2("Plots"),
                                         bsButton("info_income",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                         shinycssloaders::withSpinner(plotlyOutput("inc_plot", width = "100%")),
                                         textOutput("text2")),
                                        
                                     div(style = "margin-top: 30px",
                                         h2("Data"),
                                         DT::dataTableOutput("inc_table"))
                                         ))
                        
                        
                        )), #tabpanel close

######## health panel ##################

               tabPanel("Health", icon = icon("hospital"), value = "health",
                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group3", "Grouping variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable')
                                         )),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group4", "Grouping variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select an optional second grouping variable'))),
                                     actionButton("reset_health_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("health_variable", label = "Select a health variable",
                                                     choices = list(`Categorical Variable` = as.list(top_health_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_health_num$label_de)))),
                                     br(),
                                     p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                                     sliderInput(
                                       inputId = "yearInput_health",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = 1984,
                                       max = 2019,
                                       step = 1L,
                                       sep = ""
                                       
                                     ),
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a line plot, side by side bar plot, or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         awesomeRadio("plot_select_health",
                                                      label = "",
                                                      choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"),
                                                      selected = "Line",
                                                      status = "danger"
                                         )),
                                     p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_health", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page2', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Health page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                            h2("Plots"),
                                            bsButton("info_health",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                            shinycssloaders::withSpinner(plotlyOutput("health_plot", width = "100%"))),
                                        div(style = "margin-top: 30px",
                                            h2("Data"),
                                            DT::dataTableOutput("health_table"))
                                 ))
                          
                          
                        )), #tabpanel close

######## attitudes panel ##################

               tabPanel("Attitudes", icon = icon("smile"), value = "att",
                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group5", "Grouping Variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable'))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group6", "Grouping Variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select an optional second grouping variable'))),
                                     actionButton("reset_att_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("att_variable", label = "Select a variable",
                                                     choices = list(`Categorical Variable` = as.list(top_att_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_att_num$label_de)))),
                                     br(),
                                     p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                                     sliderInput(
                                       inputId = "yearInput_att",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = 1984,
                                       max = 2019,
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a side by side bar plot or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         awesomeRadio("plot_select_att",
                                                      label = "",
                                                      choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"), 
                                                      selected = "Line",
                                                      status = "danger")),
                                     p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_att", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page3', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Attitudes page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                            h2("Plots"),
                                            bsButton("info_att",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                            shinycssloaders::withSpinner(plotlyOutput("att_plot", width = "100%"))),
                                        div(style = "margin-top: 30px",
                                            h2("Data"),
                                            DT::dataTableOutput("att_table"))
                                 ))
                          
                          
                        )), #tabpanel close
               
######## household panel ##################

               tabPanel("Household", icon = icon("door-open"), value = "household",
                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group7", "Grouping Variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable'))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group8", "Grouping Variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select an optional second grouping variable'))),
                                     actionButton("reset_home_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("home_variable", label = "Select a home variable",
                                                     choices = list(`Categorical Variable` = as.list(top_home_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_home_num$label_de)))),
                                     br(),
                                     p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                                     sliderInput(
                                       inputId = "yearInput_home",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = 1984,
                                       max = 2019,
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a line plot, side by side bar, plot or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         awesomeRadio("plot_select_home",
                                                      label = "",
                                                      choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"), 
                                                      selected = "Line",
                                                      status = "danger")),
                                     p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_home", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page4', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Household page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                            h2("Plots"),
                                            bsButton("info_home",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                            shinycssloaders::withSpinner(plotlyOutput("home_plot", width = "100%"))),
                                        div(style = "margin-top: 30px",
                                            h2("Data"),
                                            DT::dataTableOutput("home_table"))
                                 ))
                          
                          
                        )), #tabpanel close

######## time panel ##################

               tabPanel("Time", icon = icon("clock"), value = "time",
                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group9", "Grouping Variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable'))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group10", "Grouping Variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select an optional second grouping variable'))),
                                     actionButton("reset_time_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("time_variable", label = "Select a time variable",
                                                     choices = list(`Categorical Variable` = as.list(top_time_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_time_num$label_de)))),
                                     br(),
                                     p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                                     sliderInput(
                                       inputId = "yearInput_time",
                                       label = "Year",
                                       value = c(min(1984, na.rm = TRUE), max(2019, na.rm = TRUE)),
                                       min = 1984, 
                                       max = 2019, 
                                       step = 1L,
                                       sep = ""
                                     ),
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a line plot, side by side bar plot, or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         awesomeRadio("plot_select_time",
                                                        label = "",
                                                        choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"),
                                                        selected = "Line",
                                                        status = "danger"
                                         )),
                                     p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_time", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page5', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Time page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                            h2("Plots"),
                                            bsButton("info_time",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                            shinycssloaders::withSpinner(plotlyOutput("time_plot", width = "100%"))),
                                        div(style = "margin-top: 30px",
                                            h2("Data"),
                                            DT::dataTableOutput("time_table"))
                                 ))
                          
                          
                        )), #tabpanel close



######## employment panel ##################

               tabPanel("Employment", icon = icon("briefcase"), value = "emp",
                        sidebarPanel(width = 2,
                                     h2("Variable Selection"),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group11", "Grouping Variable 1",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Select a grouping variable'))),
                                     div(style = "margin-top: 30px",
                                         selectizeInput("group12", "Grouping Variable 2",
                                                        choices = top_demo$label_de,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 1, placeholder = 'Selected an optional second grouping variable'))),
                                     actionButton("reset_emp_var", "Clear second variable", icon("rotate-right"),
                                                  style=" border-color: #2e6da4"), #dig up some better colors
                                     div(style = "margin-top: 30px",
                                         selectInput("emp_variable", label = "Select an employment variable",
                                                     choices = list(`Categorical Variable` = as.list(top_emp_cat$label_de),
                                                                    `Numerical Variable` = as.list(top_emp_num$label_de)))),
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
                                     div(title="Select a line plot or stacked bar plot if you selected a categorical variable. Select a line plot, side by side bar plot, or box plot if you selected a numerical variables.",
                                         p(tags$b("Select the plot you want to see.")),
                                         awesomeRadio("plot_select_emp",
                                                      label = "",
                                                      choices = c("Line", "Stacked Bar", "Side by Side Bar", "Box Plot"), 
                                                      selected = "Line",
                                                      status = "danger")),
                                     p(tags$b("Show or hide the 95% confidence intervals for the data selected.")), # tooltip
                                         awesomeCheckbox("ci_emp", label = "95% confidence intervals", value = FALSE, status="danger"),
                                     br(),
                                     br(),
                                     div(lp_main_box(image_name= 'back_arrow',
                                                     button_name = 'jump_to_home_page6', 
                                                     title_box = "Go to home page",
                                                     description = 'You are currently on the Employment page. Go back to the home page'))
                                     ),
                        fluidRow(
                          column(9, style = "padding-left: 0px; padding-right: 0px;",
                                 column(9,
                                        div(style = "margin-top: 30px",
                                            h2("Plots"),
                                            bsButton("info_emp",  label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                                            shinycssloaders::withSpinner(plotlyOutput("emp_plot", width = "100%"))),
                                        div(style = "margin-top: 30px",
                                            h2("Data"),
                                            DT::dataTableOutput("emp_table"))
                                 ))
                          
                          
                        )), #tabpanel close
                        
######## heatmap panel ##################

tabPanel("Heatmap", icon = icon("table"), value = "report",

         sidebarPanel(width = 2,
                      h2("Variable Selection"),
                      div(title = "Select from the list of grouping variables.",
                          style = "margin-top: 30px",
                          selectizeInput("groupheat1", "Grouping variable",
                                         choices = top_demo$label_de,
                                         multiple = TRUE,
                                         options = list(maxItems = 1, placeholder = 'Select a grouping variable')
                          )),
                      div(style = "margin-top: 30px",
                          selectInput("heatmap_variable", label = "Select an variable",
                                      choices = list(`Numerical Variable` = as.list(top_heat_num$label_de)))),
                      br(),
                      div(title="Select a statistical indicator",
                          style = "margin-top: 10px; margin-bottom: 20px;",
                          awesomeRadio("statistic_select_heatmap",
                                       label = "Select the statistical indicator you want to see",
                                       choices = c("mean", "median"),
                                       selected = "mean",
                                       status = "danger"
                          )),
                      p("Use the slider to select years", style = "font-weight: bold; color: black;"),
                      sliderInput(
                        inputId = "yearInput_heatmap",
                        label = "Year",
                        value = 2019,
                        min = 1984,
                        max = 2019,
                        step = 1L,
                        sep = ""

                      ),
                      br(),
                      br(),
                      div(lp_main_box(image_name= 'back_arrow',
                                      button_name = 'jump_to_home_page',
                                      title_box = "Go to home page",
                                      description = 'You are currently on the Income page. Go back to the home page'))
         ),
         fluidRow(
           column(9, style = "padding-left: 0px; padding-right: 0px;",
                  column(9,
                         div(style = "margin-top: 30px",
                             h2("Plots"),
                             bsButton("info_heatmap", label = "Variable Information", icon = icon("info-circle"), style = "", size = "small"),
                             shinycssloaders::withSpinner(plotlyOutput("heat_plot", width = "100%")),
                             textOutput("text")),
                         div(style = "margin-top: 30px",
                             h2("Data"),
                             DT::dataTableOutput("heatmap_table"))
                  ))


         )), #tabpanel close



######## navbar ##################

navbarMenu("Info", icon = icon("info-circle"),
           
           
           tabPanel("About SOEP data", 
                    value = "about",
                    sidebarPanel(width=1),
                    mainPanel(width=10,
                              h1("About SOEP data", style = "color:black;"),
                              h2("The German Socio-Economic Panel Study (SOEP)", style = "color:black;"),
                              
                              h4("The German Socio-Economic Panel Study (SOEP) is a 
                              household panel study like the PSID [Panel study of Income Dynamics in the US] and the BHPS [British Household Panel Study]
                              and serves a global research community by providing representative longitudinal data of private households in Germany.
                              But the SOEP is not only a data source, it is also an infrastructure unit and a research institution within the German Institute 
                              for Economic Research (DIW). Therefore, data and research are both provided and conducted at the same time at this institute."),
                              
                              h2("Basic Introduction to the SOEP", style = "color:black;"),
                              h4("The SOEP-Core data edition is the centerpiece of the Socio-Economic Panel and is used for this dashboard. 
                              All provided data output can be calculated with the soep.core.v36eu data release (Doi 10.5684/soep.core.v36eu) from the German Socio-Economic Panel (SOEP)"),
                              
                              fluidPage(
                                HTML('<p style=”text-align:center;”> <iframe width="560" height="315" src="https://www.youtube.com/embed/XBMtaA9vOf4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></p> ')
                              ),
                              
                              h2("Goal, Panel Composition and Sample Development", style = "color:black;"),
                              h4("With the launch of the SOEP in 1984, the intention was to develop a household survey that would provide information on German households. 
                              So the basic idea was that all members of the first-wave survey households and all their offspring 
                              (including those not yet born) should be part of the sample for the purposes of long-term (including intergenerational) 
                              analysis, and that they should be followed as long as possible over time and space. The SOEP defines the target population as 
                              all resident Germans within households. SOEP started with two initial samples. Sample A covers private households with a household 
                              head, who does not belong to one of the main foreigner groups of “guestworkers (i.e. Turkish, Greek, Yugoslavian, Spanish or Italian households) 
                              and the population of Sample B is oversampled and covers private households with a Turkish, Greek, Yugoslavian, Spanish or Italian household head.
                              Due to the unique structure of the SOEP, a previously taken sample such as Sample A or B can still increase, for example, when SOEP households 
                              split (i.e., individuals move out and form their own households), when people move into SOEP households, and when an original sample member 
                              gives birth to a new sample member. Over the decades since it was launched, German society has been changing continuously. and the 
                              SOEP has also reacted to this changes To trace these changes and to describe them in a way that is representative of the entire population, 
                              the study has regularly added new samples of respondents from specific population groups. 
                              One important extension of the study took place just after the fall of the Wall, when “Living in Germany” was expanded to cover the territory of the (former) GDR. 
                              From 2010 to 2013 low-income families (FID) were oversampled and in 2013, the study added a sample of new respondents from migrant populations to Germany. 
                              In 2016, a sample of refugees. And in 2019, a sample of high-wealth individuals and LGBT persons was added. 
                              So SOEP has continuously evolved its efforts in the area of survey methodology."), 
                              
                              fluidPage(
                                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/6V6NtoJYKTA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              ),
                              
                              h2("SOEP Questionnaires", style = "color:black;"),
                              h4("The interview methodology of the SOEP is based on a set of pre-tested questionnaires for households and individuals. 
                              Interviewers try to obtain face-to-face interviews with all members of a given survey household aged 16 and over. 
                              Thus, there are no proxy interviews for adult household members. Additionally, one person (the “head of household”) 
                              is asked to answer a household-related questionnaire covering information on housing, housing costs, and different sources 
                              of income (e.g., social transfers such as social assistance or housing allowances). 
                              This questionnaire also includes questions on children up to the age of 16 in the household, 
                              mainly concerning daycare, kindergarten, and school attendance. The questions in the SOEP are largely identical for 
                              all participants of the survey to ensure comparability across the participants within a given year, but of course there are differences across years. 
                              There are a few exceptions to this rule, which are due to different requirements in the target population. Up to 1996, 
                              the questionnaires for the sample of foreigners (B) and the immigrant sample (D) covered additional measures of integration or 
                              information on re-migration behavior. Between 1990 and 1992, i.e., during the first years of the German reunification process, 
                              the questionnaire for the East German sample (C) also contained some additional specific variables. From 1996 to 2012, 
                              all questionnaires were uniform and completely integrated for all of the main SOEP samples. For the IAB-SOEP Migration Sample,
                              which was launched in 2013, specific questions were added to the SOEP questionnaires. The same is true of the IAB-BAMF-SOEP Survey of Refugees, 
                              which was launched in 2016. Another special questionnaire is used for first-time respondents since some 
                              questions do not have to be repeated every year. Each respondent is asked to fill out a biographical questionnaire covering 
                              information on the life course up to the first SOEP interview (e.g., marital history, social background, and employment biography). 
                              Additional information not provided directly by the respondent can be obtained from the “address logs”. Every address log is filled 
                              in by the interviewer even in the case of non-response, thus providing very valuable information, e.g. 
                              for attrition analysis. For researchers interested in methodological issues, these data also contain information on the 
                              fieldwork process such as the number of contacts, reasons for drop-outs, and interview mode. 
                              For households that were contacted successfully, the address logs cover the size of the household, some regional information, 
                              survey status, etc. The individual data for all household members include the relationship to the household head, survey status 
                              of the individual, and some demographic information."), 
                              
                              fluidPage(
                                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/9jvk6UkmHYo" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              ),
                              
                              h2("Respondents of the SOEP study “Living in Germany", style = "color:black;"),
                              h4("Several thousand people in Germany are surveyed every year as part of the SOEP study “Living in Germany” —the same people every year. 
                                  Currently, there are 32,000 people in 22,000 households participating. 
                                  The aim is to survey these respondents regularly over a long period of time. The long time span enables researchers to study how society changes over time.
                                  “Living in Germany” collects data on many areas of life, including employment, income, housing, education, and health. 
                                  It also covers satisfaction with life and political attitudes and opinions. 
                                  It also deals with events such as the COVID-19 pandemic and the changes it has brought about in our everyday lives.With its broad range of topics, such as employment, education or income, the different questionnaires for each stage of life, the 
                                  household context, the duration of the panel, and the many special sample groups (migrants, refugees, high income households, East Germans), 
                                  comparisons over time can be done with great accuracy using SOEP data."), 

                              fluidPage(
                                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NdgJ4ckoqW4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              ),
                              
                              h2("Topics of the SOEP Dashboard", style = "color:black;"),
                              h4("The topics of the SOEP questionnaires and the various modules they contain can be grouped into 11 areas 
                                  (Demography and Population; Work and Employment; Income, Taxes, and Social Security; Family and Social Networks
                                  Health and Care; Home, Amenities, and Contributions of Private HH; Education and Qualification; Attitudes, Values, and Personality;
                                  Time Use and Environmental Behavior; Integration, Migration, Transnationalization; and Survey Methodology). 
                                  Some of the modules deal with aspects of life that tend to change from one year to the next, 
                                  and are therefore repeated annually, while other modules are repeated at intervals of several years. 
                                  Some SOEP modules are also adapted in different ways to the different questionnaires. 
                                  The questions in the “Big Five” personality traits module, for instance, are formulated differently 
                                  in the mother-child questionnaires than they are in the individual questionnaire. 
                                 The SOEP dashboard currently includes a selection of variables on the six topics, viewable on the starting page and in the navigation "),
                              fluidPage(
                                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/vm-YZ0R17hQ" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                              )
                              ),
                    ),
           br(),
           tabPanel("Metadata", icon = icon("list-ul"), 
                    value = "metadata",
                    sidebarPanel(width = 12,
                                 h2("Search a variable name to see the question"),
                                 div(style = "margin-top: 30px",
                                     DT::dataTableOutput("var_q_table")))
           ), #tabpanel close
           br(),
           tabPanel("How to use this tool", value = "info_page",
                    sidebarPanel(width = 1),
                    mainPanel(width = 10,
                              fluidRow(p(h1("Welcome to the SOEP data tool"),
                                         h4("This interactive dashboard allows you to explore nearly 4 decades of aggregated SOEP survey data.", style = "color:balck;"),
                                         h4("All of the survey questions available in this dashboard fall into a certain topic. These topics can be 
                                         explored in two ways.  One way is to click on the topic at the top of the page (tab).", style = "color:black;"),
                                         br(),
                                         img(src = 'tabs.PNG', height = 50, width = 1000),
                                         br(),
                                         h4("The other way to explore is to click on the appropriate panel on the main page.", style = "color:black;"),
                                         br(),
                                         img(src = 'panel.PNG', height = 500, width = 1000),
                                         br(),
                                         br(),
                                         h4("When you  you click on a topic, such as'Health and Care', you will be taken to a page that contains the variables (questions)
                                         for that topic. The page will be loaded with the first selectable variable for that topic. The default graph and data table contains the overall
                                          statistics for that variable. This is useful for seeing the overall change of this variable for the entire population", style = "color:black;"),
                                         br(),
                                         img(src = 'page.png', height = 600, width = 1000),
                                         br(),
                                         br(),
                                         h4("On the left side of the page are tools you need to explore the data further. To explore the change in time by a certain demographic, select a
                                            grouping variable from the first drop down menu.", style = "color:black;"),
                                         br(),
                                         img(src = 'var1.png', height = 300, width = 280),
                                         br(),
                                         br(),
                                         h4("You have the option to select a second grouping variable from the second drop down menu, if you wish. Note, grouping variables with many levels, such as
                                            Federal States, will take time to load into the graph, and the graph may be more difficult to understand."),
                                         img(src = 'var2.png', height = 340, width = 240),
                                         br(),
                                         br(),
                                         h4("When the graph has loaded, you have the ability to hover over the graph to view the statistics for a particular year.  You can also double-click on a 
                                            certain level in the graph legend to view only that level. Each additional click on other levels with bring them into view."),
                                         br(),
                                         img(src = 'plot1.png', height = 400, width = 900),
                                         br(),
                                         br(),
                                         h4("To narrow the number of years in the graph down, you can use the slider bar in the side panel, or the tabs under the graph."),
                                         br(),
                                         br(),
                                         img(src = 'sliders.png', height = 130, width = 400),
                                         br(),
                                         br(),
                                         br(),
                                         img(src = 'sliders2.png', height = 100, width = 500),
                                         br(),
                                         br(),
                                         h4("You can select a different type of graph from the side panel. Note: graph type depends on the variable type.Select a line plot or stacked bar plot 
                                            if you selected a categorical variable. Select a line plot, side by side bar plot, or box plot if you selected a numerical variables. "),
                                         br(),
                                         br(),
                                         img(src = 'plot2.png', height = 500, width = 1200),
                                         h4("You can download the data within a topic in CSV, Excel, or PDF format, or copy or print the data by selecting the appropriate button 
                                            above the data table.  If you are interested in viewing the data for a specific year, you can enter that year in the search bar."),
                                         br(),
                                         br(),
                                         img(src = 'data.png', height = 500, width = 950)
                                         ))))
           ) #navbarmenu break
) #navbarpage break

) #final ui break




# Define server logic 
server <- function(input, output, session) {
  
  ######## jump to tabs ##################
  
  observeEvent(input$jump_to_home_page, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })
  
  observeEvent(input$jump_to_home_page2, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })
  
  observeEvent(input$jump_to_home_page3, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })
  
  observeEvent(input$jump_to_home_page4, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })
  
  observeEvent(input$jump_to_home_page5, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })
  
  observeEvent(input$jump_to_home_page6, {
    updateTabsetPanel(session, "intabset", selected = "home_page")
  })

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
  
  observeEvent(input$jump_to_household, {
    updateTabsetPanel(session, "intabset", selected = "household")
  })
  
  observeEvent(input$jump_to_time, {
    updateTabsetPanel(session, "intabset", selected = "time")
  })
  
  
  observeEvent(input$jump_to_employment, {
    updateTabsetPanel(session, "intabset", selected = "emp")
  })
  
  observeEvent(input$btn_landing, {
    updateTabsetPanel(session, "intabset", selected = "info_page")
  })
  
  ######## reset buttons ##################
  
  observeEvent(input$reset_inc_var, {reset("group2")})
  observeEvent(input$reset_health_var, {reset("group4")})
  observeEvent(input$reset_att_var, {reset("group6")})
  observeEvent(input$reset_home_var, {reset("group8")})
  observeEvent(input$reset_time_var, {reset("group10")})
  observeEvent(input$reset_emp_var, {reset("group12")})
  
  ##### metadata #####
  output$var_q_table <- DT::renderDataTable({
    DT::datatable(var_q_table, escape = FALSE, options = list(lengthMenu = c(25, 100, 150, 200), pageLength = 25), filter = "top")
  })
 
  ## Income Panel ###########
  
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$inc_variable] == "Yes") { 
      vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$inc_variable] == "No") { 
      vartype <- "categorical"}
    
    return(vartype)
  })
  
  # load startyear
  min <-  reactive({ 
    input$yearInput[1]
  })
  
  # load endyear
  max <-  reactive({ 
    input$yearInput[2]
  })
  
  # load plottype
  type <-  reactive({ 
    input$plot_select
  })
  
  # load plottype
  ci <-  reactive({ 
    input$ci_income
  })
  
  
  ######## income box plot ##################
  # load graphic
  # 1.1 boxplot
  boxplot <-
    reactive({
    req(input$plot_select)
    req(input$inc_variable)
    req(inc_data())

   # boxplot
    if (input$plot_select == 'Box Plot') {
    if(!isTruthy(input$group1)){
         get_boxplot(table = inc_data(), meta = variables, variable = inc_variable(),
                     diffvar2 = "", diffvar3 = "", start = min(), end = max())
      }

    else if (isTruthy(input$group1) & (!isTruthy(input$group2))){
      get_boxplot(table = inc_data(), meta = variables, variable = inc_variable(),
                  diffvar2 = diffvar1(), diffvar3 = "", start = min(), end = max())
      }

    else if (isTruthy(input$group2) & (isTruthy(input$group2))){
        get_boxplot(table = inc_data(), meta = variables, variable = inc_variable(),
                    diffvar2 = diffvar1(), diffvar3 = diffvar2(), start = min(), end = max())
      }
    }

  })
  ######## income line plot ##################
  # 1.2 lineplot
  lineplot <-
    reactive({
      req(input$plot_select)
      req(input$inc_variable)
      req(inc_data())
      
      # lineplot
      if (input$plot_select == 'Line') {
      if (vartype() == "numerical") {
          if(!isTruthy(input$group1)){
             get_lineplot(data = inc_data(),
                       meta = variables,
                       variable = inc_variable(),
                       diffvar1 = "",
                       diffvar2 = "",
                       diffcount = 1,
                       start = min(),
                       end = max(),
                       ci = ci())
        }
         else if (isTruthy(input$group1) & (!isTruthy(input$group2))){
                  get_lineplot(data = inc_data(),
                               meta = variables,
                               variable = inc_variable(),
                               diffvar1 = diffvar1(),
                               diffvar2 = "",
                               diffcount = 2,
                               start = min(),
                               end = max(),
                               ci = ci())
        }
         else if (isTruthy(input$group2) & (isTruthy(input$group2))){
                  get_lineplot(data = inc_data(),
                               meta = variables,
                               variable = inc_variable(),
                               diffvar1 = diffvar1(),
                               diffvar2 = diffvar2(),
                               diffcount = 3,
                               start = min(),
                               end = max(),
                               ci = ci())
        }
      }
      else if (vartype() == "categorical") {
          if(!isTruthy(input$group1)){
            get_percent_lineplot(data = inc_data(),
                                 meta = variables,
                                 variable = inc_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = min(),
                                 end = max(),
                                 ci = ci())
          }
          else if (isTruthy(input$group1) & (!isTruthy(input$group2))){
            get_percent_lineplot(data = inc_data(),
                                 meta = variables,
                                 variable = inc_variable(),
                                 diffvar1 = diffvar1(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = min(),
                                 end = max(),
                                 ci = ci())
          }
          else if (isTruthy(input$group2) & (isTruthy(input$group2))){
            get_percent_lineplot(data = inc_data(),
                                 meta = variables,
                                 variable = inc_variable(),
                                 diffvar1 = diffvar1(),
                                 diffvar2 = diffvar2(),
                                 diffcount = 3,
                                 start = min(),
                                 end = max(),
                                 ci = ci())
          }
      }
      }  
    })
  ######## income stacked bar plot ##################
  # 1.3 Stacked Bar
  stackbarplot <-
    reactive({
      req(input$plot_select)
      req(input$inc_variable)
      req(inc_data())
      
      # Stacked Bar
      if (input$plot_select == 'Stacked Bar') {
        if(!isTruthy(input$group1)){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = ci(), 
                      start = min(), end = max())
        }
        else if (isTruthy(input$group1) & (!isTruthy(input$group2))){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = diffvar1(), diffvar2 = "", 
                      plottype = "stack", ci = ci(), 
                      start = min(), end = max())
        }
        else if (isTruthy(input$group2) & (isTruthy(input$group2))){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = diffvar1(), diffvar2 = diffvar2(), 
                      plottype = "stack", ci = ci(), 
                      start = min(), end = max())
        }
      }
    })
  ######## income side by side plot ##################
  
  # 1.4 Side by Side
  dodgebarplot <-
    reactive({
      req(input$plot_select)
      req(input$inc_variable)
      req(inc_data())
      
      # Stacked Bar
      if (input$plot_select == 'Side by Side Bar') {
        if(!isTruthy(input$group1)){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = ci(), 
                      start = min(), end = max())
        }
        else if (isTruthy(input$group1) & (!isTruthy(input$group2))){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = diffvar1(), diffvar2 = "", 
                      plottype = "dodge", ci = ci(), 
                      start = min(), end = max())
        }
        else if (isTruthy(input$group2) & (isTruthy(input$group2))){
          get_barplot(data = inc_data(), meta = variables, 
                      variable = inc_variable(), 
                      diffvar1 = diffvar1(), diffvar2 = diffvar2(), 
                      plottype = "dodge", ci = ci(), 
                      start = min(), end = max())
        }
      }
    })
  
  
  ######## income render plotly ##################
  t3 <- Sys.time()
  output$inc_plot <- renderPlotly({
    
    if (input$plot_select == 'Box Plot') {
        boxplot()
    }
    
    else if (input$plot_select == 'Line') {
      lineplot()
    }
    
    else if (input$plot_select == 'Stacked Bar') {
      stackbarplot()
    }
    
    else if (input$plot_select == 'Side by Side Bar') {
      dodgebarplot()
    }
  })
  Sys.sleep(2)
  t4 <- Sys.time()
  output$text2 <- renderText({paste("The income plot took", round(t4-t3, 2), "seconds to load")})
  
  #### income Var Information Button   ####  
  observeEvent(input$inc_variable, {
    
    removePopover(session, "info_income")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$inc_variable] != "" & 
       variables$documentation_link[variables$label_de==input$inc_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$inc_variable] != ""){
      
      addPopover(session, "info_income", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$inc_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$inc_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$inc_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$inc_variable] != "" & 
            variables$documentation_link[variables$label_de==input$inc_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$inc_variable] == ""){
      
      addPopover(session, "info_income", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$inc_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$inc_variable] == "" & 
            variables$documentation_link[variables$label_de==input$inc_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$inc_variable] != ""){
      
      addPopover(session, "info_income", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$inc_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$inc_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$inc_variable] == "" & 
            variables$documentation_link[variables$label_de==input$inc_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$inc_variable] != ""){
      
      addPopover(session, "info_income", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$inc_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$inc_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  ######## income render datatable ##################
  # load data
  output$inc_table <- DT::renderDataTable(
    inc_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      #columnDefs = list(list(visible=FALSE, targets = "value")),
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  

  
 #### health panel ##############

  # Variable
  health_variable <- reactive({ 
    variables$variable[variables$label_de==input$health_variable]
  })
  
  # grouping1
  diffvar3 <- reactive({ 
    variables$variable[variables$label_de==input$group3]
  })
  
  # grouping2
  diffvar4 <- reactive({ 
    variables$variable[variables$label_de==input$group4]
  })
  
  
  # Select Table Name
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  health_vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$health_variable] == "Yes") { 
      health_vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$health_variable] == "No") { 
      health_vartype <- "categorical"}
    
    return(health_vartype)
  })
  
  # load startyear
  health_min <-  reactive({ 
    input$yearInput_health[1]
  })
  
  # load endyear
  health_max <-  reactive({ 
    input$yearInput_health[2]
  })
  
  # load plottype
  health_type <-  reactive({ 
    input$plot_select_health
  })
  
  # load plottype
  health_ci <-  reactive({ 
    input$ci_health
  })
  
  ######## health box plot ##################
  # load graphic
  # 1.1 boxplot
  boxplot_health <-
    reactive({
      req(input$plot_select_health)
      req(input$health_variable)
      req(health_data())
      
      # boxplot
      if (input$plot_select_health == 'Box Plot') {
        if(!isTruthy(input$group3)){
          get_boxplot(table = health_data(), meta = variables, variable = health_variable(),
                      diffvar2 = "", diffvar3 = "", start = health_min(), end = health_max())
        }
        
        else if (isTruthy(input$group3) & (!isTruthy(input$group4))){
          get_boxplot(table = health_data(), meta = variables, variable = health_variable(),
                      diffvar2 = diffvar3(), diffvar3 = "", start = health_min(), end = health_max())
        }
        
        else if (isTruthy(input$group4) & (isTruthy(input$group4))){
          get_boxplot(table = health_data(), meta = variables, variable = health_variable(),
                      diffvar2 = diffvar3(), diffvar3 = diffvar4(), start = health_min(), end = health_max())
        }
      }
      
    })
  ######## health line plot ##################
  # 1.2 lineplot
  lineplot_health <-
    reactive({
      req(input$plot_select_health)
      req(input$health_variable)
      req(health_data())
      
      # lineplot
      if (input$plot_select_health == 'Line') {
        if (health_vartype() == "numerical") {
          if(!isTruthy(input$group3)){
            get_lineplot(data = health_data(),
                         meta = variables,
                         variable = health_variable(),
                         diffvar1 = "",
                         diffvar2 = "",
                         diffcount = 1,
                         start = health_min(),
                         end = health_max(),
                         ci = health_ci())
          }
          else if (isTruthy(input$group3) & (!isTruthy(input$group4))){
            get_lineplot(data = health_data(),
                         meta = variables,
                         variable = health_variable(),
                         diffvar1 = diffvar3(),
                         diffvar2 = "",
                         diffcount = 2,
                         start = health_min(),
                         end = health_max(),
                         ci = health_ci())
          }
          else if (isTruthy(input$group4) & (isTruthy(input$group4))){
            get_lineplot(data = health_data(),
                         meta = variables,
                         variable = health_variable(),
                         diffvar1 = diffvar3(),
                         diffvar2 = diffvar4(),
                         diffcount = 3,
                         start = health_min(),
                         end = health_max(),
                         ci = health_ci())
          }
        }
        else if (health_vartype() == "categorical") {
          if(!isTruthy(input$group3)){
            get_percent_lineplot(data = health_data(),
                                 meta = variables,
                                 variable = health_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = health_min(),
                                 end = health_max(),
                                 ci = health_ci())
          }
          else if (isTruthy(input$group3) & (!isTruthy(input$group4))){
            get_percent_lineplot(data = health_data(),
                                 meta = variables,
                                 variable = health_variable(),
                                 diffvar1 = diffvar3(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = health_min(),
                                 end = health_max(),
                                 ci = health_ci())
          }
          else if (isTruthy(input$group4) & (isTruthy(input$group4))){
            get_percent_lineplot(data = health_data(),
                                 meta = variables,
                                 variable = health_variable(),
                                 diffvar1 = diffvar3(),
                                 diffvar2 = diffvar4(),
                                 diffcount = 3,
                                 start = health_min(),
                                 end = health_max(),
                                 ci = health_ci())
          }
        }
      }  
    })
  ######## health stacked bar plot ##################
  # 1.3 Stacked Bar
  stackbarplot_health <-
    reactive({
      req(input$plot_select_health)
      req(input$health_variable)
      req(health_data())
      
      # Stacked Bar
      if (input$plot_select_health == 'Stacked Bar') {
        if(!isTruthy(input$group3)){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
        else if (isTruthy(input$group3) & (!isTruthy(input$group4))){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = diffvar3(), diffvar2 = "", 
                      plottype = "stack", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
        else if (isTruthy(input$group4) & (isTruthy(input$group4))){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = diffvar3(), diffvar2 = diffvar4(), 
                      plottype = "stack", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
      }
    })
  ######## health side by side plot ##################
  # 1.4 Side by Side
  dodgebarplot_health <-
    reactive({
      req(input$plot_select_health)
      req(input$health_variable)
      req(health_data())
      
      # Stacked Bar
      if (input$plot_select_health == 'Side by Side Bar') {
        if(!isTruthy(input$group3)){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
        else if (isTruthy(input$group3) & (!isTruthy(input$group4))){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = diffvar3(), diffvar2 = "", 
                      plottype = "dodge", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
        else if (isTruthy(input$group4) & (isTruthy(input$group4))){
          get_barplot(data = health_data(), meta = variables, 
                      variable = health_variable(), 
                      diffvar1 = diffvar3(), diffvar2 = diffvar4(), 
                      plottype = "dodge", ci = health_ci(), 
                      start = health_min(), end = health_max())
        }
      }
    })
  
  ######## health render plotly ##################
  
  output$health_plot <- renderPlotly({
    
    if (input$plot_select_health == 'Box Plot') {
      boxplot_health()
    }
    
    else if (input$plot_select_health == 'Line') {
      lineplot_health()
    }
    
    else if (input$plot_select_health == 'Stacked Bar') {
      stackbarplot_health()
    }
    
    else if (input$plot_select_health == 'Side by Side Bar') {
      dodgebarplot_health()
    }
  })
  
  #### health Var Information Button   ####
  observeEvent(input$health_variable, {
    
    removePopover(session, "info_health")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$health_variable] != "" & 
       variables$documentation_link[variables$label_de==input$health_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$health_variable] != ""){
      
      addPopover(session, "info_health", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$health_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$health_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$health_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$health_variable] != "" & 
            variables$documentation_link[variables$label_de==input$health_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$health_variable] == ""){
      
      addPopover(session, "info_health", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$health_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$health_variable] == "" & 
            variables$documentation_link[variables$label_de==input$health_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$health_variable] != ""){
      
      addPopover(session, "info_health", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$health_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$health_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$health_variable] != "" & 
            variables$documentation_link[variables$label_de==input$health_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$health_variable] == ""){
      
      addPopover(session, "info_health", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$health_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$health_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$health_variable] != "" & 
            variables$documentation_link[variables$label_de==input$health_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$health_variable] != ""){
      
      addPopover(session, "info_health", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$health_variable],
                             "  <br>",
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$health_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  
  
  ######## health render datatable ##################
  # load data
  output$health_table <- renderDataTable(
    health_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  

  ## attitudes panel #####

    # variable
  att_variable <- reactive({ 
    variables$variable[variables$label_de==input$att_variable]
  })
  # grouping1
  diffvar5 <- reactive({ 
    variables$variable[variables$label_de==input$group5]
  })
  
  # grouping2
  diffvar6 <- reactive({ 
    variables$variable[variables$label_de==input$group6]
  })
  # Select table name
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  att_vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$att_variable] == "Yes") { 
      att_vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$att_variable] == "No") { 
      att_vartype <- "categorical"}
    
    return(att_vartype)
  })
  
  # load startyear
  att_min <-  reactive({ 
    input$yearInput_att[1]
  })
  
  # load endyear
  att_max <-  reactive({ 
    input$yearInput_att[2]
  })
  
  # load plottype
  att_type <-  reactive({ 
    input$plot_select_att
  })
  
  # load plottype
  att_ci <-  reactive({ 
    input$ci_att
  })
  
  ######## attituded box plot ##################
  # load graphic
  # 1.1 boxplot
  boxplot_att <-
    reactive({
      req(input$plot_select_att)
      req(input$att_variable)
      req(att_data())
      
      # boxplot
      if (input$plot_select_att == 'Box Plot') {
        if(!isTruthy(input$group5)){
          get_boxplot(table = att_data(), meta = variables, variable = att_variable(),
                      diffvar2 = "", diffvar3 = "", start = att_min(), end = att_max())
        }
        
        else if (isTruthy(input$group5) & (!isTruthy(input$group6))){
          get_boxplot(table = att_data(), meta = variables, variable = att_variable(),
                      diffvar2 = diffvar5(), diffvar3 = "", start = att_min(), end = att_max())
        }
        
        else if (isTruthy(input$group6) & (isTruthy(input$group6))){
          get_boxplot(table = att_data(), meta = variables, variable = att_variable(),
                      diffvar2 = diffvar5(), diffvar3 = diffvar6(), start = att_min(), end = att_max())
        }
      }
      
    })
  ######## attitudes line plot ##################
  # 1.2 lineplot
  lineplot_att <-
    reactive({
      req(input$plot_select_att)
      req(input$att_variable)
      req(att_data())
      
      # lineplot
      if (input$plot_select_att == 'Line') {
        if (att_vartype() == "numerical") {
          if(!isTruthy(input$group5)){
            get_lineplot(data = att_data(),
                         meta = variables,
                         variable = att_variable(),
                         diffvar1 = "",
                         diffvar2 = "",
                         diffcount = 1,
                         start = att_min(),
                         end = att_max(),
                         ci = att_ci())
          }
          else if (isTruthy(input$group5) & (!isTruthy(input$group6))){
            get_lineplot(data = att_data(),
                         meta = variables,
                         variable = att_variable(),
                         diffvar1 = diffvar5(),
                         diffvar2 = "",
                         diffcount = 2,
                         start = att_min(),
                         end = att_max(),
                         ci = att_ci())
          }
          else if (isTruthy(input$group6) & (isTruthy(input$group6))){
            get_lineplot(data = att_data(),
                         meta = variables,
                         variable = att_variable(),
                         diffvar1 = diffvar5(),
                         diffvar2 = diffvar6(),
                         diffcount = 3,
                         start = att_min(),
                         end = att_max(),
                         ci = att_ci())
          }
        }
        else if (att_vartype() == "categorical") {
          if(!isTruthy(input$group5)){
            get_percent_lineplot(data = att_data(),
                                 meta = variables,
                                 variable = att_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = att_min(),
                                 end = att_max(),
                                 ci = att_ci())
          }
          else if (isTruthy(input$group5) & (!isTruthy(input$group6))){
            get_percent_lineplot(data = att_data(),
                                 meta = variables,
                                 variable = att_variable(),
                                 diffvar1 = diffvar5(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = att_min(),
                                 end = att_max(),
                                 ci = att_ci())
          }
          else if (isTruthy(input$group6) & (isTruthy(input$group6))){
            get_percent_lineplot(data = att_data(),
                                 meta = variables,
                                 variable = att_variable(),
                                 diffvar1 = diffvar5(),
                                 diffvar2 = diffvar6(),
                                 diffcount = 3,
                                 start = att_min(),
                                 end = att_max(),
                                 ci = att_ci())
          }
        }
      }  
    })
  ######## attitudes stacked bar plot ##################
  # 1.3 Stacked Bar
  stackbarplot_att <-
    reactive({
      req(input$plot_select_att)
      req(input$att_variable)
      req(att_data())
      
      # Stacked Bar
      if (input$plot_select_att == 'Stacked Bar') {
        if(!isTruthy(input$group5)){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
        else if (isTruthy(input$group5) & (!isTruthy(input$group6))){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = diffvar5(), diffvar2 = "", 
                      plottype = "stack", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
        else if (isTruthy(input$group6) & (isTruthy(input$group6))){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = diffvar5(), diffvar2 = diffvar6(), 
                      plottype = "stack", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
      }
    })
  ######## attitudes side by side plot ##################
  # 1.4 Side by Side
  dodgebarplot_att <-
    reactive({
      req(input$plot_select_att)
      req(input$att_variable)
      req(att_data())
      
      # Stacked Bar
      if (input$plot_select_att == 'Side by Side Bar') {
        if(!isTruthy(input$group5)){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
        else if (isTruthy(input$group5) & (!isTruthy(input$group6))){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = diffvar5(), diffvar2 = "", 
                      plottype = "dodge", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
        else if (isTruthy(input$group6) & (isTruthy(input$group6))){
          get_barplot(data = att_data(), meta = variables, 
                      variable = att_variable(), 
                      diffvar1 = diffvar5(), diffvar2 = diffvar6(), 
                      plottype = "dodge", ci = att_ci(), 
                      start = att_min(), end = att_max())
        }
      }
    })
  ######## attitudes render plotly ##################
  
  output$att_plot <- renderPlotly({
    
    if (input$plot_select_att == 'Box Plot') {
      boxplot_att()
    }
    
    else if (input$plot_select_att == 'Line') {
      lineplot_att()
    }
    
    else if (input$plot_select_att == 'Stacked Bar') {
      stackbarplot_att()
    }
    
    else if (input$plot_select_att == 'Side by Side Bar') {
      dodgebarplot_att()
    }
  })
  
  #### attitudes Var Information Button   ####
  observeEvent(input$att_variable, {
    
    removePopover(session, "info_att")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$att_variable] != "" & 
       variables$documentation_link[variables$label_de==input$att_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$att_variable] != ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$att_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$att_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$att_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$att_variable] != "" & 
            variables$documentation_link[variables$label_de==input$att_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$att_variable] == ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$att_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$att_variable] == "" & 
            variables$documentation_link[variables$label_de==input$att_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$att_variable] != ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$att_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$att_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$att_variable] == "" & 
            variables$documentation_link[variables$label_de==input$att_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$att_variable] != ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$att_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$att_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    else if(variables$description[variables$label_de==input$att_variable] != "" & 
            variables$documentation_link[variables$label_de==input$att_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$att_variable] == ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$att_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$health_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$att_variable] != "" & 
            variables$documentation_link[variables$label_de==input$att_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$att_variable] != ""){
      
      addPopover(session, "info_att", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$att_variable],
                             "  <br>",
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$att_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  
  ######## attitudes render datatable ##################
  
  # load data
  
  
  output$att_table <- renderDataTable(
    att_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  

  ### home panel ####

  
  home_variable <- reactive({ 
    variables$variable[variables$label_de==input$home_variable]
  })
  # grouping1
  diffvar7 <- reactive({ 
    variables$variable[variables$label_de==input$group7]
  })
  
  # grouping2
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  home_vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$home_variable] == "Yes") { 
      home_vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$home_variable] == "No") { 
      home_vartype <- "categorical"}
    
    return(home_vartype)
  })
  
  # load startyear
  home_min <-  reactive({ 
    input$yearInput_home[1]
  })
  
  # load endyear
  home_max <-  reactive({ 
    input$yearInput_home[2]
  })
  
  # load plottype
  home_type <-  reactive({ 
    input$plot_select_home
  })
  
  # load plottype
  home_ci <-  reactive({ 
    input$ci_home
  })
  
  ######## home box plot ##################
  # load graphic
  # 1.1 boxplot
  boxplot_home <-
    reactive({
      req(input$plot_select_home)
      req(input$home_variable)
      req(home_data())
      
      # boxplot
      if (input$plot_select_home == 'Box Plot') {
        if(!isTruthy(input$group7)){
          get_boxplot(table = home_data(), meta = variables, variable = home_variable(),
                      diffvar2 = "", diffvar3 = "", start = home_min(), end = home_max())
        }
        
        else if (isTruthy(input$group7) & (!isTruthy(input$group8))){
          get_boxplot(table = home_data(), meta = variables, variable = home_variable(),
                      diffvar2 = diffvar7(), diffvar3 = "", start = home_min(), end = home_max())
        }
        
        else if (isTruthy(input$group8) & (isTruthy(input$group8))){
          get_boxplot(table = home_data(), meta = variables, variable = home_variable(),
                      diffvar2 = diffvar7(), diffvar3 = diffvar8(), start = home_min(), end = home_max())
        }
      }
      
    })
  ######## home line plot ##################
  # 1.2 lineplot
  lineplot_home <-
    reactive({
      req(input$plot_select_home)
      req(input$home_variable)
      req(home_data())
      
      # lineplot
      if (input$plot_select_home == 'Line') {
        if (home_vartype() == "numerical") {
          if(!isTruthy(input$group7)){
            get_lineplot(data = home_data(),
                         meta = variables,
                         variable = home_variable(),
                         diffvar1 = "",
                         diffvar2 = "",
                         diffcount = 1,
                         start = home_min(),
                         end = home_max(),
                         ci = home_ci())
          }
          else if (isTruthy(input$group7) & (!isTruthy(input$group8))){
            get_lineplot(data = home_data(),
                         meta = variables,
                         variable = home_variable(),
                         diffvar1 = diffvar7(),
                         diffvar2 = "",
                         diffcount = 2,
                         start = home_min(),
                         end = home_max(),
                         ci = home_ci())
          }
          else if (isTruthy(input$group8) & (isTruthy(input$group8))){
            get_lineplot(data = home_data(),
                         meta = variables,
                         variable = home_variable(),
                         diffvar1 = diffvar7(),
                         diffvar2 = diffvar8(),
                         diffcount = 3,
                         start = home_min(),
                         end = home_max(),
                         ci = home_ci())
          }
        }
        else if (home_vartype() == "categorical") {
          if(!isTruthy(input$group7)){
            get_percent_lineplot(data = home_data(),
                                 meta = variables,
                                 variable = home_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = home_min(),
                                 end = home_max(),
                                 ci = home_ci())
          }
          else if (isTruthy(input$group7) & (!isTruthy(input$group8))){
            get_percent_lineplot(data = home_data(),
                                 meta = variables,
                                 variable = home_variable(),
                                 diffvar1 = diffvar7(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = home_min(),
                                 end = home_max(),
                                 ci = home_ci())
          }
          else if (isTruthy(input$group8) & (isTruthy(input$group8))){
            get_percent_lineplot(data = home_data(),
                                 meta = variables,
                                 variable = home_variable(),
                                 diffvar1 = diffvar7(),
                                 diffvar2 = diffvar8(),
                                 diffcount = 3,
                                 start = home_min(),
                                 end = home_max(),
                                 ci = home_ci())
          }
        }
      }  
    })
  ######## home stacked bar plot ##################
  
  # 1.3 Stacked Bar
  stackbarplot_home <-
    reactive({
      req(input$plot_select_home)
      req(input$home_variable)
      req(home_data())
      
      # Stacked Bar
      if (input$plot_select_home == 'Stacked Bar') {
        if(!isTruthy(input$group7)){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
        else if (isTruthy(input$group7) & (!isTruthy(input$group8))){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = diffvar7(), diffvar2 = "", 
                      plottype = "stack", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
        else if (isTruthy(input$group8) & (isTruthy(input$group8))){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = diffvar7(), diffvar2 = diffvar8(), 
                      plottype = "stack", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
      }
    })
  ######## home side by side plot ##################
  
  # 1.4 Side by Side
  dodgebarplot_home <-
    reactive({
      req(input$plot_select_home)
      req(input$home_variable)
      req(home_data())
      
      # Stacked Bar
      if (input$plot_select_home == 'Side by Side Bar') {
        if(!isTruthy(input$group7)){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
        else if (isTruthy(input$group7) & (!isTruthy(input$group8))){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = diffvar7(), diffvar2 = "", 
                      plottype = "dodge", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
        else if (isTruthy(input$group8) & (isTruthy(input$group8))){
          get_barplot(data = home_data(), meta = variables, 
                      variable = home_variable(), 
                      diffvar1 = diffvar7(), diffvar2 = diffvar8(), 
                      plottype = "dodge", ci = home_ci(), 
                      start = home_min(), end = home_max())
        }
      }
    })
  
  ######## home render plotly ##################
  
  output$home_plot <- renderPlotly({
    
    if (input$plot_select_home == 'Box Plot') {
      boxplot_home()
    }
    
    else if (input$plot_select_home == 'Line') {
      lineplot_home()
    }
    
    else if (input$plot_select_home == 'Stacked Bar') {
      stackbarplot_home()
    }
    
    else if (input$plot_select_home == 'Side by Side Bar') {
      dodgebarplot_home()
    }
  })
  
  #### household Var Information Button   ####
  observeEvent(input$home_variable, {
    
    removePopover(session, "info_home")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$home_variable] != "" & 
       variables$documentation_link[variables$label_de==input$home_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$home_variable] != ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$home_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$home_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$home_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$home_variable] != "" & 
            variables$documentation_link[variables$label_de==input$home_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$home_variable] == ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$home_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$home_variable] == "" & 
            variables$documentation_link[variables$label_de==input$home_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$home_variable] != ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$home_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$home_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$home_variable] == "" & 
            variables$documentation_link[variables$label_de==input$home_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$home_variable] != ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$home_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$home_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    else if(variables$description[variables$label_de==input$home_variable] != "" & 
            variables$documentation_link[variables$label_de==input$home_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$home_variable] == ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$home_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$home_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$home_variable] != "" & 
            variables$documentation_link[variables$label_de==input$home_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$home_variable] != ""){
      
      addPopover(session, "info_home", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$home_variable],
                             "  <br>",
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$home_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  
  ######## home render datatable ##################
  
  output$home_table <- renderDataTable(
    home_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )) 
  

  ### Time ####
  
  time_variable <- reactive({ 
    variables$variable[variables$label_de==input$time_variable]
  })
  # grouping9
  diffvar9 <- reactive({ 
    variables$variable[variables$label_de==input$group9]
  })
  
  # grouping10
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  time_vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$time_variable] == "Yes") { 
      time_vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$time_variable] == "No") { 
      time_vartype <- "categorical"}
    
    return(time_vartype)
  })
  
  # load startyear
  time_min <-  reactive({ 
    input$yearInput_time[1]
  })
  
  # load endyear
  time_max <-  reactive({ 
    input$yearInput_time[2]
  })
  
  # load plottype
  time_type <-  reactive({ 
    input$plot_select_time
  })
  
  # load plottype
  time_ci <-  reactive({ 
    input$ci_time
  })
  
  ######## time box plot ##################
  
  # load graphic
  # 1.1 boxplot
  boxplot_time <-
    reactive({
      req(input$plot_select_time)
      req(input$time_variable)
      req(time_data())
      
      # boxplot
      if (input$plot_select_time == 'Box Plot') {
        if(!isTruthy(input$group9)){
          get_boxplot(table = time_data(), meta = variables, variable = time_variable(),
                      diffvar2 = "", diffvar3 = "", start = time_min(), end = time_max())
        }
        
        else if (isTruthy(input$group9) & (!isTruthy(input$group10))){
          get_boxplot(table = time_data(), meta = variables, variable = time_variable(),
                      diffvar2 = diffvar9(), diffvar3 = "", start = time_min(), end = time_max())
        }
        
        else if (isTruthy(input$group10) & (isTruthy(input$group10))){
          get_boxplot(table = time_data(), meta = variables, variable = time_variable(),
                      diffvar2 = diffvar9(), diffvar3 = diffvar10(), start = time_min(), end = time_max())
        }
      }
      
    })
  ######## time line plot ##################
  
  # 1.2 lineplot
  lineplot_time <-
    reactive({
      req(input$plot_select_time)
      req(input$time_variable)
      req(time_data())
      
      # lineplot
      if (input$plot_select_time == 'Line') {
        if (time_vartype() == "numerical") {
          if(!isTruthy(input$group9)){
            get_lineplot(data = time_data(),
                         meta = variables,
                         variable = time_variable(),
                         diffvar1 = "",
                         diffvar2 = "",
                         diffcount = 1,
                         start = time_min(),
                         end = time_max(),
                         ci = time_ci())
          }
          else if (isTruthy(input$group9) & (!isTruthy(input$group10))){
            get_lineplot(data = time_data(),
                         meta = variables,
                         variable = time_variable(),
                         diffvar1 = diffvar9(),
                         diffvar2 = "",
                         diffcount = 2,
                         start = time_min(),
                         end = time_max(),
                         ci = time_ci())
          }
          else if (isTruthy(input$group10) & (isTruthy(input$group10))){
            get_lineplot(data = time_data(),
                         meta = variables,
                         variable = time_variable(),
                         diffvar1 = diffvar9(),
                         diffvar2 = diffvar10(),
                         diffcount = 3,
                         start = time_min(),
                         end = time_max(),
                         ci = time_ci())
          }
        }
        else if (time_vartype() == "categorical") {
          if(!isTruthy(input$group9)){
            get_percent_lineplot(data = time_data(),
                                 meta = variables,
                                 variable = time_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = time_min(),
                                 end = time_max(),
                                 ci = time_ci())
          }
          else if (isTruthy(input$group9) & (!isTruthy(input$group10))){
            get_percent_lineplot(data = time_data(),
                                 meta = variables,
                                 variable = time_variable(),
                                 diffvar1 = diffvar9(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = time_min(),
                                 end = time_max(),
                                 ci = time_ci())
          }
          else if (isTruthy(input$group10) & (isTruthy(input$group10))){
            get_percent_lineplot(data = time_data(),
                                 meta = variables,
                                 variable = time_variable(),
                                 diffvar1 = diffvar9(),
                                 diffvar2 = diffvar10(),
                                 diffcount = 3,
                                 start = time_min(),
                                 end = time_max(),
                                 ci = time_ci())
          }
        }
      }  
    })
  ######## time stacked bar plot ##################
  
  # 1.3 Stacked Bar
  stackbarplot_time <-
    reactive({
      req(input$plot_select_time)
      req(input$time_variable)
      req(time_data())
      
      # Stacked Bar
      if (input$plot_select_time == 'Stacked Bar') {
        if(!isTruthy(input$group9)){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
        else if (isTruthy(input$group9) & (!isTruthy(input$group10))){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = diffvar9(), diffvar2 = "", 
                      plottype = "stack", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
        else if (isTruthy(input$group10) & (isTruthy(input$group10))){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = diffvar9(), diffvar2 = diffvar10(), 
                      plottype = "stack", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
      }
    })
  ######## time side by side plot ##################
  
  # 1.4 Side by Side
  dodgebarplot_time <-
    reactive({
      req(input$plot_select_time)
      req(input$time_variable)
      req(time_data())
      
      # Stacked Bar
      if (input$plot_select_time == 'Side by Side Bar') {
        if(!isTruthy(input$group9)){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
        else if (isTruthy(input$group9) & (!isTruthy(input$group10))){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = diffvar9(), diffvar2 = "", 
                      plottype = "dodge", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
        else if (isTruthy(input$group10) & (isTruthy(input$group10))){
          get_barplot(data = time_data(), meta = variables, 
                      variable = time_variable(), 
                      diffvar1 = diffvar9(), diffvar2 = diffvar10(), 
                      plottype = "dodge", ci = time_ci(), 
                      start = time_min(), end = time_max())
        }
      }
    })
  
  ######## time render plotly ##################
  
  output$time_plot <- renderPlotly({
    
    if (input$plot_select_time == 'Box Plot') {
      boxplot_time()
    }
    
    else if (input$plot_select_time == 'Line') {
      lineplot_time()
    }
    
    else if (input$plot_select_time == 'Stacked Bar') {
      stackbarplot_time()
    }
    
    else if (input$plot_select_time == 'Side by Side Bar') {
      dodgebarplot_time()
    }
  })
  
  #### time Var Information Button   ####
  observeEvent(input$time_variable, {
    
    removePopover(session, "info_time")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$time_variable] != "" & 
       variables$documentation_link[variables$label_de==input$time_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$time_variable] != ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$time_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$time_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$time_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$time_variable] != "" & 
            variables$documentation_link[variables$label_de==input$time_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$time_variable] == ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$time_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$time_variable] == "" & 
            variables$documentation_link[variables$label_de==input$time_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$time_variable] != ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$time_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$time_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$time_variable] == "" & 
            variables$documentation_link[variables$label_de==input$time_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$time_variable] != ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$time_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$time_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    else if(variables$description[variables$label_de==input$time_variable] != "" & 
            variables$documentation_link[variables$label_de==input$time_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$time_variable] == ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$time_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$time_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$time_variable] != "" & 
            variables$documentation_link[variables$label_de==input$time_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$time_variable] != ""){
      
      addPopover(session, "info_time", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$time_variable],
                             "  <br>",
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$time_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  
  ######## time render datatable ##################
  
  
  output$time_table <- renderDataTable(
    time_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )) 
  
  

  ######## employment panel ##################
  
  
  emp_variable <- reactive({ 
    variables$variable[variables$label_de==input$emp_variable]
  })
  # grouping1
  diffvar11 <- reactive({ 
    variables$variable[variables$label_de==input$group11]
  })
  
  # grouping2
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
    
    if (type == "categorical") {
      tbl <- tbl[with(tbl, order(year, value)), ]
    }
    
    return(tbl)
  })
  
  # Confirm vartype
  emp_vartype <- reactive({
    
    if (variables$meantable[variables$label_de==input$emp_variable] == "Yes") { 
      emp_vartype <- "numerical"}
    
    if (variables$meantable[variables$label_de==input$emp_variable] == "No") { 
      emp_vartype <- "categorical"}
    
    return(emp_vartype)
  })
  
  # load startyear
  emp_min <-  reactive({ 
    input$yearInput_emp[1]
  })
  
  # load endyear
  emp_max <-  reactive({ 
    input$yearInput_emp[2]
  })
  
  # load plottype
  emp_type <-  reactive({ 
    input$plot_select_emp
  })
  
  # load plottype
  emp_ci <-  reactive({ 
    input$ci_emp
  })
  
  ######## employment boxplot ##################
  
  # load graphic
  # 1.1 boxplot
  boxplot_emp <-
    reactive({
      req(input$plot_select_emp)
      req(input$emp_variable)
      req(emp_data())
      
      # boxplot
      if (input$plot_select_emp == 'Box Plot') {
        if(!isTruthy(input$group11)){
          get_boxplot(table = emp_data(), meta = variables, variable = emp_variable(),
                      diffvar2 = "", diffvar3 = "", start = emp_min(), end = emp_max())
        }
        
        else if (isTruthy(input$group11) & (!isTruthy(input$group12))){
          get_boxplot(table = emp_data(), meta = variables, variable = emp_variable(),
                      diffvar2 = diffvar11(), diffvar3 = "", start = emp_min(), end = emp_max())
        }
        
        else if (isTruthy(input$group12) & (isTruthy(input$group12))){
          get_boxplot(table = emp_data(), meta = variables, variable = emp_variable(),
                      diffvar2 = diffvar11(), diffvar3 = diffvar12(), start = emp_min(), end = emp_max())
        }
      }
      
    })
  ######## employment line plot ##################
  
  # 1.2 lineplot
  lineplot_emp <-
    reactive({
      req(input$plot_select_emp)
      req(input$emp_variable)
      req(emp_data())
      
      # lineplot
      if (input$plot_select_emp == 'Line') {
        if (emp_vartype() == "numerical") {
          if(!isTruthy(input$group11)){
            get_lineplot(data = emp_data(),
                         meta = variables,
                         variable = emp_variable(),
                         diffvar1 = "",
                         diffvar2 = "",
                         diffcount = 1,
                         start = emp_min(),
                         end = emp_max(),
                         ci = emp_ci())
          }
          else if (isTruthy(input$group11) & (!isTruthy(input$group12))){
            get_lineplot(data = emp_data(),
                         meta = variables,
                         variable = emp_variable(),
                         diffvar1 = diffvar11(),
                         diffvar2 = "",
                         diffcount = 2,
                         start = emp_min(),
                         end = emp_max(),
                         ci = emp_ci())
          }
          else if (isTruthy(input$group12) & (isTruthy(input$group12))){
            get_lineplot(data = emp_data(),
                         meta = variables,
                         variable = emp_variable(),
                         diffvar1 = diffvar11(),
                         diffvar2 = diffvar12(),
                         diffcount = 3,
                         start = emp_min(),
                         end = emp_max(),
                         ci = emp_ci())
          }
        }
        else if (emp_vartype() == "categorical") {
          if(!isTruthy(input$group11)){
            get_percent_lineplot(data = emp_data(),
                                 meta = variables,
                                 variable = emp_variable(),
                                 diffvar1 = "",
                                 diffvar2 = "",
                                 diffcount = 1,
                                 start = emp_min(),
                                 end = emp_max(),
                                 ci = emp_ci())
          }
          else if (isTruthy(input$group11) & (!isTruthy(input$group12))){
            get_percent_lineplot(data = emp_data(),
                                 meta = variables,
                                 variable = emp_variable(),
                                 diffvar1 = diffvar11(),
                                 diffvar2 = "",
                                 diffcount = 2,
                                 start = emp_min(),
                                 end = emp_max(),
                                 ci = emp_ci())
          }
          else if (isTruthy(input$group12) & (isTruthy(input$group12))){
            get_percent_lineplot(data = emp_data(),
                                 meta = variables,
                                 variable = emp_variable(),
                                 diffvar1 = diffvar11(),
                                 diffvar2 = diffvar12(),
                                 diffcount = 3,
                                 start = emp_min(),
                                 end = emp_max(),
                                 ci = emp_ci())
          }
        }
      }  
    })
  ######## employment stacked bar plot ##################
  
  # 1.3 Stacked Bar
  stackbarplot_emp <-
    reactive({
      req(input$plot_select_emp)
      req(input$emp_variable)
      req(emp_data())
      
      # Stacked Bar
      if (input$plot_select_emp == 'Stacked Bar') {
        if(!isTruthy(input$group11)){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "stack", ci = emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
        else if (isTruthy(input$group11) & (!isTruthy(input$group12))){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = diffvar11(), diffvar2 = "", 
                      plottype = "stack", ci = emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
        else if (isTruthy(input$group12) & (isTruthy(input$group12))){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = diffvar11(), diffvar2 = diffvar12(), 
                      plottype = "stack", ci = emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
      }
    })
  
  ######## employment side by side plot ##################
  # 1.4 Side by Side
  dodgebarplot_emp <-
    reactive({
      req(input$plot_select_emp)
      req(input$emp_variable)
      req(emp_data())
      
      # Stacked Bar
      if (input$plot_select_emp == 'Side by Side Bar') {
        if(!isTruthy(input$group11)){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = "", diffvar2 = "", 
                      plottype = "dodge", ci = emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
        else if (isTruthy(input$group11) & (!isTruthy(input$group12))){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = diffvar11(), diffvar2 = "", 
                      plottype = "dodge", ci =emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
        else if (isTruthy(input$group12) & (isTruthy(input$group12))){
          get_barplot(data = emp_data(), meta = variables, 
                      variable = emp_variable(), 
                      diffvar1 = diffvar11(), diffvar2 = diffvar12(), 
                      plottype = "dodge", ci = emp_ci(), 
                      start = emp_min(), end = emp_max())
        }
      }
    })
  
  ######## employment render plotly ##################
  
  
  output$emp_plot <- renderPlotly({
    
    if (input$plot_select_emp == 'Box Plot') {
      boxplot_emp()
    }
    
    else if (input$plot_select_emp == 'Line') {
      lineplot_emp()
    }
    
    else if (input$plot_select_emp == 'Stacked Bar') {
      stackbarplot_emp()
    }
    
    else if (input$plot_select_emp == 'Side by Side Bar') {
      dodgebarplot_emp()
    }
  })
  
  #### employment Var Information Button   ####
  
  observeEvent(input$emp_variable, {
    
    removePopover(session, "info_emp")
    Sys.sleep(0.2)
    
    if(variables$description[variables$label_de==input$emp_variable] != "" & 
       variables$documentation_link[variables$label_de==input$emp_variable] != "" & 
       variables$documentation_paper[variables$label_de==input$emp_variable] != ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$emp_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$emp_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$emp_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_link[variables$label_de==input$emp_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$emp_variable] == ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$emp_variable],
                             "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$emp_variable] == "" & 
            variables$documentation_link[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$emp_variable] != ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$emp_variable],
                               target="_blank"), "  <br>", 
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$emp_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$emp_variable] == "" & 
            variables$documentation_link[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$emp_variable] != ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$emp_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$emp_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    else if(variables$description[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_link[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_paper[variables$label_de==input$emp_variable] == ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$emp_variable],
                             "  <br>", 
                             a("Additional Variable Information", 
                               href = variables$documentation_link[variables$label_de==input$emp_variable],
                               target="_blank"), "  <br>"
                 )),
                 trigger = "hold", placement = "right")
    }
    
    else if(variables$description[variables$label_de==input$emp_variable] != "" & 
            variables$documentation_link[variables$label_de==input$emp_variable] == "" & 
            variables$documentation_paper[variables$label_de==input$emp_variable] != ""){
      
      addPopover(session, "info_emp", title=HTML("<b> Variable description: </b>"), 
                 HTML(paste0(variables$description[variables$label_de==input$emp_variable],
                             "  <br>",
                             a("Dataset Codebook ", 
                               href = variables$documentation_paper[variables$label_de==input$emp_variable],
                               target="_blank")
                 )),
                 trigger = "hold", placement = "right")
    }
  })
  
  
  ######## employment render datatable ##################
  
  
  output$emp_table <- renderDataTable(
    emp_data(), server = FALSE,
    extensions = c('Buttons', 'Scroller', 'Responsive'),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )) 
  
  ## Heatmap Panel ###########
  t1 <- Sys.time()
  # Variable
  heatmap_variable <- reactive({
    variables$variable[variables$label_de==input$heatmap_variable]
  })

  # grouping1
  diffvar_heatmap <- reactive({
    variables$variable[variables$label_de==input$groupheat1]
  })

  statistic_heatmap <-  reactive({
    input$statistic_select_heatmap
  })

  # Select Table Name
  heatmap_table <- reactive({
    get_user_table(meta = variables, variable = heatmap_variable(),
                   diffvar1 = diffvar_heatmap(), diffvar2 = "",
                   heatmap = TRUE)
  })

  # Load selected Variable
  heatmap_data <- reactive({

    if (variables$meantable[variables$label_de==input$heatmap_variable] == "Yes") {
      type <- "numerical"}

    if (variables$meantable[variables$label_de==input$heatmap_variable] == "No") {
      type <- "categorical"}

    tbl <- read.csv(paste0("../tables/", type, "/" , heatmap_variable(), "/", heatmap_table()),
                    encoding = "UTF-8")
    return(tbl)
  })


# load year
year_heatmap <-  reactive({
  input$yearInput_heatmap
})

######## heatmap map plot ##################


# load graphic
# 1.1 heatmap
heatmap <-
  reactive({
    req(input$statistic_select_heatmap)
    req(input$heatmap_variable)
    req(heatmap_data())
    req(year_heatmap())

    # heatmap
    if(!isTruthy(input$groupheat1)){

      get_map_plot(table = heatmap_data(),
                   syear = year_heatmap(),
                   variable = heatmap_variable(),
                   statistic = statistic_heatmap(),
                   diffvar = "")

    }

    else {

      get_map_plot(table = heatmap_data(),
                   syear = year_heatmap(),
                   variable = heatmap_variable(),
                   statistic = statistic_heatmap(),
                   diffvar = diffvar_heatmap())
    }
  })



######## heatmap render plotly ##################
output$heat_plot <- renderPlotly({
  heatmap()
})

# Var Information Button
observeEvent(input$heatmap_variable, {

  removePopover(session, "info_heatmap")
  Sys.sleep(0.2)

  if(variables$description[variables$label_de==input$heatmap_variable] != "" &
     variables$documentation_link[variables$label_de==input$heatmap_variable] != "" &
     variables$documentation_paper[variables$label_de==input$heatmap_variable] != ""){

    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"),
               HTML(paste0(variables$description[variables$label_de==input$heatmap_variable],
                           "  <br>",
                           a("Additional Variable Information",
                             href = variables$documentation_link[variables$label_de==input$heatmap_variable],
                             target="_blank"), "  <br>",
                           a("Dataset Codebook ",
                             href = variables$documentation_paper[variables$label_de==input$heatmap_variable],
                             target="_blank")
               )),
               trigger = "hold", placement = "right")
  }

  else if(variables$description[variables$label_de==input$heatmap_variable] != "" &
          variables$documentation_link[variables$label_de==input$heatmap_variable] == "" &
          variables$documentation_paper[variables$label_de==input$heatmap_variable] == ""){

    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"),
               HTML(paste0(variables$description[variables$label_de==input$heatmap_variable],
                           "  <br>"
               )),
               trigger = "hold", placement = "right")
  }
  
  

  else if(variables$description[variables$label_de==input$heatmap_variable] == "" &
          variables$documentation_link[variables$label_de==input$heatmap_variable] != "" &
          variables$documentation_paper[variables$label_de==input$heatmap_variable] != ""){

    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"),
               HTML(paste0(a("Additional Variable Information",
                             href = variables$documentation_link[variables$label_de==input$heatmap_variable],
                             target="_blank"), "  <br>",
                           a("Dataset Codebook ",
                             href = variables$documentation_paper[variables$label_de==input$heatmap_variable],
                             target="_blank")
               )),
               trigger = "hold", placement = "right")
  }
  
  else if(variables$description[variables$label_de==input$heatmap_variable] != "" &
          variables$documentation_link[variables$label_de==input$heatmap_variable] != "" &
          variables$documentation_paper[variables$label_de==input$heatmap_variable] == ""){
    
    
    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"),
               HTML(paste0(variables$description[variables$label_de==input$heatmap_variable],
                           "  <br>",
                           a("Additional Variable Information",
                             href = variables$documentation_link[variables$label_de==input$heatmap_variable],
                             target="_blank"), "  <br>"
               )),
               trigger = "hold", placement = "right")
  }
  else if(variables$description[variables$label_de==input$heatmap_variable] != "" & 
          variables$documentation_link[variables$label_de==input$heatmap_variable] != "" & 
          variables$documentation_paper[variables$label_de==input$heatmap_variable] == ""){
    
    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"), 
               HTML(paste0(variables$description[variables$label_de==input$heatmap_variable],
                           "  <br>", 
                           a("Additional Variable Information", 
                             href = variables$documentation_link[variables$label_de==input$heatmap_variable],
                             target="_blank"), "  <br>"
               )),
               trigger = "hold", placement = "right")
  }
  
  else if(variables$description[variables$label_de==input$heatmap_variable] != "" & 
          variables$documentation_link[variables$label_de==input$heatmap_variable] == "" & 
          variables$documentation_paper[variables$label_de==input$heatmap_variable] != ""){
    
    addPopover(session, "info_heatmap", title=HTML("<b> Variable description: </b>"), 
               HTML(paste0(variables$description[variables$label_de==input$heatmap_variable],
                           "  <br>",
                           a("Dataset Codebook ", 
                             href = variables$documentation_paper[variables$label_de==input$heatmap_variable],
                             target="_blank")
               )),
               trigger = "hold", placement = "right")
  }
})

######## heatmap render datatable ##################
# load data
output$heatmap_table <- DT::renderDataTable(
  heatmap_data(), server = FALSE,
  extensions = c('Buttons', 'Scroller', 'Responsive'),
  options = list(
    dom = 'Bfrtip',
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
Sys.sleep(2)
t2 <- Sys.time()
output$text <- renderText({paste("The heatmap took", round(t2-t1, 2), "seconds to load")})
#################
  # Downloads
################
  
  income_csv <- reactive({ format_csv(inc_data())})
  output$download_income_data <- downloadHandler(filename = 'income_data.csv',
                                            content = function(file) {write.csv(inc_data(), file, row.names = TRUE)})
  
  health_csv <- reactive({ format_csv(health_data())})
  output$download_health_data <- downloadHandler(filename = 'health_data.csv',
                                            content = function(file) {write.csv(health_data(), file, row.names = TRUE)})
  
  att_csv <- reactive({ format_csv(att_data())})
  output$download_att_data <- downloadHandler(filename = 'attitudes_data.csv',
                                            content = function(file) {write.csv(att_data(), file, row.names = TRUE)})
  
  home_csv <- reactive({ format_csv(home_data())})
  output$download_home_data <- downloadHandler(filename = 'household_data.csv',
                                            content = function(file) {write.csv(home_data(), file, row.names = TRUE)})
  
  time_csv <- reactive({ format_csv(time_data())})
  output$download_time_data <- downloadHandler(filename = 'time_data.csv',
                                            content = function(file) {write.csv(time_data(), file, row.names = TRUE)})
  
  emp_csv <- reactive({ format_csv(emp_data())})
  output$download_emp_data <- downloadHandler(filename = 'emp_data.csv',
                                            content = function(file) {write.csv(emp_data(), file, row.names = TRUE)})
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)