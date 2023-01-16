library(shiny)
library(shinydashboard)

# # Define UI for application
spinner = 'folding-cube'

dashboardPage(
  dashboardHeader(
    title = ''),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem(
        "Homepage", 
        tabName = "homepage", 
        icon = icon("landmark"),
        menuSubItem("Maps", tabName = "dreamhome", icon = icon("map")),
        menuSubItem("Maps2?", tabName = "landmark2", icon = icon("map"))
      ),
      menuItem("Dream Home", tabName = 'landma', icon = icon('house')),
      menuItem("Home Flipping", tabName = 'flipping', icon = icon('wand-magic-sparkles')),
      menuItem("Machine Learning", tabName = 'ml', icon = icon('gears')),
      menuItem("About", tabName = 'about', icon = icon('user'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dreamhome",
        fluidPage(
          theme = 'journal',
          fluidRow(
            box(
              title=tagList('Location', icon('location-dot')),
              status='success',
              collapsible=TRUE,
              width = 4,
              height = 200, 
                selectInput(
                  inputId = 'neighborhood', 
                  label = 'Neighborhood',
                  choices = c('All Neighborhoods', 
                              unique(df_predictions$Neighborhood)),
                  selected = 'Neighborhood'
                ),
                pickerInput(
                  inputId = 'address',
                  label = 'Homes',
                  choices = df_predictions$Prop_Addr),
                  options = pickerOptions(
                    virtualScroll = TRUE,
                    dropupAuto = FALSE,
                    width = '70%',
                    title = FALSE
            )),
          
            column(
              width = 4,
              style='padding:3px',
                valueBoxOutput(width=NULL, 'crime_rate'),
                valueBoxOutput(width=NULL, 'school_quality')),
          column(
            width = 4, 
            style='padding:3px',
                valueBoxOutput(width=NULL, 'income'),
                valueBoxOutput(width=NULL, 'appreciation'),
              
          )),
        
##################################################################

          verticalTabsetPanel(
            menuSide = 'right',
            
            verticalTabPanel(
              title = 'Time Forecasting', box_height = 5,
              icon = icon("line-chart", class = "fa-2x"),
              # tags$b('Forecasted Time Analysis of Homes in Ames'),
              tags$br(), tags$br(), tags$br(), 
              addSpinner(
                uiOutput(outputId = "sarima", height = "100%"),
                spin = spinner
              )
            ),
            
            verticalTabPanel(
              title = 'Parameter Tuning', box_height = 5,
              icon = icon('gears', class = "fa-2x"),
              
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  style='padding:3px',
                  box(
                    width = NULL,
                    title=tagList('Current Home', icon('house')),
                    status = 'info', 
                    solidHeader = TRUE,                    
                    collapsible = TRUE,
                    uiOutput(outputId = 'current_home')
                  )),
                column(
                    width = 5,
                    style='padding:3px',
                    box(
                      width = NULL,
                      title=tagList('What if...', icon('question')),
                      status = 'info', 
                      solidHeader = TRUE,                    
                      collapsible = TRUE,
                      sliderInput(inputId = "animation", 
                                  label = "Looping Animation:",
                                  min = 1, max = 2000,
                                  value = 1, step = 10,
                                  animate =
                                    animationOptions(interval = 300, loop = TRUE)),
                      valueBoxOutput(width=NULL, 'prediction')),
                  
                  
                
                )
              )
            ),
            
            verticalTabPanel(
              box_height = 5,
              title = "Exchange", icon = icon("exchange", class = "fa-2x"),
              addSpinner(
                billboarderOutput(outputId = "plot_exchange", height = "100%"),
                spin = spinner
              ),
              materialSwitch(
                inputId = "by_country", 
                label = "See details by country", 
                value = FALSE, right = TRUE
              )
            ),
            
            verticalTabPanel(
              box_height = 5,
              title = "Generation capacities", icon = icon("industry", class = "fa-2x"),
              radioGroupButtons(
                inputId = "capacities_plot", 
                label = "Active production units", 
                choiceNames = list(
                  tags$span(icon("percent"), "Summary"),
                  tags$span(icon("map"), "Map"), 
                  tags$span(icon("industry"), "Global")
                ),
                choiceValues = c("summary", "map", "global"),
                status = "dreamrs", justified = TRUE, selected = "summary"
              ),
              conditionalPanel(
                condition = "input.capacities_plot == 'summary'",
                addSpinner(
                  billboarderOutput(outputId = "plot_active_units_p", height = "100%"),
                  spin = spinner
                )
              ),
              conditionalPanel(
                condition = "input.capacities_plot != 'summary'",
                addSpinner(
                  leafletOutput(outputId = "map_capacities", height = "100%"),
                  spin = spinner
                )
              )
            )
          )
        )
      )
    )
  )
)
