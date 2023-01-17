library(shiny)
library(shinydashboard)

# # Define UI for application
spinner = 'double-bounce'

dashboardPage(
  dashboardHeader(
    title = '',
    titleWidth = 0
    ),
  dashboardSidebar(
    width = 100,
    collapsed = TRUE,
    sidebarMenu(
      menuItem('Homepage', tabName = 'dashboard', icon = icon('house')),
      menuItem("Home Flipping", tabName = 'flipping', icon = icon('wand-magic-sparkles')),
      menuItem("Machine Learning", tabName = 'ml', icon = icon('gears')),
      menuItem("About", tabName = 'about', icon = icon('user'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'dashboard',
        fluidPage(
          theme = 'journal',
          
# ----------------------------------------------------------------
# MAIN PANEL
# ----------------------------------------------------------------
          
          fluidRow(
            box(
              title = tagList('Location', 
                icon('location-dot')),
              status='success',
              collapsible=TRUE,
              width = 4,
              height = 200, 
              
              # ------------------------------
              # Main Panel: Property Selection
              # ------------------------------
              selectInput(
                inputId = 'neighborhood', 
                label = 'Neighborhood',
                choices = c('All Neighborhoods', 
                            unique(df_predictions$Neighborhood)),
                selected = 'Neighborhood'
              ),
              pickerInput(
                inputId = 'address',
                label = 'Property',
                choices = df_predictions$Prop_Addr,
                options = pickerOptions(
                  virtualScroll = TRUE,
                  dropupAuto = FALSE,
                  width = '100%'
                )
              )
            ),
          
            # ------------------------------
            # Main Panel: Info Boxes
            # ------------------------------
            
              column(
                width = 4,
                style='padding:3px',
                  valueBoxOutput(width=NULL, 'crime_rate'),
                  valueBoxOutput(width=NULL, 'school_quality')
              ),
              column(
                width = 4, 
                style='padding:3px',
                    valueBoxOutput(width=NULL, 'income'),
                    valueBoxOutput(width=NULL, 'appreciation'),
                  
              )
            ),
            
##################### VERTICAL PANELS ############################

            verticalTabsetPanel(
              menuSide = 'right',
            
# ----------------------------------------------------------------
# HOUSING MAP PANEL
# ----------------------------------------------------------------
            
            verticalTabPanel(
              title = 'Ames Housing Map', box_height = 5,
              icon = icon("house", class = "fa-2x"),
              leafletOutput("map")
            ),
            
# ----------------------------------------------------------------
# TIMESERIES PANEL
# ----------------------------------------------------------------

            verticalTabPanel(
              title = 'Time Forecasting', box_height = 5,
              icon = icon("line-chart", class = "fa-2x"),
              materialSwitch(
                status = 'success',
                inputId = 'ts_ci', 
                label = 'Show confidence intervals', 
                value = TRUE, right = TRUE
              ),
              addSpinner(
                uiOutput(outputId = "sarima", height = "100%"),
                spin = spinner
              )
              
            ),
            
# ----------------------------------------------------------------
# PARAMETER TUNING PANEL
# ----------------------------------------------------------------
            
            verticalTabPanel(
              title = 'Parameter Tuning', box_height = 5,
              icon = icon('gears', class = "fa-2x"),
              
              # ------------------------------
              # Parameter Tuning: Current Home
              # ------------------------------
              
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
                
                # ------------------------------
                # Parameter Tuning: What If...?
                # ------------------------------
                
                column(
                    width = 5,
                    style='padding:3px',
                    box(
                      width = NULL,
                      title=tagList('What if...', icon('question')),
                      status = 'info', 
                      solidHeader = TRUE,                    
                      collapsible = TRUE,
                      
                      # ------------------------------------
                      # Parameter Tuning: Sq. Ft. Slider
                      # ------------------------------------
                      
                      sliderInput(
                        inputId = 'sqft_slider',
                        label = 'Total Sq. Ft',
                        min = 1, max = 2000,
                        value = 1, step = 10,
                        animate =
                          animationOptions(interval = 300, loop = TRUE)),
                      
                      # ------------------------------------
                      # Parameter Tuning: Bed/Bath Slider
                      # ------------------------------------
                      
                      # column(
                      #   width = 6,
                      #   style='padding:1px',
                      # sliderInput(
                      #   inputId = 'sqft_slider',
                      #   label = 'Total Sq. Ft',
                      #   min = 1, max = 2000,
                      #   value = 1, step = 10,
                      #   animate =
                      #     animationOptions(interval = 300, loop = TRUE))),
                      # 
                      # column(
                      #   width = 6,
                      #   style='padding:1px',
                      #   sliderInput(
                      #     inputId = 'sqft_slider',
                      #     label = 'Total Sq. Ft',
                      #     min = 1, max = 2000,
                      #     value = 1, step = 10,
                      #     animate =
                      #       animationOptions(interval = 300, loop = TRUE))),
                      
                      # ------------------------------
                      # Parameter Tuning: Left Column
                      # ------------------------------
                      
                      column(
                        offset = 1,
                        width = 5,
                        style='padding:0px',
                        
                        # ------------------------------
                        # Left Column: Bedrooms
                        # ------------------------------
                        
                        uiOutput(outputId = 'Bedrooms'),
                        actionGroupButtons(
                          inputIds = c('bedrooms_up', 'bedrooms_down'),
                          labels = list(tags$span(icon('arrow-up'), ''),
                                      tags$span(icon('arrow-down'), '')),
                          status = c('success', 'danger')
                        ),
                        
                        # ------------------------------
                        # Left Column: Overall Condition
                        # ------------------------------
                        
                        tags$br(), tags$br(),
                        uiOutput(outputId = 'OverallCondition'),
                        actionGroupButtons(
                          inputIds = c('OvCond_up', 'OvCond_down'),
                          labels = list(tags$span(icon('arrow-up'), ''),
                                        tags$span(icon('arrow-down'), '')),
                          status = c('success', 'danger')
                        )
                      ),
                      
                      # ------------------------------
                      # Parameter Tuning: Right Column
                      # ------------------------------
                      
                      column(
                        offset = 1,
                        width = 5,
                        style='padding:0px',
                        
                        # ------------------------------
                        # Right Column: Bathrooms
                        # ------------------------------
                        
                        uiOutput(outputId = 'Bathrooms'),
                        actionGroupButtons(
                          inputIds = c('bathrooms_up', 'bathrooms_down'),
                          labels = list(tags$span(icon('arrow-up'), ''),
                                        tags$span(icon('arrow-down'), '')),
                          status = c('success', 'danger')
                        ),
                        
                        # ------------------------------
                        # Right Column: Overall Quality
                        # ------------------------------
                        
                        tags$br(), tags$br(),
                        uiOutput(outputId = 'OverallQuality'),
                        actionGroupButtons(
                          inputIds = c('OvQual_up', 'OvQual_down'),
                          labels = list(tags$span(icon('arrow-up'), ''),
                                       tags$span(icon('arrow-down'), '')),
                          status = c('success', 'danger')
                          )
                        )
                      ),
                    
                    # ------------------------------
                    # Parameter Tuning: Prediction
                    # ------------------------------
                    
                      infoBoxOutput(
                        width=NULL, 
                        outputId = 'prediction'),
                  
                  
                
                )
              )
            ),
            
# ----------------------------------------------------------------
# DATASET PANEL
# ----------------------------------------------------------------

            verticalTabPanel(
              box_height = 5,
              title = 'Dataset', 
              icon = icon("exchange", class = "fa-2x"),
              fluidRow(
                dataTableOutput(
                  outputId = 'prediction_df', 
                  height = "100%")
              )
            )
          )
        )
      )
    )
  )
)
