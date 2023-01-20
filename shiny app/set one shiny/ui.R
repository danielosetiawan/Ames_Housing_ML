library(shiny)
library(shinydashboard)

# # Define UI for application
spinner = 'double-bounce'
theme = 'Minty'


dashboardPage(
  dashboardHeader(
    disable = TRUE
    ),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem('Homepage', tabName = 'dashboard', icon = icon('house')),
      menuItem('Dataset', tabName = 'dataset', icon = icon('wand-magic-sparkles')),
      menuItem("About", tabName = 'about', icon = icon('user'))
    )
  ),
  dashboardBody(
    tabItems(

# ------------------------------
# Menu Item: Homepage
# ------------------------------
      
      tabItem(
        tabName = 'dashboard',
        fluidPage(
          theme = theme,
          
      # ------------------------------
      # Homepage: Main Panel
      # ------------------------------
          
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
            
      # ------------------------------
      # Homepage: Vertical Panels
      # ------------------------------

            verticalTabsetPanel(
              # style = 'padding: 1em',
              menuSide = 'right',
            
          # ------------------------------
          # Vertical Panel: Ames Housing
          # ------------------------------
            
            verticalTabPanel(
              title = 'Ames Housing Map', box_height = 5,
              icon = icon("house", class = "fa-2x"),
              leafletOutput("map")
            ),
            
          # ------------------------------
          # Vertical Panel: Time Analysis
          # ------------------------------

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
            
          # --------------------------------
          # Vertical Panel: Parameter Tuning
          # --------------------------------
            
            verticalTabPanel(
              title = 'Parameter Tuning', box_height = 5,
              icon = icon('gears', class = "fa-2x"),
              
              # ------------------------------
              # Parameter Tuning: Current Home
              # ------------------------------
              
              fluidRow(
                column(
                  width = 5,
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
                    width = 7,
                    # style='padding:0px',
                    box(
                      width = NULL,
                      title = uiOutput('predicted_price'),
                      status = 'info', 
                      solidHeader = TRUE,                    
                      collapsible = TRUE,
                      
                      
                      # ----------------------------------
                      # Parameter Tuning: Sq. Ft. Slider
                      # ----------------------------------
                      
                      chooseSliderSkin('Flat'),
                      sliderInput(
                        inputId = 'sqft_slider',
                        label = 'Total Sq. Ft',
                        min = 1, max = 2000,
                        value = 1, step = 10,
                        animate =
                          animationOptions(interval = 500, loop = TRUE)),
                      
                      # ------------------------------
                      # Parameter Tuning: Left Column
                      # ------------------------------
                      fluidRow(
                        column(
                          width = 3,
                          uiOutput(outputId = 'bedrooms'),
                          # uiOutput(outputId = 'condition'),
                          uiOutput(outputId = 'quality')
                          ),
                        column(
                          width = 3,
                          # fluidRow(
                          uiOutput(outputId = 'bathrooms'),
                          uiOutput(outputId = 'kitchen')
                        ),
                        column(
                          width = 3,
                          # fluidRow(
                          # uiOutput(outputId = 'quality'),
                          uiOutput(outputId = 'basement')
                        ),
                        column(
                          width = 3,
                          # fluidRow(
                          # uiOutput(outputId = 'quality'),
                          uiOutput(outputId = 'condition')
                        )
                      )
                    )
                  )
                )
              ),
              
          # ------------------------------
          # Vertical Panel: Visualization
          # ------------------------------

            verticalTabPanel(
              box_height = 5,
              title = 'Visualize Prediction', 
              icon = icon("exchange", class = "fa-2x"),
              fluidPage(
                fluidRow(
                infoBoxOutput(
                  width = 6, 
                  outputId = 'predicted'),
                infoBoxOutput(
                  width =6,
                  outputId = 'addedarea'),
              
                plotOutput('scatplot', height = 300)
              )
              ))
          )
        )
      ),
      
# ------------------------------
# Menu Item: Dataset
# ------------------------------

      tabItem(
        tabName = 'dataset',
        fluidPage(
          theme = theme,
          dataTableOutput(
            outputId = 'df', 
            height = "100%")
        )
      ),

# ------------------------------
# Menu Item: About Me
# ------------------------------

      tabItem(
        tabName = 'about',
        theme = theme,
        fluidPage(
          fluidRow(
        # ------------------------------
        # About Me: Laurel He
        # ------------------------------
        
            column(
              width = 4,
              fluidRow(
                box(
                  title = div(
                    a(href = 'https://github.com/LaurelHe1',
                      icon('github')),
                    a(href = 'https://www.linkedin.com/in/cheng-laurel-he-b04a59104/',
                      icon('linkedin'))),
                  width = 12,
                  status = 'primary',
                  boxProfile(
                    image = './img/about_me/laurel.jpg',
                    title = 'Laurel He',
                    subtitle = 'Data Science Fellow',
                    bordered = TRUE,
                    uiOutput(align = 'center',
                             outputId = 'laurel_bio')
                    )
                  )
                )
              ),
        
        # ------------------------------
        # About Me: Daniel Setiawan
        # ------------------------------
            
            column(
              width = 4,
              fluidRow(
                box(
                  title = div(
                    a(href = 'https://github.com/set-one',
                      icon('github')),
                    a(href = 'https://www.linkedin.com/in/danielosetiawan/',
                      icon('linkedin'))),
                  width = 12,
                  status = 'success',
                  boxProfile(
                    image = './img/about_me/daniel_s.jpeg',
                    title = 'Daniel Setiawan',
                    subtitle = 'Data Science Fellow',
                    bordered = TRUE,
                    uiOutput(align = 'center',
                             outputId = 'daniels_bio')
                  )
                )
              )
            ),
        
        # ------------------------------
        # About Me: Daniel Erickson
        # ------------------------------
        
            column(
              width = 4,
              fluidRow(
                box(
                  title = div(
                    a(href = 'https://github.com/LaurelHe1',
                      icon('github')),
                    a(href = 'https://www.linkedin.com/in/cheng-laurel-he-b04a59104/',
                      icon('linkedin'))),
                  width = 12,
                  status = 'warning',
                  boxProfile(
                    image = './img/about_me/laurel.jpg',
                    title = 'Daniel Erickson',
                    subtitle = 'Data Science Fellow',
                    bordered = TRUE,
                    uiOutput(align = 'center',
                             outputId = 'daniele_bio')
                  )
                )
              )
            )
          )
        )
      )
# ------------------------------
# END
# ------------------------------
    )
  )
)
