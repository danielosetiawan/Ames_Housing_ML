library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)
library(bslib)
library(thematic)
library(waiter)


thematic_shiny()

# # Define UI for application
spinner = 'double-bounce'
theme = 'Minty'

dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dark = TRUE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  dashboardHeader(
    
    rightUi = tags$li(class = "dropdown",
                      
                      tags$li(
                        class = "dropdown", div(
                          # selectInput(
                          #   inputId = 'neighborhood', 
                          #   label = '',
                          #   choices = c('All Neighborhoods', 
                          #               unique(df_predictions$Neighborhood)),
                          #   selected = 'Neighborhood'
                          # ),
                          style= "width: 60%; margin-left: auto; margin-right: 0px; 
                      margin-top:-20px; margin-bottom:-20px;")),
                      
                      tags$li(class = "dropdown", div(
                        # selectInput(
                        #   inputId = 'address',
                        #   label = '',
                        #   choices = df_predictions$Prop_Addr,
                        # ),
                      style= "width: 90%; margin-left: 10px; margin-right: -80px; 
                      margin-top:-20px; margin-bottom:-20px;"))
                      
                        
                      
                      )
    
    
    ),
  dashboardSidebar(
    collapsed = TRUE,
    minified = TRUE,
    expandOnHover = TRUE,
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Cards"),
      menuItem('Homepage', tabName = 'dashboard', icon = icon('house')),
      menuItem('Dataset', tabName = 'dataset', icon = icon('database')),
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
          
      # tabItem(
      #   tabName = "valueboxes",
      #   h4("Value Boxes"),
        fluidRow(
          valueBoxOutput('crime_rate', width=3),
          valueBoxOutput('school_quality', width=3),
          valueBoxOutput('appreciation', width=3),
          valueBoxOutput('income', width=3),
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
              fluidPage(
              fluidRow(
                column(
                  width = 5,
                  style='padding:-2px',
                  box(
                    width = NULL,
                    title=tagList('Current Home', icon('house')),
                    status = 'primary',
                    solidHeader = TRUE,
                    uiOutput(outputId = 'current_home')
                  )),
                
                # ------------------------------
                # Parameter Tuning: What If...?
                # ------------------------------
                fluidPage(
                column(
                    width = 7,
                    box(
                      width = NULL, 
                      title = uiOutput('predicted_price'),
                      status = 'primary', 
                      solidHeader = TRUE,
                      # collapsible = TRUE,
                      
                      
                      # ----------------------------------
                      # Parameter Tuning: Sq. Ft. Slider
                      # ----------------------------------
                      
                      chooseSliderSkin('Flat', color = "#112446"),
                      sliderInput(
                        inputId = 'sqft_slider',
                        label = 'Gross Living Area (1st floor + 2nd floor)',
                        min = 1, max = 3000,
                        value = 1, step = 10,
                        animate =
                          animationOptions(interval = 550, loop = TRUE)
                        ),
                        column(
                          width = 4,
                          align = 'center',
                          uiOutput(outputId = 'quality'),
                          uiOutput(outputId = 'kitchen'),
                        ),
                        column(
                          width = 4,
                          align = 'center',
                          uiOutput(outputId = 'condition'),
                          uiOutput(outputId = 'basement'),
                        ),
                        column(
                        width = 4,
                        align = 'center',
                        uiOutput(outputId = 'bedrooms'),
                        uiOutput(outputId = 'bathrooms'),

                        )
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
              icon = icon("magnifying-glass", class = "fa-2x"),
              fluidPage(
                fluidRow(
                  column(
                    offset = 1,
                    width = 12,
                valueBoxOutput(
                  outputId = 'predicted'),
                valueBoxOutput(
                  outputId = 'addedarea')
                ),
                plotOutput('scatplot', height = 250)
                )
              )
            )
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
                    a(href = 'https://github.com/acsuf',
                      icon('github')),
                    a(href = 'https://www.linkedin.com/in/daniel-erickson-779943262/',
                      icon('linkedin'))),
                  width = 12,
                  status = 'warning',
                  boxProfile(
                    image = './img/about_me/daniel_e.jpg',
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
# RIGHT CONTROL BAR
# ------------------------------
    )
  ),
dashboardControlbar(
  id = "controlbar",
  skin = "light",
  pinned = TRUE,
  collapsed = FALSE,
  overlay = FALSE,
  # controlbarMenu(
    # id = "controlbarMenu",
    # type = "pills",
    # controlbarItem(
    column(width = 10, offset=1,
      selectInput(
        inputId = 'neighborhood', 
        label = '',
        choices = c('All Neighborhoods', 
                    unique(df_predictions$Neighborhood)),
        selected = 'Neighborhood'
        
      ),
      multiInput(
        inputId = "address",
        label = "Address :", 
        choices = NULL,
        choiceNames = df_predictions$Prop_Addr,
        choiceValues = df_predictions$Prop_Addr
      )
      
      # selectInput(
      #   inputId = 'address',
      #   label = '',
      #   choices = df_predictions$Prop_Addr,
      # )
    )
    # )
    # controlbarItem(
    #   "Skin",
    #   skinSelector()
    # )
  # )
)
)
