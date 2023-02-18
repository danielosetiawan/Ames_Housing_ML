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
  # scrollToTop = TRUE,
  
  dashboardHeader(
    rightUi = rightUi
    
    
    ),
  dashboardSidebar(
    collapsed = TRUE,
    minified = TRUE,
    expandOnHover = TRUE,
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = FALSE,
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
          
        fluidRow(
          valueBoxOutput('valuebox1', width=3),
          valueBoxOutput('valuebox2', width=3),
          valueBoxOutput('valuebox3', width=3),
          valueBoxOutput('valuebox4', width=3),
        ),
      
        
      bs4TabCard(
        id = "tabcard",
        side='left',
        title = "",
        # selected = 'Compare',
        width = 12,
        tabPanel(
          title = 'Search',
            width = 12,
          
            fluidRow(
              column(
                width = 6,
                leafletOutput('map', height=288)
                ),
              column(
                width = 6,
                  fluidRow(
                    style='margin-top: -35px',
                  column(
                    width = 6,
                    pickerInput(
                      inputId = 'neighborhood', 
                      label = 'Neighborhood',
                      choices = c('All Neighborhoods', 
                                  unique(df_predictions$Neighborhood)),
                      selected = 'Neighborhood'
                    ),
                    infoBoxOutput(
                      'appreciation', 
                      width = NULL
                    ),
                    style = 'margin-bottom: -500px',
                    infoBoxOutput(
                      'income', 
                      width = NULL
                    )
                    
                  ),
                  column(
                    width = 6,
                    pickerInput(
                      inputId = 'address',
                      label = uiOutput(outputId = 'property_value'),
                      choices = df_predictions$Prop_Addr,
                      options = pickerOptions(
                        `live-search` = TRUE,
                        virtualScroll = TRUE,
                        dropupAuto = FALSE)
                    ),
                    boxPad(
                      color = 'lightblue',
                      # title=tagList('Current Home', icon('house')),
                      status = 'primary',
                      solidHeader = TRUE,
                      fluidRow(
                      column(width = 6,
                       uiOutput(outputId = 'house_params')),
                      column(width=6,
                       uiOutput(outputId = 'house_labels'))
                     )
)
                    
                  )
                )
              )
            )
          ),
        tabPanel(
          title = 'Compare',
          fluidRow(
          column(5,
                 div(class = "addrSearch",
                     pickerInput(
                       inputId = 'compare_1',
                       label = 'Property #1',
                       choices = df_predictions$Prop_Addr,
                       options = pickerOptions(
                         `live-search` = TRUE,
                         virtualScroll = TRUE,
                         dropupAuto = FALSE)
                     ),
                     class = "search")),
                 column(5, offset=2,
                        div(class = "addrSearch",
                            pickerInput(
                              inputId = 'compare_2',
                              label = 'Property #2',
                              choices = df_predictions$Prop_Addr,
                              options = pickerOptions(
                                `live-search` = TRUE,
                                virtualScroll = TRUE,
                                dropupAuto = FALSE)
                            ),
                            class = "search")
                        )),
          
                fluidRow(
                  column(5,
                         leafletOutput('map1', height=288),
                  ),
                  column(2,
                         checkboxGroupButtons(
                           # width=NULL,
                           inputId = "Id058",
                           label = "",
                           
                           choices = c("Subways", 
                                       "Restaurants", "Shops", "Storefronts"),
                           # justified = TRUE,
                           direction = 'vertical',
                           checkIcon = list(
                             yes = icon("ok", 
                                        lib = "glyphicon"))
                         )
                  ),
                  column(5,
                         leafletOutput('map2', height=288)
                    )
                  
                )
        ),
        tabPanel(
          title = 'When to buy',
          "Content 3"
        )
      ),  
            
      # ------------------------------
      # Homepage: Tab Panels
      # ------------------------------
            verticalTabsetPanel(
              menuSide = 'right',
            
          # ------------------------------
          # Vertical Panel: Ames Housing
          # ------------------------------
            
            verticalTabPanel(
              title = 'Where', box_height = 5,
              icon = icon("house", class = "fa-2x"),
              # leafletOutput("map", width=NULL)
            ),
            
          # ------------------------------
          # Vertical Panel: Time Analysis
          # ------------------------------

            verticalTabPanel(
              title = 'When', box_height = 5,
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
              title = 'What', box_height = 5,
              icon = icon('gears', class = "fa-2x"),
              
              # ------------------------------
              # Parameter Tuning: Current Home
              # ------------------------------
              fluidPage(
              fluidRow(
                column(
                  width = 5,
                  style='padding:-2px',
                  # box(
                  #   width = NULL,
                  #   title=tagList('Current Home', icon('house')),
                  #   status = 'primary',
                  #   solidHeader = TRUE,
                  #   uiOutput(outputId = 'current_home')
                  # )
                  ),
                
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
                          # uiOutput(outputId = 'quality'),
                          # uiOutput(outputId = 'kitchen'),
                        ),
                        column(
                          width = 4,
                          align = 'center',
                          # uiOutput(outputId = 'condition'),
                          # uiOutput(outputId = 'basement'),
                        ),
                        column(
                        width = 4,
                        align = 'center',
                        # uiOutput(outputId = 'bedrooms'),
                        # uiOutput(outputId = 'bathrooms'),

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
              title = 'How', 
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
  # pinned = TRUE,
  # collapsed = FALSE,
  overlay = FALSE,
  controlbarMenu(
    # sortable(),
    # bucket_list(
    #   header = "Drag the items in any desired bucket",
    #   group_name = "bucket_list_group",
    #   orientation = "horizontal",
    #   add_rank_list(
    #     text = "Drag from here",
    #     labels = list(
    #       "one",
    #       "two",
    #       "three",
    #       htmltools::tags$div(
    #         htmltools::em("Complex"), " html tag without a name"
    #       ),
    #       "five" = htmltools::tags$div(
    #         htmltools::em("Complex"), " html tag with name: 'five'"
    #       )
    #     ),
    #     input_id = "rank_list_1"
    #   ))
    ))
#     # id = "controlbarMenu",
#     # type = "pills",
#     # controlbarItem(
#     # column(width = 10, offset=1,
#     #   # selectInput(
#     #   #   inputId = 'neighborhood',
#     #   #   label = '',
#     #   #   choices = c('All Neighborhoods',
#     #   #               unique(df_predictions$Neighborhood)),
#     #   #   selected = 'Neighborhood'
#     #   #   
#     #   # ),
#     #   # multiInput(
#     #   #   inputId = "address",
#     #   #   label = "Address :", 
#     #   #   choices = NULL,
#     #   #   choiceNames = df_predictions$Prop_Addr,
#     #   #   choiceValues = df_predictions$Prop_Addr
#     #   # )
#     #   
#     #   
#     # )
#     # )
#     # controlbarItem(
#     #   "Skin",
#     #   skinSelector()
#     # )
#   # )
# )
)
