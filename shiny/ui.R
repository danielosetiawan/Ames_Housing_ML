library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(bs4Dash)
library(bslib)
library(thematic)
library(waiter)

css = HTML("
  .vbox1 .small-box {
    background-color: #FFFFFF !important;
  }
  .vbox2 .small-box {
    background-color: #F6FC00 !important;
  }
  .vbox3 .small-box {
    background-color: #D20000 !important;
  }
  .vbox4 .small-box {
    background-color: #D20000 !important;
  }
")


thematic_shiny()

# # Define UI for application
spinner = 'double-bounce'
# theme = 'Minty'

dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), 
                   color = "#343a40"),
  dark = TRUE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  dashboardHeader(
    'Dashboard: Ames, IA'
  ),
  dashboardSidebar(
    disable = TRUE
    ),
  dashboardBody(
# ------------------------------
# Menu Item: Homepage
# ------------------------------
      
        fluidPage(
          # theme = theme,
          
      # ------------------------------
      # Homepage: Main Panel
      # ------------------------------
          
        fluidRow(
          useShinyjs(),
          uiOutput('pointer'),
          
          valueBoxOutput('valuebox1', width=3),
          valueBoxOutput('valuebox2', width=3),
          valueBoxOutput('valuebox3', width=3),
          valueBoxOutput('valuebox4', width=3),
        ),
      
        
      bs4TabCard(
        id = "tabcard",
        side='left',
        title = "",
        selected = 'Search',
        collapsible = FALSE,
        width = 12,
        tabPanel(
          title = 'Search',
            width = 12,
          
            fluidRow(
              column(
                width = 6,
                leafletOutput('map', height=400) #278
                ),
              column(
                width = 6,
                  fluidRow(
                    style='margin-top: -20px',
                  column(
                    width = 6,
                    tags$p('Neighborhood', style='font-size: 80%; 
                           font-style: italic; margin-left: 0px; margin-top: 0px;
                           margin-bottom: -20px'),
                    pickerInput(
                      inputId = 'neighborhood', 
                      label = tags$p('', style='margin-top: -80px'),
                      choices = c('All Neighborhoods', 
                                  unique(df_predictions$Neighborhood)),
                      selected = 'Neighborhood'
                    ),
                    fluidRow(
                      tags$head(
                        tags$style(
                          ".fa { color: black}; .fa-3x {font-size: 300%}"
                        )
                      ),
                    column(
                      width = 6,
                    
                    infoBoxOutput(
                      'income', 
                      width = NULL
                      )
                    ),
                    column(
                      width = 6,
                      infoBoxOutput(
                        'appreciation', 
                        width = NULL
                      )
                    ),
                    column(
                      width = 12,
                    
                    infoBoxOutput(
                      'nb_info', 
                      width = NULL
                    ))
                    ),
                    

                    
                    
                  ),
                  column(
                    width = 6,
                    style = 'margin-top: -2px',
                    fluidRow(
                    column(
                      width = 6,
                      tags$p('Property:', style='font-size: 80%; 
                             font-style: italic; margin-left: 0px; margin-top: 0px;
                             margin-bottom: -25px')
                    ),
                    column(
                      width = 6,
                      style = 'text-align: right',
                      uiOutput('property_label')
                    )
                    ),
                    pickerInput(
                      inputId = 'address',
                      label = tags$p('', style='margin-top: -95px; 
                                     margin-bottom: 5px'),
                      choices = df_predictions$Prop_Addr,
                      options = pickerOptions(
                        `live-search` = TRUE,
                        virtualScroll = TRUE,
                        dropupAuto = FALSE)
                    ),
                    valueBoxOutput('property', width=NULL),
                    
                  )
                )
              )
            )
          ),
        tabPanel(
          title = 'Compare',
            fluidRow(
              column(5,
                     pickerInput(
                       inputId = 'compare_1',
                       label = 'Property #1',
                       choices = df_predictions$Prop_Addr,
                       selected = '209 HARTFORD DR',
                       options = pickerOptions(
                         `live-search` = TRUE,
                         virtualScroll = TRUE,
                         dropupAuto = FALSE)
                     ),
                     leafletOutput('map1', height=300), #288
              ),
              column(2,
                     style = "text-align:center;",
                     checkboxGroupButtons(
                       inputId = "check_box",
                       label = "",
                       
                       choices = list("Shops" = 1, 
                                   "Restaurants" = 2, 
                                   "Parks" = 3,
                                   "Church" = 4),
                       direction = 'vertical',
                       status = "info",
                       checkIcon = list(
                         yes = icon("ok", 
                                    lib = "glyphicon"))
                     )
              ),
              column(5,
                     pickerInput(
                       inputId = 'compare_2',
                       label = 'Property #2',
                       choices = df_predictions$Prop_Addr,
                       selected = '2527 CLAYTON DR',
                       options = pickerOptions(
                         `live-search` = TRUE,
                         virtualScroll = TRUE,
                         dropupAuto = FALSE)
                     ),
                     leafletOutput('map2', height=300) #288
                )
              
            )
        ),
        
        
        # ------------------------------
        # Forecast
        # ------------------------------
        
        # fluidPage(
        #   fluidRow(
        #     column(12,
        #            "Fluid 12",
        #            fluidRow(
        #              column(6,
        #                     "Fluid 6",
        #                     fluidRow(
        #                       column(6,
        #                              "Fluid 6"),
        #                       column(6,
        #                              "Fluid 6")
        #                     )
        #              ),
        #              column(width = 6,
        #                     "Fluid 6")
        #            )
        #     )
        #   )
        # )
        
        
        tabPanel(
          title = 'Forecast',
          fluidRow(
            column(8,
                   uiOutput(outputId = "sarima"),
                   ),
            column(4,
                   fluidRow(
            column(6, 
                   style = 'margin-top: -50px',
                   materialSwitch(
                         status = 'success',
                         inputId = 'ts_ci',
                         label = "Confidence Interval",
                         value = TRUE,
                         right = TRUE
                             ),
                   ),
            column(6, 
                   style = 'margin-top: -80px',
                   pickerInput(
                     inputId = 'neighborhood_timeseries',
                     label = '',
                     choices = c('All Neighborhoods',
                                 unique(df_predictions$Neighborhood)),
                     selected = 'Neighborhood'
                   ),
                   
                   ),
            fluidRow(
              
              bs4TabCard(
                side='left',
                title = "",
                collapsible = FALSE,
                width=12,
                selected = 'Info',
                tabPanel(
                  title='Info',
                  uiOutput(outputId = "info"),
                ),
                tabPanel(
                  title='Trend',
                  uiOutput(outputId = "trend"),
                ),
                tabPanel(
                  title='Seasonality',
                  uiOutput(outputId = "season"),
                )

              
              # uiOutput(outputId = "trend"),
            ))
            )
            
            
          )
          )

        ),
          

        tabPanel(
          title = 'Flip',
          box(
            solidHeader = FALSE,
            collapsible = FALSE,
            title = uiOutput('current_price'),
            background = NULL,
            width = 12,
            # status = "danger",
            uiOutput('predicted_price'),
            
            tags$p('', style='margin-top:-30px; 
                   font-size: 12px; margin-bottom: -100px; margin-top: -10px;'
                   ),
            footer = fluidRow(
              column(
                width = 6,
                boxPad(
                  color = 'info',
                  gradient = TRUE,
                  # width = 12,
                  # id = 'flip2',
                  tags$b('How did we come to our conclusion?'),
                  tags$br(),tags$br(),
                  uiOutput('conclusion'),
                  splitLayout(
                    cellWidths = c("55%", "45%"),
                    div(tableOutput("table"), style = "font-size: 75%; width: 75%"),
                    div(plotOutput("rank_bar")), 
                    style = "height: 280px;"
                  )
                )
              ),
              
              column(
                width = 6,
                style='margin-top: -25px',
                fluidRow(
              column(
                width = 4,
                offset = 2,
                style = 'margin-top: -70px;margin-bottom: 0px;',
                tags$p('Neighborhood', style='margin-top: 5px; margin-bottom: -20px;
                  font-style: italic;font-size: 10px;'),
                pickerInput(
                  inputId = 'neighborhood_flip', 
                  label = '',
                  choices = c('All Neighborhoods', 
                              unique(df_predictions$Neighborhood)),
                  selected = 'Neighborhood'
                )
              ),
              column(
                width = 6,
                style = 'margin-top: -70px;margin-bottom: 0px;',
                tags$p('Property (ranked by most undervalued)', style='margin-top: 5px; margin-bottom: -20px;
                  font-style: italic;font-size: 10px;'),
                pickerInput(
                  inputId = 'address_flip', 
                  label = '',
                  choices = df_predictions$Prop_Addr
                ),
              )
              ),

                boxPad(
                  chooseSliderSkin('Flat', color = "#112446"),
                  column(
                    offset = 6,
                    width = 6,
                  tags$p('Gross Square Footage', style='margin-bottom: -40px; font-style: italic;font-size: 12px; font-weight: bold')
                         ),
                  sliderInput(
                    inputId = 'sqft_slider',
                    label = '',
                    min = 1, max = 3000,
                    value = 1, step = 10,
                    animate =
                      animationOptions(interval = 550, loop = TRUE)
                  ),
                  fluidRow(
                column(
                  width = 9,
                  fluidRow(
                  column(
                    width = 4, 
                    align = 'center',
                    style = 'margin-right: -10px; margin-left: -20px; margin-top: -30px;',
                  uiOutput(outputId = 'bedrooms'),
                  tags$p('BEDS', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px;'),
                  uiOutput(outputId = 'kitchen'),
                  tags$p('KITCHEN', style='font-size: 12px; margin-top: -20px;')
                  ),
                  column(
                    width = 4,
                    align = 'center',
                    style = 'margin-right: -10px;margin-top: -30px;',
                  uiOutput(outputId = 'bathrooms'),
                  tags$p('BATHS', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px;'),
                  uiOutput(outputId = 'basement'),
                  tags$p('BASEMENT', style='font-size: 12px; margin-top: -20px;')
                  ),
                  column(
                    width = 4,
                    align = 'center',
                    style = 'margin-right: -10px;margin-top: -30px;',
                  uiOutput(outputId = 'quality'),
                  tags$p('QUALITY', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px'),
                  uiOutput(outputId = 'garage'),
                  tags$p('GARAGE', style='font-size: 12px; margin-top: -20px;')
                  ),
                  )
                    
                  ),
                
                column(
                  width = 3,
                  style='margin-top: -25px; margin-left: -25px;',
                  uiOutput('months', inline=TRUE),
                  tags$p('SELL MONTH', style='margin-bottom: -40px; 
                         font-size: 12px'),
                
                  )
                  ),
                  
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              )
            ))
          ),
          tabPanel(
            title = 'Data',
            dataTableOutput(
              outputId = 'df',
              height = "100%")
          ),
          tabPanel(
            title = 'About',
            fluidRow(
              # ------------------------------
              # About Project
              # ------------------------------
              
              column(
                width = 4,
                fluidRow(
                  box(
                    title = div(
                    ),
                    width = 12,
                    status = 'warning',
                    boxProfile(
                      #image = './img/about_me/.jpg',
                      title = 'About Project',
                      #subtitle = 'Data Science Fellow',
                      bordered = TRUE,
                      uiOutput(align = 'center',
                               outputId = 'about_project')
                      
                    )
                  )
                )
              ),
              
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
              
              # column(
              #   width = 4,
              #   fluidRow(
              #     box(
              #       title = div(
              #         a(href = 'https://github.com/acsuf',
              #           icon('github')),
              #         a(href = 'https://www.linkedin.com/in/daniel-erickson-779943262/',
              #           icon('linkedin'))),
              #       width = 12,
              #       status = 'warning',
              #       boxProfile(
              #         image = './img/about_me/daniel_e.jpg',
              #         title = 'Daniel Erickson',
              #         subtitle = 'Data Science Fellow',
              #         bordered = TRUE,
              #         uiOutput(align = 'center',
              #                  outputId = 'daniele_bio')
              #       )
              #     )
              #   )
              # )
            )
          )
        
      )

        )
      ),
)
