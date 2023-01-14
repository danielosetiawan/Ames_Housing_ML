library(shiny)
library(shinydashboard)

# # Define UI for application

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
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
        
          selectInput(
          inputId = 'neighborhood', 
          label = 'Neighborhood',
          choices = c('All Neighborhoods', unique(df$Neighborhood)),
          selected = 'Neighborhood',
          width = '28%'),
        sliderInput(inputId = 'budget', 
                    label = 'Budget',
                    min = min(df$SalePrice), 
                    max = max(df$SalePrice),
                    value = c(2e5, 5e5)),
        tabsetPanel(type = "tabs",
                    tabPanel(
                      'Homes', 
                      pickerInput(
                        inputId = 'undervalued',
                        label = 'Browse all undervalued homes...', 
                        choices = unique(undervalued25$Prop_Addr),
                        choicesOpt = list(
                          subtext = paste0(' Price: $',
                            undervalued25$SalePrice, ' (',
                            undervalued25$Value, '$',
                            undervalued25$Delta, ')')),
                        options = pickerOptions(virtualScroll = TRUE,
                                                dropupAuto = FALSE,
                                                width = '70%')
                      )
                    ),
                    tabPanel(
                      'Time Prediction', verbatimTextOutput("summary")
                    ),
                    tabPanel(
                      '', tableOutput("table")
                    )
        )
      )
    )
  )
)