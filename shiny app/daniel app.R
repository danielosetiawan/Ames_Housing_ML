library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(shiny)
library(sf)
library(tigris)
library(mapview)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
# library(reticulate)

# sns = import('seaborn')
# pd = import('pandas')
# plt = import('matplotlib.pyplot')
# sm = import('statsmodels.api')

location = read.csv("./../data/Ames_loc.csv")
location = na.omit(location)
df = read.csv('./../data/cleaned_housing.csv')
housing = merge(df, location, by="PID")


# 
# # Define UI for application

ui <- dashboardPage(
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
          "neighbor_ts", "Neighborhood",
          choices = c('All Neighborhoods', unique(df$Neighborhood)),
          selected = 'Neighborhood',
          width = '28%'),
        sliderInput('Budget', 'Budget',
                    min = min(df$SalePrice), 
                    max = max(df$SalePrice),
                    value = c(0.25 * max(df$SalePrice), 
                              0.75 * max(df$SalePrice))),
        plotOutput('timeseries')
      )
    )
  )
)


source_python('timeseries.py')
server <- function(input, output, session) {
  
  
############## home flipping #########

  output$timeseries = renderPlot(
  expr = all_neighborhoods()
    )
  
  observeEvent(input$neighbor_ts, {
    min_price = min(df_neighborhood()$SalePrice)
    max_price = max(df_neighborhood()$SalePrice)
    mid_price = max_price - min_price
    first_qtl = min_price + 0.25 * mid_price
    third_qtl = max_price - 0.25 * mid_price
    
    updateSliderInput(
      session,
      inputId = 'Budget',
      min = min_price,
      max = max_price,
      value = c(first_qtl, third_qtl)
    )
  })
  
  df_neighborhood = reactive({
    if (input$neighbor_ts == 'All Neighborhoods') {
      df
    } else {
      df %>%
        filter(Neighborhood == input$neighbor_ts)
    }})
    
  

}

shinyApp(ui, server)

