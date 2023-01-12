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

location = read.csv("Ames_loc.csv")
location = na.omit(location)
df = read.csv('cleaned_housing.csv')
housing = merge(df, location, by="PID")

# 
# # Define UI for application
# ui <- dashboardPage(
#   # Application title
#   dashboardHeader(title = "Ames Housing Analysis"),
#   # Sidebar content
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Homepage", tabName = 'homepage', icon = icon('landmark')),
#       menuItem("Dream Home", tabName = 'dreamhome', icon = icon('house')),
#       menuItem("Home Flipping", tabName = 'flipping', icon = icon('wand-magic-sparkles')),
#       menuItem("Machine Learning", tabName = 'ml', icon = icon('gears')),
#       menuItem("About", tabName = 'about', icon = icon('user'))
#     )
#   ),
#   # Body content
#   dashboardBody(
#     tabItems(
#       # First tab content
#       tabItem(tabName = 'hompage',
#               fluidRow(
#                 box(width = 12, leafletOutput(outputId ="map", height="600px"))
#                 #column(width = 12, withSpinner(leafletOutput(outputId ="map", height="600px"),size=2, color="#0080b7"))
#               )
#               )
#               # fluidRow(
#               #   box(width = 12, leafletOutput(outputId ="map", height="600px") %>% withSpinner(size=2, color="#0080b7"))
#               #          ))
#               # )
#               )
#       # Second tab content
#       # tabItem(tabName = 'dreamhome'), 
#       # Third tab content
#       # tabItem(tabName = 'flipping'),
#       # Fourth tab content
#       # tabItem(tabName = 'ml'),
#       # Fifth tab content
#       # tabItem(tabName = 'about')
#     )
# )
# 
# # Define server logic 
# server <- function(input, output, session) {
#   # Create map
#   map = createLeafletMap(session, 'map')
#   session$onFlushed(once=T, function(){
#     output$map = renderLeaflet({
#       leaflet(housing) %>%
#         addTiles() %>%
#         addCircleMarkers(
#            lng = ~Longitude, lat = ~Latitude, color = "red", radius = 0.5, layerId = ~PID
#         )
#    })
#   })  
#   
#   # Map marker click popup to show site info
#   observe({
#         click <- input$map_marker_click
#         if (is.null(click))
#           return()
#         data = housing[housing$PID == click$id,]
#         # data = housing %>% filter(Longitude == click$lng & Latitude == click$lat)
#         content <- as.character(tagList(
#           sprintf("Sale Price: $ %s", data$SalePrice), tags$br(),
#           sprintf("Neighborhood: %s", data$Neighborhood), tags$br(),
#           sprintf("Living Area: %s sq ft", data$GrLivArea), tags$br(),
#           sprintf("Overall Condition: %s", data$OverallCond)
#         ))
#         leafletProxy(mapId = "map") %>% 
#           addPopups(lng = click$lng, lat= click$lat, popup = content, layerId = click$id)
#       })
# 
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

# fluidPage version worked, couldn't get map to display in the dashboard version, feel free to play around with it

# Define UI for application
ui <- fluidPage(
    navbarPage("Ames Housing Analysis", id = 'nav',
               tabPanel("Homepage",
                        column(width = 8, withSpinner(leafletOutput(outputId ="map", height="600px"),size=2, color="#0080b7"))
               ),
               tabPanel("Machine Learning"
               )
    )
)
# Define server logic 
server <- function(input, output, session) {
  # Create map
  map = createLeafletMap(session, 'map')
  session$onFlushed(once=T, function(){
    output$map = renderLeaflet({
      leaflet(housing) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude, color = "red", radius = 0.5, layerId = ~PID
        )
    })
  })  
  
  # Map marker click popup to show site info
  observe({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    data = housing[housing$PID == click$id,]
    # data = housing %>% filter(Longitude == click$lng & Latitude == click$lat)
    content <- as.character(tagList(
      sprintf("Sale Price: $ %s", data$SalePrice), tags$br(),
      sprintf("Neighborhood: %s", data$Neighborhood), tags$br(),
      sprintf("Living Area: %s sq ft", data$GrLivArea), tags$br(),
      sprintf("Overall Condition: %s", data$OverallCond)
    ))
    leafletProxy(mapId = "map") %>% 
      addPopups(lng = click$lng, lat= click$lat, popup = content, layerId = click$id)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

