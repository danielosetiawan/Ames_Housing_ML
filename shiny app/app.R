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

location = na.omit(read.csv("./../data/Ames_loc.csv"))
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
        menuSubItem("Maps", tabName = "landmark1", icon = icon("map")),
        menuSubItem("Maps2?", tabName = "landmark2", icon = icon("map"))
      ),
      menuItem("Dream Home", tabName = 'dreamhome', icon = icon('house')),
      menuItem("Home Flipping", tabName = 'flipping', icon = icon('wand-magic-sparkles')),
      menuItem("Machine Learning", tabName = 'ml', icon = icon('gears')),
      menuItem("About", tabName = 'about', icon = icon('user'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "landmark1",
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px);}"),
        box(
          title = "title goes here",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          leafletOutput("map")
        )
      ),
      tabItem(
        tabName = "landmark2",
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px);}"),
        box(
          title = "title goes here",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          leafletOutput("map2")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  map = createLeafletMap(session, 'map')
  
  # for map1
  session$onFlushed(once=T, function(){
    output$map = renderLeaflet({
      leaflet(housing) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude, 
          color = "red", radius = 0.5, layerId = ~PID
        )
    })
  })
  
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
      addPopups(lng = click$lng, lat= click$lat, 
                popup = content, layerId = click$id)
  })
  
  # for map2, if we decide to have one...
  session$onFlushed(once=T, function(){
    output$map2 = renderLeaflet({
      leaflet(housing) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude, 
          color = "red", radius = 0.5, layerId = ~PID
        )
    })
  })

  
  # was trying to make a hover observe event for map2 but failed
  # good source for leaflet: https://medium.com/ibm-data-ai/capture-and-leverage-mouse-locations-and-clicks-on-leaflet-map-6d8601e466a5
}

shinyApp(ui, server)

