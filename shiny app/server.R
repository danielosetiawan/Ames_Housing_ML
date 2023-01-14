# ML Ames
# backend code

function(input, output, session) {
  
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

