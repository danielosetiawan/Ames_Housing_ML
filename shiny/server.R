# ----------------------------------------------------------------

# Backend for Ames Housing Project

# ----------------------------------------------------------------

function(input, output, session) {
  
  observe({
    
    # ----------------------------------------------------------------
    # Vertical Panel: Ames Housing Map
    # ----------------------------------------------------------------
    
    session$onFlushed(once=T, function(){
      
      map1 = createLeafletMap(session, 'compare_map1')
      
      map = createLeafletMap(session, 'map')
      # named list of circle markers
      markers = iconList(
        "Undervalued" = makeIcon(
          './icons/icons8-green-circle-48.png',
          iconWidth = 10,
          iconHeight = 10
        ),
        "Fair" = makeIcon(
          './icons/icons8-orange-circle-48.png',
          iconWidth = 10,
          iconHeight = 10
        ),
        "Overvalued" = makeIcon(
          './icons/icons8-red-circle-48.png',
          iconWidth = 10,
          iconHeight = 10
        )
      )
      # named list of map icons
      logos = iconList(
        "Park" = makeIcon(
          './icons/icons8-evergreen-30.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Store" = makeIcon(
          './icons/icons8-grocery-store-30.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Restaurant" = makeIcon(
          './icons/icons8-dining-50.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Church" = makeIcon(
          './icons/icons8-church-32.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Hotel" = makeIcon(
          './icons/icons8-4-star-hotel-32.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "School" = makeIcon(
          './icons/icons8-school-building-30.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Hospital" = makeIcon(
          './icons/icons8-hospital-sign-30.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Airport" = makeIcon(
          './icons/icons8-airport-30.png',
          iconWidth = 15,
          iconHeight = 20
        ),
        "Library" = makeIcon(
          './icons/icons8-library-building-50.png',
          iconWidth = 15,
          iconHeight = 20
        )
      )
      
      # main map
      session$onFlushed(once=T, function(){
        observe({
          
          if (input$neighborhood == 'All Neighborhoods') {
            output$map = renderLeaflet({
              factpal = colorFactor(
                c('orange', 'red', 'green'),
                df_predictions$Value
              )
              
              undervalue = df_predictions[df_predictions$Value == 'undervalued', ]
              fair = df_predictions[df_predictions$Value == 'fair price', ]
              overvalue = df_predictions[df_predictions$Value == 'overvalued', ]
              
              leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
                setView(lng = '-93.651915', lat = '42.030781', zoom = 12) %>%
                addTiles() %>% 
                addProviderTiles('CartoDB') %>%
                addProviderTiles('Stamen.TonerLines') %>% 
                addMarkers(data = undervalue, lng = ~Longitude, lat = ~Latitude,
                           icon = markers['Undervalued'], layerId = ~PID, label = glue(
                             "<b>Sale Price: $ </b>{format(undervalue$SalePrice, big.mark = ',')}</br>",
                             "<b>Address: </b> {undervalue$Prop_Addr}<br/>",
                             "<b>Neighborhood: </b> {undervalue$Neighborhood}<br/>",
                             "<b>Gross Living Area: sq. ft. </b> {format(undervalue$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                           group = 'Housing') %>%
                addMarkers(data = fair, lng = ~Longitude, lat = ~Latitude,
                           icon = markers['Fair'], layerId = ~PID, label = glue(
                             "<b>Sale Price: $ </b>{format(fair$SalePrice, big.mark = ',')}</br>",
                             "<b>Address: </b> {fair$Prop_Addr}<br/>",
                             "<b>Neighborhood: </b> {fair$Neighborhood}<br/>",
                             "<b>Gross Living Area: sq. ft. </b> {format(fair$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                           group = 'Housing') %>%
                addMarkers(data = overvalue, lng = ~Longitude, lat = ~Latitude,
                           icon = markers['Overvalued'], layerId = ~PID, label = glue(
                             "<b>Sale Price: $ </b>{format(overvalue$SalePrice, big.mark = ',')}</br>",
                             "<b>Address: </b> {overvalue$Prop_Addr}<br/>",
                             "<b>Neighborhood: </b> {overvalue$Neighborhood}<br/>",
                             "<b>Gross Living Area: sq. ft. </b> {format(overvalue$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                           group = 'Housing') %>%
                addLegend(
                  position = "bottomleft",
                  pal = factpal,
                  values = df_predictions$Value
                ) %>%
                addMarkers(data = df_places[df_places$Type == 'Park', ], icon = logos['Park'], lng = ~Longitude, 
                           lat = ~Latitude, group = 'Park') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Store', ], icon = logos['Store'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Store') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Restaurant', ], icon = logos['Restaurant'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Restaurant') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Church', ], icon = logos['Church'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Church') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Hotel', ], icon = logos['Hotel'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Hotel') %>%
                
                addMarkers(data = df_places[df_places$Type == 'School', ], icon = logos['School'], lng = ~Longitude,
                           lat = ~Latitude, group = 'School') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Hospital', ], icon = logos['Hospital'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Hospital') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Airport', ], icon = logos['Airport'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Airport') %>%
                
                addMarkers(data = df_places[df_places$Type == 'Library', ], icon = logos['Library'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Library') %>%
                addProviderTiles('CartoDB.Positron') %>% 
                
                addLayersControl(
                  overlayGroups = c('Housing', 'Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'),  
                  options = layersControlOptions(collapsed = TRUE)
                ) %>%
                hideGroup(c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'))
            })
          }
          else {
            house = df_predictions[df_predictions$Neighborhood == input$neighborhood, ]
            places = df_places[df_places$Neighborhood == input$neighborhood, ]
            
            undervalue = house[house$Value == 'undervalued', ]
            fair = house[house$Value == 'fair price', ]
            overvalue = house[house$Value == 'overvalued', ]
            
            output$map = renderLeaflet({
              factpal = colorFactor(
                c('orange', 'red', 'green'),
                df_predictions$Value)
              m = leaflet(options = leafletOptions(attributionControl=FALSE)) %>% addTiles() 
              if ( dim(undervalue)[1] != 0) {
                m = m %>% 
                  addMarkers(data = undervalue, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Undervalued'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b> {format(undervalue$SalePrice, big.mark = ',')}</br>",
                               "<b>Address: </b> {undervalue$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {undervalue$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {format(undervalue$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing')
              }
              if (dim(fair)[1] != 0) {
                m = m %>%
                  addMarkers(data = fair, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Fair'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{format(fair$SalePrice, big.mark = ',')}</br>",
                               "<b>Address: </b> {fair$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {fair$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {format(fair$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing')
              }
              if (dim(overvalue)[1] != 0) {
                m = m %>%
                  addMarkers(data = overvalue, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Overvalued'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{format(overvalue$SalePrice, big.mark = ',')}</br>",
                               "<b>Address: </b> {overvalue$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {overvalue$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {format(overvalue$GrLivArea, big.mark = ',')}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing') 
              }
              
              m %>% addLegend(
                position = "bottomleft",
                pal =  factpal,
                values = house$Value
              ) %>%
                addMarkers(data = places[places$Type == 'Park', ], icon = logos['Park'], lng = ~Longitude, 
                           lat = ~Latitude, group = 'Park') %>%
                
                addMarkers(data = places[places$Type == 'Store', ], icon = logos['Store'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Store') %>%
                
                addMarkers(data = places[places$Type == 'Restaurant', ], icon = logos['Restaurant'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Restaurant') %>%
                
                addMarkers(data = places[places$Type == 'Church', ], icon = logos['Church'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Church') %>%
                
                addMarkers(data = places[places$Type == 'Hotel', ], icon = logos['Hotel'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Hotel') %>%
                
                addMarkers(data = places[places$Type == 'School', ], icon = logos['School'], lng = ~Longitude,
                           lat = ~Latitude, group = 'School') %>%
                
                addMarkers(data = places[places$Type == 'Hospital', ], icon = logos['Hospital'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Hospital') %>%
                
                addMarkers(data = places[places$Type == 'Airport', ], icon = logos['Airport'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Airport') %>%
                
                addMarkers(data = places[places$Type == 'Library', ], icon = logos['Library'], lng = ~Longitude,
                           lat = ~Latitude, group = 'Library') %>%
                addProviderTiles('CartoDB.Positron') %>% 
                
                addLayersControl(
                  overlayGroups = c('Housing', 'Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'),  
                  options = layersControlOptions(collapsed = TRUE)
                ) %>%
                hideGroup(c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'))
            })
          }
        })
      })
      
      # compare-map1
      session$onFlushed(once=T, function(){
        observe({
          df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_1,]
          NB = df_NB$Neighborhood
          places = df_places[df_places$Neighborhood == NB, ]
          output$map1 = renderLeaflet({
            m = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
            addMarkers(data = df_predictions[df_predictions$Prop_Addr == input$compare_1,], lng = ~Longitude,
                                                                                                 lat = ~Latitude) 
            if (1 %in% input$check_box) {
              m = m %>% addMarkers(data = places[places$Type == 'Store', ], icon = logos['Store'], lng = ~Longitude,
                                   lat = ~Latitude, group = 'Store')
            }

            if (2 %in% input$check_box) { 
              m = m %>% addMarkers(data = places[places$Type == 'Restaurant', ], icon = logos['Restaurant'], lng = ~Longitude,
                         lat = ~Latitude, group = 'Restaurant')
            }
            if (3 %in% input$check_box) {
              m = m %>% addMarkers(data = places[places$Type == 'Park', ], icon = logos['Park'], lng = ~Longitude, 
                         lat = ~Latitude, group = 'Park') 
            }
            if (4 %in% input$check_box) {
              m = m %>% addMarkers(data = places[places$Type == 'Church', ], icon = logos['Church'], lng = ~Longitude, 
                                   lat = ~Latitude, group = 'Church') 
            }
            m %>% addProviderTiles(providers$Thunderforest.OpenCycleMap)
              # 
              # addLayersControl(
              #   overlayGroups = c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'),  
              #   options = layersControlOptions(collapsed = TRUE)
              # ) %>%
              # hideGroup(c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'))

          })
        })
      })
      
      # compare-map2
       session$onFlushed(once=T, function(){
         observe({
           df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_2,]
           NB = df_NB$Neighborhood
           places = df_places[df_places$Neighborhood == NB, ]
           output$map2 = renderLeaflet({
               m = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
                 addMarkers(data = df_predictions[df_predictions$Prop_Addr == input$compare_2,], lng = ~Longitude,
                            lat = ~Latitude) 
               if (1 %in% input$check_box) {
                 m = m %>% addMarkers(data = places[places$Type == 'Store', ], icon = logos['Store'], lng = ~Longitude,
                                      lat = ~Latitude, group = 'Store')
               }
               
               if (2 %in% input$check_box) { 
                 m = m %>% addMarkers(data = places[places$Type == 'Restaurant', ], icon = logos['Restaurant'], lng = ~Longitude,
                                      lat = ~Latitude, group = 'Restaurant')
               }
               if (3 %in% input$check_box) {
                 m = m %>% addMarkers(data = places[places$Type == 'Park', ], icon = logos['Park'], lng = ~Longitude, 
                                      lat = ~Latitude, group = 'Park') 
               }
               if (4 %in% input$check_box) {
                 m = m %>% addMarkers(data = places[places$Type == 'Church', ], icon = logos['Church'], lng = ~Longitude, 
                                      lat = ~Latitude, group = 'Church') 
               }
               m %>% addProviderTiles(providers$Thunderforest.OpenCycleMap)
               # 
               # addLayersControl(
               #   overlayGroups = c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'),  
               #   options = layersControlOptions(collapsed = TRUE)
               # ) %>%
               # hideGroup(c('Park', 'Store', 'Restaurant', 'Church', 'Hotel', 'School', 'Hospital', 'Airport','Library'))

           })
       })
     })

    })
    

    # ---------------------------
    # Vertical Panel: Timeseries
    # ---------------------------
    
    observeEvent(input$neighborhood_timeseries, {
    observeEvent(input$ts_ci, {
      
      if (input$neighborhood_timeseries == 'All Neighborhoods') {
        sarima_model = ifelse(
          input$ts_ci == TRUE, 
          './img/ts_with_ci/ames_sarima_prediction.png', 
          './img/ts_without_ci/ames_sarima_prediction_noci.png'
        )
        
        model_trend = './img/trends/All_Neighborhoods.png'
        model_season = './img/season/All_Neighborhoods.png'
        
      } else {
        sarima_model = ifelse(
          input$ts_ci == TRUE, 
          paste0('./img/ts_with_ci/', input$neighborhood_timeseries, '_sarima_prediction.png'),
          paste0('./img/ts_without_ci/', input$neighborhood_timeseries, '_sarima_prediction_noci.png')
        )
        
        model_trend = paste0('./img/trends/', input$neighborhood_timeseries, '.png')
        model_season = paste0('./img/season/', input$neighborhood_timeseries, '.png')
      }
      
      output$sarima = renderUI({
        HTML(paste0('<img width = "100%", height = "150%",
                    src= "', sarima_model, '"/>'))
      })
      
      output$trend = renderUI({
        HTML(paste0('<img width = "100%", height = "100%",
                    src= "', model_trend, '"/>'))
      })
      
      output$season = renderUI({
        HTML(paste0('<img width = "100%", height = "100%",
                    src= "', model_season, '"/>'))
      })
      
      output$info = renderUI({
        info = case_when(
          input$neighborhood_timeseries == 'All Neighborhoods' ~ all_nb,
          input$neighborhood_timeseries == 'IDOTRR' ~ IDOTTR, 
          input$neighborhood_timeseries == 'NAmes' ~ NAmes,
          input$neighborhood_timeseries == 'Gilbert' ~ Gilbert,
          input$neighborhood_timeseries == 'StoneBr' ~ StoneBr,
          input$neighborhood_timeseries == 'NWAmes' ~ NWAmes, 
          input$neighborhood_timeseries == 'Somerst' ~ Somerst,
          input$neighborhood_timeseries == 'BrDale' ~ BrDale,
          input$neighborhood_timeseries == 'NPkVill' ~ NPkVill,
          input$neighborhood_timeseries == 'NridgHt' ~ NridgHt,
          input$neighborhood_timeseries == 'Blmngtn' ~ Blmngtn,
          input$neighborhood_timeseries == 'NoRidge' ~ NoRidge,
          input$neighborhood_timeseries == 'SawyerW' ~ SawyerW,
          input$neighborhood_timeseries == 'Sawyer' ~ Sawyer,
          input$neighborhood_timeseries == 'Veenker' ~ Veenker,
          input$neighborhood_timeseries == 'Greens' ~ Greens,
          input$neighborhood_timeseries == 'BrkSide' ~ BrkSide,
          input$neighborhood_timeseries == 'OldTown' ~ OldTown,
          input$neighborhood_timeseries == 'ClearCr' ~ ClearCr,
          input$neighborhood_timeseries == 'Edwards' ~ Edwards,
          input$neighborhood_timeseries == 'SWISU' ~ SWISU,
          input$neighborhood_timeseries == 'CollgCr' ~ CollgCr,
          input$neighborhood_timeseries == 'Crawfor' ~ Crawfor,
          input$neighborhood_timeseries == 'Mitchel' ~ Mitchel,
          input$neighborhood_timeseries == 'Timber' ~ Timber,
          input$neighborhood_timeseries == 'MeadowV' ~ MeadowV
        )

        tags$p(HTML(paste0(
            '<span style="font-size: 15px;"><b>', input$neighborhood_timeseries, '</b>', info, '</span>',
            tags$br(), tags$hr(),
            "<font size='-1'> We analyzed sales price time series data to learn about the best time to sell and buy, and
                  if a certain neighborhood housing price would fair well during a recession.
                  Seasonality directly impacts supply and demand: we found that summer seasons see an influx of demand, increasing
                  costs. After decomposing trend and seasonality from sales price time series, we see that the best month to buy is March,
                  where the best month to sell is January. Ames as a neighborhood faired especially well during the 2008 recession, and among
                  those NWAmes, Mitchel, Collge Creek, Gilbert and Timber are more recession proof than others. By adjusting for differencing,
                  seasonality and trend can help forecast future prices and predict optimal timing to buy and sell more efficiently. </font>"),
            ),
            style = 'margin-top: 5px; font-weight: normal; line-height: 1;
              color: white; text-align: center; margin-bottom: 5px'
          )
      })

    })
    })
    
    # ---------------------
    # Homepage: Main Panel
    # ---------------------
    # observeEvent(input$neighborhood_flip, {
      
      price = format(round(df_neighborhood()$SalePrice/1e3), big.mark = ',')
      
      updatePickerInput(
        session,
        inputId = 'address',
        # label = label,
        # selected = NULL,
        choices = df_neighborhood()$Prop_Addr,
        choicesOpt = list(
          subtext = paste0(
            'Price: $',
            price, 'K'
          )
        )
      )
    
    # observeEvent(input$address, {
      
      updatePickerInput(
        session,
        inputId = 'address_flip',
        # label = label,
        # selected = NULL,
        choices = df_undervalued()$Prop_Addr,
        choicesOpt = list(
          subtext = paste0(
            c('\t\t'), 'Price: $',
            df_undervalued()$SalePrice, ' (',
            df_undervalued()$Value,
            df_undervalued()$Delta, ')'
          )
        )
      )
      
      # observeEvent(input$address_flip, {
      
      # ----------------------
      # Main Panel: Info Boxes
      # ----------------------

      output$property <- renderValueBox({
        
        lot_area = format(df_property()$LotArea, big.mark = ",")
        overall = paste0(df_property()$OverallQual/2, '/5')
        kitchen = ifelse(df_property()$KitchenQual != '-', 
                         paste0(df_property()$KitchenQual, '/5'), '-')
        garage = ifelse(df_property()$GarageQual != '-', 
                        paste0(df_property()$GarageQual, '/5'), '-')
        basement = ifelse(df_property()$BsmtQual != '-', 
                          paste0(df_property()$BsmtQual, '/5'), '-')
        school = paste0(df_property()$school_quality, ' / 10')
        crime = paste0(df_property()$crime_rate, ' / 10')
        sqft = format(df_property()$GrLivArea, big.mark = ',')
        price = paste0('$', format(df_property()$SalePrice, big.mark = ","))
        beds = df_property()$BedroomAbvGr
        baths = df_property()$FullBath + df_property()$HalfBath
        
        if (df_property()$Delta/1e3 <= -5) {
          value = 'UNDERVALUED'
          color = 'lime'
        } else if (df_property()$Delta/1e3 >= 5) {
          value = 'OVERVALUED'
          color = 'red'
        } else {
          value = 'FAIR PRICE'
          color = 'orange'
        }
        
        property_value = paste0(
            '<b><span style="color:', color, '; font-size: 12px;">', value, ' BY ', 
            abs(round(df_property()$Delta/1e3)), 'K</span></b>'
          )
        
        label = HTML(paste0(
          beds,' bds | ',baths,' ba | ',sqft,' sqft.'
        ))

        column_1 = paste0('<b>Overall Quality: </b>', overall,
                          '<br><b>Kitchen Quality: </b>', kitchen,
                          '<br><b>Garage Quality: </b>', garage,
                          '<br><b>Basement Quality: </b>', basement)
              
        column_2 = paste0('<b>Crime Rate: </b>', crime,
                          '<br><b>School Quality: </b>', school,
                          '<br><b>Total Lot Sqft: </b>', lot_area,
                          '<br><b>Year Built: </b>', df_property()$YearBuilt)
        
        extra_label = paste0(
            '<div style="display: flex; flex-wrap: wrap;"><span style="flex-basis: 50%; 
            text-align: center;font-size: 11px">',column_1,'
            </span><span style="flex-basis: 50%; text-align: center; 
            font-size: 11px">', column_2,'</span></div>')
      
        
        valueBox(
          value = tags$p(HTML(paste0('<b><h1>', price, '</b></h1><h6>', property_value, '</h6>')),
            style = "margin-bottom: 20px; margin-left: 2px;
              margin-top:-10px; margin-bottom: 0px;color: black;
              font-size: 20%;  line-height: 0;"),
          subtitle = tags$p(HTML(paste0(
            '<b><h1>', '', '</h1></b><br>', extra_label)), 
            style = "margin-bottom: -20px; color: black;"
          ),
          icon = icon('1'),
          color = 'lightblue',
          footer = tags$p(label, style='text-align: left;color: black;
                          margin-left: 10px; margin-bottom: 0px')
        )
        
        
      })
      
      rv = reactiveValues(tabSelected = 'Search')
      
      observeEvent(input$tabcard, {
        rv$tabSelected = input$tabcard
      })
      
#############################################  
######### UPPER VALUE BOX #1 ################
#############################################
      
      # creating a function to update valueboxes
      update_property <- function(num) {
        observeEvent(input$valueboxes_click, {
          valbox_address <- comps()[num, ]$Prop_Addr
          updatePickerInput(session, "address", selected = valbox_address)
        })

        shinyjs::runjs("
    $(document).on('click', '.small-box', function() {
      Shiny.setInputValue('valueboxes_click', Math.random());
      });
      ")}
      
      
      update_property(1)
      update_property(2)
      update_property(3)
      update_property(4)
      
      output$pointer = renderUI({
        if (rv$tabSelected == 'Search') {
          tags$head(tags$style(HTML('.small-box {cursor: pointer;}')))
        } else {
          tags$head(tags$style(HTML('.small-box {cursor: default;}')))
        }
      })
      
      output$valuebox1 <- renderValueBox({
        
        if (rv$tabSelected == 'Search') {
          num = 1
          distance = round(comps()[num, ]$distance, 2)
          address = comps()[num, ]$Prop_Addr
          sqft = format(comps()[num, ]$GrLivArea, big.mark = ",")
          price = format(comps()[num, ]$SalePrice, big.mark = ",")
          beds = comps()[num, ]$BedroomAbvGr
          baths = comps()[num, ]$FullBath + 0.5*comps()[num, ]$HalfBath
          subtitle = label = HTML(paste0('<b>$',
            price, '<br></b><span style="font-size: 12px; font-style: italic; font-weight: normal">',
            distance, 'mi away</span>'))
          
          label = HTML(paste0(
            beds,' bds | ',baths,' ba | ',sqft,' sqft.'
          ))
          tags$style(".small-box.bg-green")
          valueBox(
            value = tags$p(
              address, 
              style = "margin-bottom: 25px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(
              subtitle,
              style = "font-size: 200%; margin-top: 23px; margin-bottom: -10px; 
              color: black; line-height: 0.5"),
            icon = icon('2'),
            color = 'warning',
            footer = tags$p(label, style='text-align: left;
                          margin-left: 10px; color: black; margin-bottom: 0px')
          )

        } else if (rv$tabSelected == 'Compare') {
          df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_1,]
          NB = df_NB$Neighborhood
          places = df_demo[df_demo$Neighborhood == NB, ]
          label = HTML(paste0(
            '<i>Income</i> : <b>', df_property1()$nb_income, ' / 10</b><br>',
            '<i>Crime Rate</i> : <b>', df_property1()$crime_rate, ' / 10</b><br>',
            '<i>Appreciation</i> : <b>', df_property1()$nb_appreciation, ' / 10</b><br>',
            '<i>School Quality</i> : <b>', df_property1()$school_quality, ' / 10</b><br>',
            '<i>Population</i> : <b>', places$Population, ' </b><br>'))
          info = HTML(paste0('Neighborhood Info: ', NB))
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 0x; color: black;
              margin-top: -10px; color: black;margin-bottom: -17px; font-weight: normal;
              font-size: 80%"),
            value = tags$p(
              '', 
              style = "margin-bottom: -20px; margin-left: 2px;
              margin-top: -14px; margin-bottom: 0px; color: black;
              font-size: 80%; font-style: italic"),
            icon = icon('info'),
            color = 'info',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          margin-left: 10px;color: black; margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Forecast') {
          
          month = '03 - 05'
          label = 'March - May'
          
          valueBox(
            value = tags$p(
              'Best time to buy', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(HTML(paste0(
              '<b>', month, '</b>')), 
              style = "font-size: 200%; margin-bottom: -20px;color: black;"
            ),
            icon = icon('calendar-plus'),
            color = 'olive',
            footer = tags$p(label, style='text-align: left;color: black;
                          margin-left: 10px; margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Flip') {
          label = paste0(df_property_flip()$crime_rate, ' / 10') 
          info = case_when(
            df_property_flip()$crime_rate < 3 ~ "Generally pretty safe!",
            df_property_flip()$crime_rate < 6 ~ "Neighborhood's not too bad...",
            df_property_flip()$crime_rate < 9 ~ 'Just a tad bit dangerous...',
            TRUE ~ 'Yikes!'
          )
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; font-weight: bold;
              font-size: 200%"),
            value = tags$p(
              'Crime Rate', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('handcuffs'), 
            color = 'lightblue',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Data') {
          
          link = 'https://www.neighborhoodscout.com/ia/ames'
          label = HTML('78 features<br>
                        2500 observations<br>
                        28 neighborhoods')
          info = 'Spans from 2008-2011'
          # info = tags$a('Link here', href = link)
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 100%"),
            value = tags$p(
              'Data Summary', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('border-all'), 
            color = 'info',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
          
        } else {
          label = HTML('Read our <br> Blog Story')
          link = 'https://nycdatascience.com/blog/student-works/machine-learning/maximizing-home-flipping-profits-using-ml-techniques/'
          info = tags$a('Blog link here', href = link)
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'Detail summary', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('book'), 
            color = 'warning',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
        }
      })
      
      #############################################  
      ######### UPPER VALUE BOX #2 ################
      #############################################
      
      
      output$valuebox2 <- renderValueBox({
        
        if (rv$tabSelected == 'Search') {
          num = 2
          distance = round(comps()[num, ]$distance, 2)
          address = comps()[num, ]$Prop_Addr
          sqft = format(comps()[num, ]$GrLivArea, big.mark = ",")
          price = format(comps()[num, ]$SalePrice, big.mark = ",")
          beds = comps()[num, ]$BedroomAbvGr
          baths = comps()[num, ]$FullBath + 0.5*comps()[num, ]$HalfBath
          subtitle = label = HTML(paste0('<b>$',
             price, '<br></b><span style="font-size: 12px; font-style: italic; font-weight: normal">',
             distance, 'mi away</span>'))
          
          label = HTML(paste0(
            beds,' bds | ',baths,' ba | ',sqft,' sqft.'
          ))
          
          valueBox(
            value = tags$p(
              address, 
              style = "margin-bottom: 25px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(
              subtitle,
              style = "font-size: 200%; margin-top: 23px; margin-bottom: -10px; 
              color: black; line-height: 0.5"),
            icon = icon('3'),
            color = 'white',
            footer = tags$p(label, style='text-align: left;
                          margin-left: 10px; color: black; margin-bottom: 0px')
          )
        
        } else if (rv$tabSelected == 'Compare') {
          df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_1,]
          NB = df_NB$Neighborhood
          places = df_demo[df_demo$Neighborhood == NB, ]
          label = HTML(paste0(
            '<i>White</i> : <b>', places$White, ' </b><br>',
            '<i>Asian</i> : <b>', places$Asian, ' </b><br>',
            '<i>Black</i> : <b>', places$Black, ' </b><br>',
            '<i>Hispanic</i> : <b>', places$Hispanic, ' </b><br>',
            '<i>Multi-Racial</i> : <b>', places$MultiRacial, ' </b><br>'))
          info = HTML(paste0('Neighborhood Demographics: ', NB))
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; color: black;
              margin-top: -10px; margin-bottom: -17px; font-weight: normal;
              font-size: 80%"),
            value = tags$p(
              '', 
              style = "margin-bottom: -20px; margin-left: 2px;color: black;
              margin-top: -14px; margin-bottom: 0px;
              font-size: 80%; font-style: italic"),
            icon = icon('location-dot'),
            color = 'lightblue',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          margin-left: 10px; color: black;margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Forecast') {
          month = '07 - 09'
          label = 'July - September'
          
          valueBox(
            value = tags$p(
              'Best time to sell', 
              style = "margin-bottom: -28px; margin-left: 2px;color: black;
              margin-top: -8px; margin-bottom: 0px;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(HTML(paste0(
              '<b>', month, '</b>')), 
              style = "font-size: 200%; color: black;margin-bottom: -20px"
            ),
            icon = icon('calendar-minus'),
            color = 'danger',
            footer = tags$p(label, style='text-align: left;color: black;
                          margin-left: 10px; margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Flip') {
          
          label = paste0(df_property_flip()$school_quality, ' / 10')
          info = case_when(
            df_property_flip()$school_quality < 3 ~ "Poor school district",
            df_property_flip()$school_quality < 6 ~ "Average school district",
            df_property_flip()$school_quality < 9 ~ 'Great school district',
            TRUE ~ 'Excellent School District!'
          )
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; font-weight: bold;
              font-size: 200%"),
            value = tags$p(
              'School Quality', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('graduation-cap'), 
            color = 'info',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
        } else if (rv$tabSelected == 'Data') {
          label = HTML('1. Removed Collinearity<br>
                        2. Dummified Nominal<br>
                        3. Standard Scaled')
          info = 'welp...'
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 100%"),
            value = tags$p(
              'Data Preprocessing',
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('soap'), 
            color = 'teal',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
          
        } else {
          label = HTML('Lasso<br>Regression')
          info = 'Train Acc: 92%, CV:10, α:1.00'
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'About our model', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('dumbbell'), 
            color = 'teal',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
        }
      })
      
      #############################################  
      ######### UPPER VALUE BOX #3 ################
      #############################################
      
      output$valuebox3 <- renderValueBox({
        if (rv$tabSelected == 'Search') {
          num = 3
          distance = round(comps()[num, ]$distance, 2)
          address = comps()[num, ]$Prop_Addr
          sqft = format(comps()[num, ]$GrLivArea, big.mark = ",")
          price = format(comps()[num, ]$SalePrice, big.mark = ",")
          beds = comps()[num, ]$BedroomAbvGr
          baths = comps()[num, ]$FullBath + 0.5*comps()[num, ]$HalfBath
          subtitle = label = HTML(paste0('<b>$',
             price, '<br></b><span style="font-size: 12px; font-style: italic; font-weight: normal">',
             distance, 'mi away</span>'))
          
          label = HTML(paste0(
            beds,' bds | ',baths,' ba | ',sqft,' sqft.'
          ))
          
          valueBox(
            value = tags$p(
              address, 
              style = "margin-bottom: 25px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(
              subtitle,
              style = "font-size: 200%; margin-top: 23px; margin-bottom: -10px; 
              color: black; line-height: 0.5"),
            icon = icon('4'),
            color = 'olive',
            footer = tags$p(label, style='text-align: left;
                          margin-left: 10px; color: black; margin-bottom: 0px')
          )
        
        } else if (rv$tabSelected == 'Compare') {
          df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_2,]
          NB = df_NB$Neighborhood
          places = df_demo[df_demo$Neighborhood == NB, ]
          label = HTML(paste0(
            '<i>White</i> : <b>', places$White, ' </b><br>',
            '<i>Asian</i> : <b>', places$Asian, ' </b><br>',
            '<i>Black</i> : <b>', places$Black, ' </b><br>',
            '<i>Hispanic</i> : <b>', places$Hispanic, ' </b><br>',
            '<i>Multi-Racial</i> : <b>', places$MultiRacial, ' </b><br>'))
          info = HTML(paste0('Neighborhood Demographics: ', NB))
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; color: black;
              margin-top: -10px; margin-bottom: -17px; font-weight: normal;
              font-size: 80%"),
            value = tags$p(
              '', 
              style = "margin-bottom: -20px; margin-left: 2px;color: black;
              margin-top: -14px; margin-bottom: 0px;
              font-size: 80%; font-style: italic"),
            icon = icon('location-dot'),
            color = 'orange',
            footer = tags$p(info, style='text-align: left; color: black;font-size: 80%;
                          margin-left: 10px; margin-bottom: 0px')
          )
        } else if (rv$tabSelected == 'Forecast') {
          month = 'Yes'
          label = 'Uptrend detected in 2008'
          
          valueBox(
            value = tags$p(
              'Recession Proof?', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(HTML(paste0(
              '<b>', month, '</b>')), 
              style = "font-size: 200%; margin-bottom: -20px;color: black;"
            ),
            icon = icon('arrow-trend-down'),
            color = 'warning',
            footer = tags$p(label, style='text-align: left;color: black;
                          margin-left: 10px; margin-bottom: 0px')
          )
        } else if (rv$tabSelected == 'Flip') {
          label = paste0(df_property_flip()$nb_income, ' / 10')
          info = case_when(
            df_property_flip()$nb_income < 5 ~ "Negative annual income growth rate",
            df_property_flip()$nb_income == 5 ~ "No change to annual income growth",
            TRUE ~ 'Positive annual income growth rate'
          )
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; font-weight: bold;
              font-size: 200%"),
            value = tags$p(
              'Neighborhood Income', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('sack-dollar'),
            color = 'olive',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
        
        } else if (rv$tabSelected == 'Data') {
          link = 'https://graderdata.s3.amazonaws.com/Machine+Learning+Project+Proposal.zip'
          label = HTML('Ames Housing <br><span style="font-size: 12px; font-style: italic; font-weight: normal">
                       Professor. Dean De Cock</span>')
          info = tags$a('Download here', href = link)
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 21px; margin-bottom: 20px; line-height: 0.5; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'Explore the data', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('database'), 
            color = 'white',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
          
        } else {
          link = 'https://github.com/set-one/MLProject'
          label = HTML('explore the<br>nitty-gritty')
          info = tags$a('Github link here', href=link)
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'See our code to...', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('code'), 
            color = 'white',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
        }
      })
      
      #############################################  
      ######### UPPER VALUE BOX #4 ################
      #############################################
      
      output$valuebox4 <- renderValueBox({
        if (rv$tabSelected == 'Search') {
        
        num = 4
        distance = round(comps()[num, ]$distance, 2)
        address = comps()[num, ]$Prop_Addr
        sqft = format(comps()[num, ]$GrLivArea, big.mark = ",")
        price = format(comps()[num, ]$SalePrice, big.mark = ",")
        beds = comps()[num, ]$BedroomAbvGr
        baths = comps()[num, ]$FullBath + 0.5*comps()[num, ]$HalfBath
        subtitle = label = HTML(paste0('<b>$',
         price, '<br></b><span style="font-size: 12px; font-style: italic; font-weight: normal">',
         distance, 'mi away</span>'))
        
        label = HTML(paste0(
          beds,' bds | ',baths,' ba | ',sqft,' sqft.'
        ))
        
        valueBox(
          value = tags$p(
            address, 
            style = "margin-bottom: 25px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
          subtitle = tags$p(
            subtitle,
            style = "font-size: 200%; margin-top: 23px; margin-bottom: -10px; 
              color: black; line-height: 0.5"),
          icon = icon('5'),
          color = 'pink',
          footer = tags$p(label, style='text-align: left;
                          margin-left: 10px; color: black; margin-bottom: 0px')
        )
        
        
        } else if (rv$tabSelected == 'Compare') {
          df_NB = df_predictions[df_predictions$Prop_Addr == input$compare_2,]
          NB = df_NB$Neighborhood
          places = df_demo[df_demo$Neighborhood == NB, ]
          label = HTML(paste0(
            '<i>Income</i> : <b>', df_property2()$nb_income, ' / 10</b><br>',
            '<i>Crime Rate</i> : <b>', df_property2()$crime_rate, ' / 10</b><br>',
            '<i>Appreciation</i> : <b>', df_property2()$nb_appreciation, ' / 10</b><br>',
            '<i>School Quality</i> : <b>', df_property2 ()$school_quality, ' / 10</b><br>',
            '<i>Population</i> : <b>', places$Population, ' </b><br>'))
          info = HTML(paste0('Neighborhood Info: ', NB))
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px;color: black;
              margin-top: -10px; margin-bottom: -17px; font-weight: normal;
              font-size: 80%"),
            value = tags$p(
              '', 
              style = "margin-bottom: -20px; margin-left: 2px;
              margin-top: -14px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            icon = icon('info'),
            color = 'warning',
            footer = tags$p(info, style='text-align: left; font-size: 80%;color: black;
                          margin-left: 10px; margin-bottom: 0px')
          )
        } else if (rv$tabSelected == 'Forecast') {
          month = '15%'
          label = 'Based on our data...'
          
          valueBox(
            value = tags$p(
              'Average Annual ROI', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black;
              font-size: 80%; font-style: italic"),
            subtitle = tags$p(HTML(paste0(
              '<b>', month, '</b>')), 
              style = "font-size: 200%; margin-bottom: -20px; color: black;"
            ),
            icon = icon('arrow-trend-up'),
            color = 'orange',
            footer = tags$p(label, style='text-align: left;color: black;
                          margin-left: 10px; margin-bottom: 0px')
          )
          
        } else if (rv$tabSelected == 'Flip') {
            label = paste0(df_property_flip()$nb_appreciation, ' / 10')
            info = case_when(
              df_property_flip()$nb_appreciation < 5 ~ "Likely to depreciate in value",
              df_property_flip()$nb_appreciation > 5 ~ "Likely to appreciate in value",
              TRUE ~ 'Likely stable home value'
            )
            
            icon = case_when(
              df_property_flip()$nb_appreciation < 5 ~ 'arrow-trend-down',
              df_property_flip()$nb_appreciation > 5 ~ 'arrow-trend-up',
              TRUE ~ 'arrow-right'
            )
            
            color = case_when(
              df_property_flip()$nb_appreciation < 5 ~ 'warning',
              df_property_flip()$nb_appreciation > 5 ~ 'teal',
              TRUE ~ 'white'
            )
            
            valueBox(
              subtitle = tags$p(
                label, 
                style = "margin-bottom: 0px; margin-left: 2px;color: black;
              margin-top: 0px; margin-bottom: -15px; font-weight: bold;
              font-size: 200%"),
              value = tags$p(
                'Neighborhood Appreciation', 
                style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px; color: black;
              font-size: 80%; font-style: italic"),
              icon = icon(icon),
              color = color,
              footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
            )
        } else if (rv$tabSelected == 'Data') {
          link = 'https://www.neighborhoodscout.com/ia/ames'
          label = HTML('Neighborhood<br>Scout')
          info = tags$a('Link here', href = link)
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'Feature Engineering references', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('house'), 
            color = 'warning',
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px')
          )
          
        } else {
          
          link = 'https://docs.google.com/presentation/d/1Efo76u7wFO1JSmdlqWcFj9rXMs7zM1hdPrYlR4KQ6zY/edit?usp=sharing'
          label = 'to see our full thought process'
          info = tags$a('GSlides Link Here', href=link)
          
          valueBox(
            subtitle = tags$p(
              label, 
              style = "margin-bottom: 0px; margin-left: 2px;color: black; 
              margin-top: 0px; margin-bottom: -15px; line-height: 1; font-weight: bold;
              font-size: 150%"),
            value = tags$p(
              'View the slidedeck to...', 
              style = "margin-bottom: -28px; margin-left: 2px;
              margin-top: -8px; margin-bottom: 0px;color: black; 
              font-size: 80%; font-style: italic"),
            icon = icon('info'), 
            color = 'warning',
            
            footer = tags$p(info, style='text-align: left; font-size: 80%;
                          color: black; margin-left: 10px; margin-bottom: 0px'),
          )
        }
      })
      
      
      output$income <- renderInfoBox({
        
        label = paste0(df_property()$nb_income, ' / 10')
        
        infoBox(
          value = tags$p(HTML(paste0(
            '<span style="font-size: 12px;">Income</span>')),
            style = 'margin-top: -32px; font-weight: normal; 
              color: black; text-align: center; margin-bottom: -20px'
          ),
          title = tags$p(HTML(paste0('<b><h3>', label, '</b></h3>')),
                         style = 'margin-bottom: 20px; margin-top: -20px;
                          color: black;'), 
          icon = icon("sack-dollar", class = "fa"),
          color = 'teal',
          fill = TRUE,
          gradient = TRUE
        )
      })
      
      output$appreciation <- renderInfoBox({
        
        label = paste0(df_property()$nb_appreciation, ' / 10')
        
        infoBox(
          value = tags$p(HTML(paste0(
            '<span style="font-size: 12px;">Appreciation</span>')),
            style = 'margin-top: -32px; font-weight: normal; 
              color: black; text-align: center; margin-bottom: -20px'
          ),
          title = tags$p(HTML(paste0('<b><h3>', label, '</b></h3>')),
                         style = 'margin-bottom: 20px; margin-top: -20px;
                          color: black;'), 
          icon = icon("arrow-trend-up", class = "fa"),
          color = 'orange',
          fill = TRUE,
          gradient = TRUE
        )
      })
      
      output$nb_info <- renderInfoBox({
        
        label = paste0(df_property()$nb_appreciation, ' / 10')
        
        info = case_when(
          df_property()$Neighborhood == 'All Neighborhoods' ~ all_nb,
          df_property()$Neighborhood == 'IDOTRR' ~ IDOTTR, 
          df_property()$Neighborhood == 'NAmes' ~ NAmes,
          df_property()$Neighborhood == 'Gilbert' ~ Gilbert,
          df_property()$Neighborhood == 'StoneBr' ~ StoneBr,
          df_property()$Neighborhood == 'NWAmes' ~ NWAmes, 
          df_property()$Neighborhood == 'Somerst' ~ Somerst,
          df_property()$Neighborhood == 'BrDale' ~ BrDale,
          df_property()$Neighborhood == 'NPkVill' ~ NPkVill,
          df_property()$Neighborhood == 'NridgHt' ~ NridgHt,
          df_property()$Neighborhood == 'Blmngtn' ~ Blmngtn,
          df_property()$Neighborhood == 'NoRidge' ~ NoRidge,
          df_property()$Neighborhood == 'SawyerW' ~ SawyerW,
          df_property()$Neighborhood == 'Sawyer' ~ Sawyer,
          df_property()$Neighborhood == 'Veenker' ~ Veenker,
          df_property()$Neighborhood == 'Greens' ~ Greens,
          df_property()$Neighborhood == 'BrkSide' ~ BrkSide,
          df_property()$Neighborhood == 'OldTown' ~ OldTown,
          df_property()$Neighborhood == 'ClearCr' ~ ClearCr,
          df_property()$Neighborhood == 'Edwards' ~ Edwards,
          df_property()$Neighborhood == 'SWISU' ~ SWISU,
          df_property()$Neighborhood == 'CollgCr' ~ CollgCr,
          df_property()$Neighborhood == 'Crawfor' ~ Crawfor,
          df_property()$Neighborhood == 'Mitchel' ~ Mitchel,
          df_property()$Neighborhood == 'Timber' ~ Timber,
          df_property()$Neighborhood == 'MeadowV' ~ MeadowV
        )
        
        
        infoBox(
          value = tags$p(HTML(paste0(
            '<span style="font-size: 14px;"><b>', df_property()$Neighborhood, '</b>', info, '</span>')),
            style = 'margin-top: 5px; font-weight: normal; line-height: 1;
              color: black; text-align: center; margin-bottom: 5px'
          ),
          title = '',
          icon = icon("question", class = "fa-3x"),
          color = 'info',
          fill = TRUE,
          gradient = TRUE
        )
      })
      
      
      
      
      # ------------------------------
      # Parameter Tuning: What If...?
      # ------------------------------
      
      output$current_price = renderUI({
        
        title = tagList('What if...', icon('question'))
        price = format(df_property_flip()$SalePrice, big.mark = ",")
        delta = format(round(abs(df_property_flip()$Delta)), big.mark = ',')
        value = case_when(
          df_property_flip()$Value == 'overvalued' ~ paste0('Overvalued by $', delta),
          df_property_flip()$Value == 'undervalued' ~ paste0('Undervalued by $', delta),
          TRUE ~ 'Fair Price'
        )
        color = case_when(
          df_property_flip()$Value == 'overvalued' ~ 'red',
          df_property_flip()$Value == 'undervalued' ~ 'lime',
          TRUE ~ 'orange'
        )
        
        tagList(
          title, 
          dashboardBadge(paste0('Home Listed for $', price), color = 'primary'),
          HTML(paste0('&nbsp;&nbsp;',
                      '<span style="font-size: 12px; font-style: italic; 
                      color: ', color,'">', value,'</span>'))
          )
        
      })
      
      output$conclusion = renderUI({
        text = 'Our model has been trained using Lasso Regression with a 92% train
        score. While this model did not yield the highest accuracy (as opposed to SVR/RF at 95%), it
        was the most interpretable and easily transferable from Python to R.'
        tags$p(text, style='font-size: 12px;')
      })
      output$table = renderTable({
        df_summary
      })
      
      output$rank_bar = renderPlot({
        ggplot(df_summary, aes(x = reorder(Feature, Importance), y = Importance)) + 
          geom_col(fill = 'royalblue4', width = 0.8) +
          geom_text(aes(label = Importance), vjust = 0.5, hjust = 1.2, color = "white", size = 2) +
          # scale_y_continuous(expand = c(0, 0)) +
          coord_flip() +
          # labs(title = "Rank of the most important features"
          #      #subtitle = ""
          #      ) +
          theme(
            #plot.title = element_text(size=12), #, margin = margin(5, 0, 0, 0)
            #plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
            panel.background = element_rect(fill = NA),
            panel.grid.major = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=10, face='bold', color = "white"), #, margin = margin(0, 2, 0, 0)
          )
      }, height = 280, width = 280)
      
      output$predicted_price = renderUI({
        
        title = tagList(icon('sign-hanging'), 'Our Estimation: ')
        price = format(round(predicted_price()), big.mark = ',')
        delta = format(round(df_property_flip()$Delta), big.mark = ',')
        value = case_when(
          df_property_flip()$Value == 'overvalued' ~ paste0('Overvalued by ', delta),
          df_property_flip()$Value == 'undervalued' ~ paste0('Overvalued by ', delta),
          TRUE ~ 'Fair Price'
        )
        
        
        tags$p(
          title, 
          dashboardBadge(color = 'success', HTML(paste0(
            '$', price
            ))),
          style = 'margin-top: -15px;'
        )
        
      })
      
      # --------------------------
      # What If...? Square Footage
      # --------------------------
      
      updateSliderInput(
        session,
        inputId = 'sqft_slider',
        min = df_features()$GrLivArea,
        value = df_features()$GrLivArea,
        max = 3000
      )
      
      # --------------------
      # What If...? Bedrooms
      # --------------------
      size = 60
      
      output$bedrooms = renderUI({
        
        knobInput(
          inputId = 'bedrooms', 
          label = '',
          height = size, width = size,
          min = 0, max = 10, 
          skin = 'tron',
          value = df_property()$BedroomAbvGr
        )
      })
      
      # ---------------------
      # What If...? Bathrooms
      # ---------------------
      
      output$bathrooms = renderUI({
        knobInput(
          inputId = 'bathrooms', 
          label='', 
          height = size, width = size,
          min = 0, max = 10, step = 0.5,
          skin = 'tron',
          value = df_property()$FullBath + 
            df_property()$HalfBath / 2
        )
      })
      
      # ----------------------------
      # What If...? Overall Quality
      # ----------------------------
      
      output$quality = renderUI({
        knobInput(
          inputId = 'quality',
          label='',
          height = size, width = size,
          min = 0, max = 10,
          skin = 'tron',
          value = df_features()$OverallQual
        )
      })
      
      # ------------------------------
      # What If...? Garage Quality
      # ------------------------------
      
      output$garage = renderUI({
        knobInput(
          inputId = 'garage',
          label='',
          height = size, width = size,
          min = 0, max = 10,
          value = df_features()$GarageQual,
          skin = 'tron',
          fgColor = '#FFA500'
        )
      })
      
      # ------------------------------
      # What If...? Basement Quality
      # ------------------------------
      
      output$basement = renderUI({
        knobInput(
          inputId = 'basement',
          label='',
          height = size, width = size,
          min = 0, max = 10,
          value = df_features()$BsmtQual,
          skin = 'tron',
          fgColor = '#FFA500'
        )
      })
      
      # ------------------------------
      # What If...? Kitchen Quality
      # ------------------------------
      
      
      output$kitchen = renderUI({
        knobInput(
          inputId = 'kitchen',
          label = '',
          height = size, width = size,
          min = 0, max = 10,
          value = df_features()$KitchenQual,
          skin = 'tron',
          fgColor = '#FFA500'
        )
      })
      
      # ------------------------------
      # What If...? Lot Area
      # ------------------------------
      
      output$months = renderUI({
        sliderTextInput(
          inputId = 'month',
          label = '',
          choices = c('January', 'February', 'March',
                      'April', 'May', 'June', 'July',
                      'August', 'September', 'October', 
                      'November', 'December'),
          selected = case_when(
            df_features()$MoSold == 1 ~ 'January',
            df_features()$MoSold == 2 ~ 'February',
            df_features()$MoSold == 3 ~ 'March',
            df_features()$MoSold == 4 ~ 'April',
            df_features()$MoSold == 5 ~ 'May',
            df_features()$MoSold == 6 ~ 'June',
            df_features()$MoSold == 7 ~ 'July',
            df_features()$MoSold == 8 ~ 'August',
            df_features()$MoSold == 9 ~ 'September',
            df_features()$MoSold == 10 ~ 'October',
            df_features()$MoSold == 11 ~ 'November',
            df_features()$MoSold == 12 ~ 'December'
          )
        )
      })
  
      # -------------------------------------
      # Vertical Panel: Visualize Prediction
      # -------------------------------------
      # -------------------------------------
      # Visualize Prediction: Predicted Price
      # -------------------------------------
      
      output$predicted = renderValueBox({
        
        price = paste0(round(predicted_price() / 1000), 'K')
        
        valueBox(
          subtitle = 'Predicted',
          value = HTML(paste0('<b><h3>', price, '</b></h3>')),
          icon = icon('money-bill-transfer'),
          color = 'green'
        )
      })
      
      
      # -------------------
      # SIDEBAR : ABOUT ME
      # -------------------
      
      # About Project
      output$about_project = renderUI({
        
        HTML("<font size='-1'>  Our Ames housing project aims to better understand what factors play in
        deciding the final property sales price. If we could predict sales price accurately from these
        features, we could, for example, maximize profits in the Ames housing market or in projects like 
        homeflipping. We used ridge regression to select the deciding property features with regularization, 
        and came up with an interpretable model to predict property sales price. We included neighborhood information 
        for a more comprehensive model such as crime rate, school quality, demographics etc. We analyzed
        sales price time series data to learn about seasonality, trend, the best time to sell and buy,
        and if a certain neighborhood housing price would fair well during a recession. This dashboard
        uses map features, selection buttons and info boxes to provide an interactive user experience
        to showcase our machine learning project.</font>")
      })
      
      # Laurel He
      output$laurel_bio = renderUI({
        
        HTML("<font size='-1'>  My undergrad was in 
       Geosystems Engineering and Hydrogeology at the University of Texas, 
       Austin. I got my Master’s in Atmosphere and Energy, Civil Engineering at 
       Stanford University. I’m passionate about nature, 
       environmental protection and renewable energy. I’m excited about how 
       machine learning and data analytics are giving us better tools to 
       understand and fight climate change, and I’m looking forward to kickstart 
       my career in this exciting field.</font>")
        
      })
      
      # Daniel Setiawan
      output$daniels_bio = renderUI({
        
        HTML("<font size='-1'>  I'm an alumnus of UCSB with a BS in Chemistry. 
        I currently work as an R&D engineer at a quantum computing startup, 
        where I had the opportunity to interface with various electronics (RF/DC) 
        and code (python, SCPI, Labview) to analyze failure rates, 
        improve product performance, and develop methods attuned to scalability. 
        This journey brought me to channel my inner passion for data science, 
        as there are many ways to creatively tell a story using data.</font>")
        
      })
    })

  # ------------------
  # SIDEBAR : DATASET
  # ------------------
  
  output$df = renderDataTable(
    datatable(style = "bootstrap5",
    df_predictions %>%
      select(-c(PID, Latitude, Longitude)),
    filter = 'top',
    options = list(scrollX = TRUE,
                   scrollY = TRUE,
                   pageLength = 5))
    )
  
  # -----------------------
  # ALL REACTIVE FUNCTIONS
  # -----------------------
  
  sqft = reactive({
    input$sqft_slider
  })
  
  df_neighborhood = reactive({
    
    if (input$neighborhood == 'All Neighborhoods') {
      df_predictions
    } else {
      df_predictions %>%
        filter(Neighborhood == input$neighborhood)
    }
  })
  
  df_neighborhood_flip = reactive({
    
    if (input$neighborhood_flip == 'All Neighborhoods') {
      df_predictions
    } else {
      df_predictions %>%
        filter(Neighborhood == input$neighborhood_flip)
    }
  })
  
  df_property = reactive({ # features for current home and main panel
    df_predictions %>%
      filter(Prop_Addr == input$address)
  })
  
  
  df_property1 = reactive({ # property comparison #1
    df_predictions %>%
      filter(Prop_Addr == input$compare_1)
  })
  
  df_property2 = reactive({ # property comparison #2
    df_predictions %>%
      filter(Prop_Addr == input$compare_2)
  })
  
  df_property_flip = reactive({ # features for selected property in homeflipping
    df_predictions %>%
      filter(Prop_Addr == input$address_flip)
  })
  
  df_features = reactive({ # features for dot matrix
    df_feats %>%
      filter(Prop_Addr == input$address_flip) %>%
      select(-Prop_Addr)
  })
  
  
  
  df_undervalued = reactive({
    (df_neighborhood_flip() %>%
       # head(25) %>%
       select(Prop_Addr, Delta, Value, SalePrice) %>%
       arrange(Delta) %>%
       mutate(SalePrice = paste0(round(as.numeric(SalePrice / 1000)), 'K'),
              Value = case_when(Value == 'undervalued' ~ 'undervalued by ',
                                Value == 'overvalues' ~ 'overvalued by  ',
                                TRUE ~ 'fair price'),
              Delta = ifelse(Value == 'fair price', '',
                             paste0(abs(round(as.numeric(Delta/1e3))), 'K'))))[-1:-3,]
    
  })
  
  
  predicted_price = reactive({
    
    
    df_features() %>%
      mutate(GrLivArea = sqft(),
             GarageQual = input$garage,
             OverallQual = input$quality,
             BedroomAbvGr = input$bedrooms,
             HalfBath = input$bathrooms,
             KitchenQual = input$kitchen,
             BsmtQual = input$basement,
             MoSold = case_when(
               input$month == 'January' ~ 1,
               input$month == 'February' ~ 2,
               input$month == 'March' ~ 3,
               input$month == 'April' ~ 4,
               input$month == 'May' ~ 5,
               input$month == 'June' ~ 6,
               input$month == 'July' ~ 7,
               input$month == 'August' ~ 8,
               input$month == 'September' ~ 9,
               input$month == 'October' ~ 10,
               input$month == 'November' ~ 11,
               input$month == 'December' ~ 12,
               TRUE ~ 12)
      ) %>%
      as_vector() %*%
      df_coefs$coefs
    
  })
  
  comps = reactive({
    for (i in 1:nrow(df_predictions)) {
      df_predictions$distance[i] = haversine_distance(
        df_property()$Latitude, df_property()$Longitude,
        df_predictions$Latitude[i], df_predictions$Longitude[i]
      )
    }
    
    final = df_predictions %>%
      filter(distance != 0,
             between(SalePrice, 0.9*df_property()$SalePrice, 1.1*df_property()$SalePrice),
             between(GrLivArea, 0.8*df_property()$GrLivArea, 1.2*df_property()$GrLivArea),
             between(OverallQual, df_property()$OverallQual-2, 3+df_property()$OverallQual),
             between(OverallCond, df_property()$OverallCond-2, 3+df_property()$OverallCond)
      ) %>%
      arrange(distance) %>%
      select(distance, SalePrice, Prop_Addr, GrLivArea,
             BedroomAbvGr, FullBath, HalfBath) %>%
      distinct() %>%
      head(4)
  })
  
}





