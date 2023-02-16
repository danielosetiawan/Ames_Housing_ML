# ----------------------------------------------------------------

# Backend for Ames Housing Project

# ----------------------------------------------------------------

function(input, output, session) {
  
  observeEvent(input$neighborhood, {

# ----------------------------------------------------------------
# Vertical Panel: Ames Housing Map
# ----------------------------------------------------------------
   
    session$onFlushed(once=T, function(){
        
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
                
                leaflet() %>%
                  addTiles() %>% 
                  addProviderTiles('CartoDB') %>%
                  addProviderTiles('Stamen.TonerLines') %>% 
                  addMarkers(data = undervalue, lng = ~Longitude, lat = ~Latitude,
                                   icon = markers['Undervalued'], layerId = ~PID, label = glue(
                                     "<b>Sale Price: $ </b>{undervalue$SalePrice}</br>",
                                     "<b>Address: </b> {undervalue$Prop_Addr}<br/>",
                                     "<b>Neighborhood: </b> {undervalue$Neighborhood}<br/>",
                                     "<b>Gross Living Area: sq. ft. </b> {undervalue$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                                   group = 'Housing') %>%
                  addMarkers(data = fair, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Fair'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{fair$SalePrice}</br>",
                               "<b>Address: </b> {fair$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {fair$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {fair$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                            group = 'Housing') %>%
                  addMarkers(data = overvalue, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Overvalued'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{overvalue$SalePrice}</br>",
                               "<b>Address: </b> {overvalue$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {overvalue$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {overvalue$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing') %>%
                  addLegend(
                    position = "topleft",
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
                m = leaflet() %>% addTiles() 
                if ( dim(undervalue)[1] != 0) {
                  m = m %>% 
                  addMarkers(data = undervalue, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Undervalued'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b> {undervalue$SalePrice}</br>",
                               "<b>Address: </b> {undervalue$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {undervalue$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {undervalue$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing')
                }
                if (dim(fair)[1] != 0) {
                  m = m %>%
                  addMarkers(data = fair, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Fair'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{fair$SalePrice}</br>",
                               "<b>Address: </b> {fair$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {fair$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {fair$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing')
                }
                if (dim(overvalue)[1] != 0) {
                  m = m %>%
                  addMarkers(data = overvalue, lng = ~Longitude, lat = ~Latitude,
                             icon = markers['Overvalued'], layerId = ~PID, label = glue(
                               "<b>Sale Price: $ </b>{overvalue$SalePrice}</br>",
                               "<b>Address: </b> {overvalue$Prop_Addr}<br/>",
                               "<b>Neighborhood: </b> {overvalue$Neighborhood}<br/>",
                               "<b>Gross Living Area: sq. ft. </b> {overvalue$GrLivArea}<br/>") %>% lapply(htmltools::HTML),
                             group = 'Housing') 
                }
                
                  m %>% addLegend(
                    position = "topleft",
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
        
    })

# ---------------------------
# Vertical Panel: Timeseries
# ---------------------------
    
    observeEvent(input$ts_ci, {
      
      if (input$neighborhood == 'All Neighborhoods') {
        sarima_model = ifelse(
          input$ts_ci == TRUE, 
          './img/ts_with_ci/Ames_sarima_prediction.png', 
          './img/ts_without_ci/Ames_sarima_prediction_noci.png'
        )
      } else {
        sarima_model = ifelse(
          input$ts_ci == TRUE, 
          paste0('./img/ts_with_ci/', input$neighborhood, '_sarima_prediction.png'),
          paste0('./img/ts_without_ci/', input$neighborhood, '_sarima_prediction_noci.png')
        )
      }
      
      output$sarima = renderUI({
        HTML(paste0('<img width = "100%", height = "70%",
                    src= "', sarima_model, '"/>'))
        
      })
    })
    
# ---------------------
# Homepage: Main Panel
# ---------------------
    updatePickerInput(
      session,
      inputId = 'address',
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
    
    observeEvent(input$address, {

      # ----------------------
      # Main Panel: Info Boxes
      # ----------------------
      
      output$crime_rate <- renderValueBox({
        label = paste0(df_property()$crime_rate, ' / 10')
        
        valueBox(
          subtitle = "",
          value = tags$p(label, style = "font-size: 200%;"),
          icon = icon('handcuffs'),
          color = 'secondary',
          footer = tags$p('Crime Rate', style='text-align: left; 
                          margin-left: 10px; margin-bottom: 0px')
          )
        
      })
      
      
      output$school_quality <- renderValueBox({
        
        label = paste0(df_property()$school_quality, ' / 10')
        
        valueBox(
          subtitle = "",
          value = tags$p(label, style = "font-size: 200%;"),
          icon = icon('graduation-cap'),
          color = 'info',
          footer = tags$p('School Quality', style='text-align: left; 
                          margin-left: 10px; margin-bottom: 0px')
        )
      })
      
      output$income <- renderValueBox({
        
        label = paste0(df_property()$nb_income, ' / 10')
        
        valueBox(
          subtitle = "",
          value = tags$p(label, style = "font-size: 200%;"),
          icon = icon('sack-dollar'),
          color = 'olive',
          footer = tags$p('NB Income', style='text-align: left; 
                          margin-left: 10px; margin-bottom: 0px')
        )
      })
      
      output$appreciation <- renderValueBox({
        
        label = paste0(df_property()$nb_appreciation, ' / 10')
        
        valueBox(
          subtitle = "",
          value = tags$p(label, style = "font-size: 200%;"),
          icon = icon('arrow-trend-up'),
          color = 'warning',
          footer = tags$p('NB Appreciation', style='text-align: left; 
                          margin-left: 10px; margin-bottom: 0px')
        )
      })
      
# --------------------------------
# Vertical Panel: Parameter Tuning
# --------------------------------
      
      # ------------------------------
      # Parameter Tuning: Current Home
      # ------------------------------
      
      output$current_home = renderUI({
        
        sale_price = paste0(round(df_property()$SalePrice / 1000), 'K')
        predicted_price = paste0(round(df_property()$Predicted / 1000), 'K')
        accommodations = paste0(
          df_property()$BedroomAbvGr, 'BR / ', df_property()$FullBath + 
            df_property()$HalfBath / 2, 'BA')
        
        
        if (df_property()$Delta/1e3 <= -5) {
          value = 'UNDERVALUED'
          color = 'green'
        } else if (df_property()$Delta/1e3 >= 5) {
          value = 'OVERVALUED'
          color = 'red'
        } else {
          value = 'FAIR PRICE'
          color = 'orange'
        }
        
        HTML(
          paste0('<b><span style="color:', color, '">', value, '</span></b><br><br>',
                 'Sale Price: <b>', sale_price, '</b><br>',
                 'Bed / Bath: <b>', accommodations, '</b><br>',
                 'Year Built: <b>', df_property()$YearBuilt, '</b><br>',
                 'Year Remodeled: <b>', df_property()$YearRemodAdd, '</b><br>',
                 'Gross Living Area: <b>', df_property()$GrLivArea, '</b><br>',
                 '1st floor SF: <b>', df_property()$X1stFlrSF, '</b><br>',
                 '2nd floor SF: <b>', df_property()$X2ndFlrSF, '</b><br>',
                 'Total Basement SF: <b>', df_property()$TotalBsmtSF, '</b><br>',
                 'Overall Quality: <b>', df_property()$OverallQual, '/10</b><br>',
                 'Overall Condition: <b>', df_property()$OverallCond, '/10</b><br>',
                 'Kitchen Quality: <b>', df_property()$KitchenQual, '/10</b><br>',
                 'Garage Quality: <b>', df_property()$GarageQual, '/10</b><br>',
                 'Basement Quality: <b>', df_property()$BsmtQual, '/10</b><br>'
          ))
        
      })
      
      
  # ------------------------------
  # Parameter Tuning: What If...?
  # ------------------------------
      
      output$predicted_price = renderUI({
        
        title = tagList('What if...', icon('question'))
        price = paste0(round(predicted_price() / 1000), 'K')
        
        tagList(
          title, dashboardLabel('   ', status = 'primary'),
          dashboardLabel('Predicted Price: ', status = 'primary'),
          dashboardLabel(price, status = 'success'))
        
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
      size = 53
      
      output$bedrooms = renderUI({
        
        knobInput(
          inputId = 'bedrooms', 
          label = 'Beds',
          height = size, width = size,
          min = 0, max = 10, 
          value = df_property()$BedroomAbvGr
        )
      })
      
      # ---------------------
      # What If...? Bathrooms
      # ---------------------
      
      output$bathrooms = renderUI({
        knobInput(
          inputId = 'bathrooms', 
          label='Baths', 
          height = size, width = size,
          min = 0, max = 10, 
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
          label='Quality', 
          height = size, width = size,
          min = 0, max = 10, 
          value = df_features()$OverallCond
        )
      })
      
      # ------------------------------
      # What If...? Overall Condition
      # ------------------------------
      
      output$condition = renderUI({
        knobInput(
          inputId = 'condition', 
          label='Condition', 
          height = size, width = size,
          min = 0, max = 10, 
          value = df_features()$OverallQual,
          # fgColor = '#FFA500'
        )
      })
      
      # ------------------------------
      # What If...? Basement Quality
      # ------------------------------
      
      output$basement = renderUI({
        knobInput(
          inputId = 'basement', 
          label='Basement', 
          height = size, width = size,
          min = 0, max = 10, 
          value = df_features()$BsmtQual,
          # fgColor = '#FFA500'
        )
      })
      
      # ------------------------------
      # What If...? Kitchen Quality
      # ------------------------------
      
      
      output$kitchen = renderUI({
        knobInput(
          inputId = 'kitchen', 
          label='Kitchen', 
          height = size, width = size,
          min = 0, max = 10, 
          value = df_features()$KitchenQual,
          # fgColor = '#FFA500'
        )
      })
      
      
# -------------------------------------
# Vertical Panel: Visualize Prediction
# -------------------------------------
      # -------------------------------------
      # Visualize Prediction: Predicted Price
      # -------------------------------------
      
      # session$onFlushed(once=T, function(){
      output$predicted = renderValueBox({
        
        price = paste0(round(predicted_price() / 1000), 'K')
        
        valueBox(
          subtitle = 'Predicted',
          value = HTML(paste0('<b><h3>', price, '</b></h3>')),
          icon = icon('money-bill-transfer'),
          color = 'green'
          )
      })
      
      # -------------------------------------
      # Visualize Prediction: Added Sq. Feet
      # -------------------------------------
      
      output$addedarea = renderValueBox({
        
        added_sqft = sqft() - df_features()$GrLivArea
        
        valueBox(
          subtitle = 'Added Sq. Ft.',
          value = HTML(paste0('<b><h3>', added_sqft, '</b></h3>')),
          icon = icon('sort-amount-up-alt'),
          color = 'blue'
        )
      })
      
      # -----------------------------------
      # Visualize Prediction: Scatter Plot
      # -----------------------------------
      
      output$scatplot = renderPlot({
        
        orig_predict = df_features() %>%
          as_vector() %*% 
          df_coefs$coefs
        
        ggplot(data = df_predictions, aes(x = GrLivArea, y = SalePrice)) +
          scale_color_manual(values = c('After renovation' = 'red', 
                                        'Before renovation' = 'orange', 
                                        'Original price' = 'green')) +
          theme_bw() +
          
          # ----------------------------
          # Scatter plot: Original Price
          # ----------------------------
          geom_point(data = df_property(),
                   aes(x=GrLivArea, y = SalePrice, 
                       color = 'Original price'),
                   size = 3, shape = 7, alpha = 1, stroke = 1.25) + 
          geom_point(size = 0.5) +
          
          # -------------------------------
          # Scatter plot: Before Renovation
          # -------------------------------
          geom_point(data = data.frame('GrLivArea' = df_features()$GrLivArea,
                                     'SalePrice' = orig_predict),
                   aes(x=GrLivArea, y = SalePrice, 
                       color = 'Before renovation'),
                   size = 3, shape = 7, alpha = 1, stroke = 1.25) +
          
          # ------------------------------
          # Scatter plot: After Renovation
          # ------------------------------
          
          geom_point(data = data.frame('GrLivArea' = sqft(),
                                       'SalePrice' = predicted_price()),
                     aes(x=GrLivArea, y = SalePrice, 
                         color = 'After renovation'),
                     size = 3, shape = 7, alpha = 1, stroke = 1.25) +
          
          # ------------------------------
          # Scatter plot: Final Touchups
          # ------------------------------
          
          
          scale_y_continuous(breaks = c(1e5, 3e5, 5e5, 7e5),
                             labels = c('100K', '300K', '500K', '700K')) +
          labs(x = 'Gross Living Area', y = 'Price', color = '')
        
        })

      
      
# -------------------
# SIDEBAR : ABOUT ME
# -------------------
      
      # Laurel He
      output$laurel_bio = renderUI({
        
        HTML("<font size='-1'>  My undergrad was in 
       Geosystems Engineering and Hydrogeology at the University of Texas, 
       Austin. I got my Master’s in Atmosphere and Energy, Civil Engineering at 
       Stanford. I’m passionate about nature, 
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
      
      
      
      # Daniel Erickson
      output$daniele_bio = renderUI({
        
        HTML("<font size='-1'>  I received my BS in mathematics from the University
        of Minnesota, and I earned my Ph.D in mathematics from Oregon State University.
        I am interested in the wealth of information that tools from data science
        and machine learning can provide and how they can uncover aspects of reality
        that would be difficult to otherwise parse. I am excited to begin my career
        outside of education and tackle real-world problems with the skillset I've developed.  </font>")
        
      })
    })
  })
  
# ------------------
# SIDEBAR : DATASET
# ------------------
  
  output$df = renderDataTable(
    df_predictions %>%
      select(-c(PID, Latitude, Longitude)),
    filter = 'top',
    options = list(scrollX = TRUE,
                   scrollY = TRUE))

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
    }})
  
  df_property = reactive({ # features for current home and main panel
    df_predictions %>%
      filter(Prop_Addr == input$address)
  })
  
  df_features = reactive({ # features for dot matrix
    df_feats %>%
      filter(Prop_Addr == input$address) %>%
      select(-Prop_Addr)
  })
  
  
  df_undervalued = reactive({
    (df_neighborhood() %>%
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
  
  predicted_price_null = reactive({
    df_features() %>%
      mutate(GrLivArea = sqft(),
             # OverallCond = OverallCondVal(),
             # OverallQual = OverallQualVal(),
             BedroomAbvGr = df_property()$BedroomAbvGr) %>%
      # HalfBath = BathroomsVal()) %>%
      as_vector() %*%
      df_coefs$coefs
    
  })
  
  predicted_price = reactive({
    df_features() %>%
      mutate(GrLivArea = sqft(),
             OverallCond = input$condition,
             OverallQual = input$quality,
             BedroomAbvGr = input$bedrooms,
             HalfBath = input$bathrooms,
             KitchenQual = input$kitchen,
             BsmtQual = input$basement) %>%
      as_vector() %*%
      df_coefs$coefs
    
  })

    
}




