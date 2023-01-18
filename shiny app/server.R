# ----------------------------------------------------------------

# Backend for Ames Housing Project

# ----------------------------------------------------------------

function(input, output, session) {
  
  observeEvent(input$neighborhood, {
    

# ----------------------------------------------------------------
# HOUSING MAP PANEL
# ----------------------------------------------------------------
    
    session$onFlushed(once=TRUE, function(){
      output$map = renderLeaflet({  
        factpal = colorFactor(
          palette = rainbow(25), 
          domain = housing$Neighborhood)
        leaflet(data = housing) %>%
          addTiles() %>% 
          addCircleMarkers(
            lng = ~Longitude, lat = ~Latitude, color = ~factpal(Neighborhood), 
            radius = 0.5, layerId = ~PID) %>% 
          addLegendFactor(
            pal = factpal, values = ~Neighborhood, position = 'topright', 
            labelStyle = 'font-size: 8px;', height = 8, width=8)
      })
    })
    
    
    
    observe({
      click <- input$map_marker_click
      if (is.null(click))
        return()
      data = housing[housing$PID == click$id,]
      content <- as.character(tagList(
        sprintf("Sale Price: $ %s", data$SalePrice), tags$br(),
        sprintf("Neighborhood: %s", data$Neighborhood), tags$br(),
        sprintf("Living Area: %s sq ft", data$GrLivArea), tags$br(),
        sprintf("Overall Condition: %s", data$OverallCond)
      ))
      leafletProxy(mapId = "map") %>%
        addPopups(lng = click$lng, lat = click$lat, 
                  popup = content, layerId = click$id)
    })
    
        
# ----------------------------------------------------------------
# TIMESERIES PANEL
# ----------------------------------------------------------------
    
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
    
# ----------------------------------------------------------------
# MAIN PANEL
# ----------------------------------------------------------------
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
    
    output$undervalued_dt = renderDataTable(
      server = FALSE,
      datatable(df_undervalued(), 
                filter = 'top', 
                selection = 'single',
                options = list(
                  pageLength = 5, 
                  scrollY = "600px", 
                  autoWidth = TRUE))
    )
    
    
    
    observeEvent(input$address, {

      # ------------------------------
      # Main Panel: Info Boxes
      # ------------------------------
      
      output$crime_rate <- renderValueBox({
        valueBox(
          value = tags$p('Crime Rate', style = "font-size: 50%;"),
          subtitle=tags$p(paste0(df_property()$crime_rate, ' / 10'), 
                          style = "font-size: 200%;"),
          icon = icon('handcuffs'),
          color = 'purple')
      })
      
      
      output$school_quality <- renderValueBox({
        valueBox(
          value = tags$p('School Quality', style = "font-size: 50%;"),
          subtitle=tags$p(paste0(df_property()$school_quality, ' / 10'), 
                          style = "font-size: 200%;"),
          icon = icon('graduation-cap'),
          color = 'light-blue')
      })
      
      output$income <- renderValueBox({
        valueBox(
          value = tags$p('Neighborhood Income', style = "font-size: 50%;"),
          subtitle=tags$p(paste0(df_property()$nb_income, ' / 10'), 
                          style = "font-size: 200%;"),
          icon = icon('sack-dollar'),
          color = 'olive')
      })
      
      output$appreciation <- renderValueBox({
        valueBox(
          value = tags$p('Neighborhood Appreciation', style = "font-size: 50%;"),
          subtitle=tags$p(paste0(df_property()$nb_appreciation, ' / 10'), 
                          style = "font-size: 200%;"),
          icon = icon('arrow-trend-up'),
          color = 'yellow')
      })
      
# ----------------------------------------------------------------
# PARAMETER TUNING PANEL
# ----------------------------------------------------------------
      
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
                 'Predicted Price: <b>', predicted_price, '</b><br>',
                 'Accommodations: <b>', accommodations, '</b><br>',
                 'Year Built: <b>', df_property()$YearBuilt, '</b><br>',
                 'Year Remodeled: <b>', df_property()$YearRemodAdd, '</b><br>',
                 'Total SF: <b>', df_property()$GrLivArea, '</b><br>',
                 '1st floor SF: <b>', df_property()$X1stFlrSF, '</b><br>',
                 '2nd floor SF: <b>', df_property()$X2ndFlrSF, '</b><br>',
                 'Total Basement SF: <b>', df_property()$TotalBsmtSF, '</b><br>',
                 'Overall Quality: <b>', df_property()$OverallQual, '/10</b><br>',
                 'Overall Condition: <b>', df_property()$OverallCond, '/10</b><br>',
                 'Kitchen Quality: <b>', df_property()$KitchenQual, '/5</b><br>',
                 'Garage Quality: <b>', df_property()$GarageQual, '/5</b><br>',
                 'Basement Quality: <b>', df_property()$BsmtQual, '/5</b><br>'
          ))
        
      })
      
      
      # ------------------------------
      # Parameter Tuning: What If...?
      # ------------------------------
      
      updateSliderInput(
        session,
        inputId = 'sqft_slider',
        min = df_features()$GrLivArea,
        value = df_features()$GrLivArea,
        max = 5000
      )

      
      output$Bedrooms = renderUI({
        
        
        
        
        HTML(paste0('Bedrooms: ', BedroomsVal()))
      })
      
      output$Bathrooms = renderUI({
        HTML(paste0('Bathrooms: ', BathroomsVal()))
      })
      
      output$OverallCondition = renderUI({
        HTML(paste0('Condition: ', OverallCondVal()))
      })
      
      output$OverallQuality = renderUI({
        HTML(paste0('Quality: ', OverallQualVal()))
      })
      
      
      # ------------------------------
      # Parameter Tuning: Prediction
      # ------------------------------
      
      output$prediction = renderInfoBox({
        
        predicted_df = df_features() %>%
          mutate(GrLivArea = sqft(),
                 OverallCond = OverallCondVal(),
                 OverallQual = OverallQualVal(),
                 BedroomAbvGr = BedroomsVal(),
                 HalfBath = BathroomsVal()
                 
                 ) %>%
          as_vector() %*% 
          df_coefs$coefs
        
        predicted_price = paste0(round(predicted_df / 1000), 'K')
        
        infoBox(
          title = 'Predicted', 
          value = HTML(paste0('<b><h3>', predicted_price, '</b></h3>')),
          icon = icon('money-bill-transfer'),
          color = 'green', fill = TRUE)
        
        
        
      })
    })
  })
  
  # ----------------------------------------------------------------
  # VISUAL PANEL
  # ----------------------------------------------------------------  
  
  output$predicted = renderInfoBox({
    
    predicted_df2 = df_features() %>%
      mutate(GrLivArea = sqft(),
             OverallCond = OverallCondVal(),
             OverallQual = OverallQualVal(),
             BedroomAbvGr = BedroomsVal(),
             HalfBath = BathroomsVal()
             
      ) %>%
      as_vector() %*%
      df_coefs$coefs
    
    predicted_price = paste0(round(predicted_df2 / 1000), 'K')
    
    infoBox(
      title = 'Predicted Price',
      value = HTML(paste0('<b><h3>', predicted_price, '</b></h3>')),
      icon = icon('money-bill-transfer'),
      color = 'green', fill = TRUE)
  })
  
  output$addedarea = renderInfoBox({
    infoBox(
      title = 'Added Square Footage',
      value = HTML(paste0('<b><h3>', sqft()-df_features()['GrLivArea'], '</b></h3>')),
      color = 'blue', fill = TRUE
    )
  })
  
  
  output$scatplot = renderPlot({
    
    predicted_df3 = df_features() %>%
      mutate(GrLivArea = sqft(),
             OverallCond = OverallCondVal(),
             OverallQual = OverallQualVal(),
             BedroomAbvGr = BedroomsVal(),
             HalfBath = BathroomsVal()
             
      ) %>%
      as_vector() %*%
      df_coefs$coefs
    
    orig_predict = df_features() %>%
      as_vector() %*% 
      df_coefs$coefs
    
    ggplot(data = df, aes(x= GrLivArea, y = SalePrice)) +
      geom_point(size=.5) +
      labs(x = 'Gross Living Area', y = 'Price') +
      geom_point(data = data.frame('GrLivArea' = sqft(),
                                   'SalePrice' = predicted_df3),
                 aes(x=GrLivArea, y = SalePrice, 
                     color = 'Estimated price after renovation'),
                 size = 3,
                 shape = 7,
                 alpha = 1,
                 stroke = 1.25) +
      geom_point(data = data.frame('GrLivArea' = df_features()$GrLivArea,
                                   'SalePrice' = orig_predict),
                 aes(x=GrLivArea, y = SalePrice, 
                     color = 'Estimated price before renovation'),
                 size = 3,
                 shape = 7,
                 alpha = 1,
                 stroke = 1.25)+
      geom_point(data = df_property(),
                 aes(x=GrLivArea, y = SalePrice, 
                     color = 'Original price of home'),
                 size = 3,
                 shape = 7,
                 alpha = 1,
                 stroke = 1.25,
      ) + 
      scale_color_manual(name ='', 
                         values = c('Estimated price after renovation'='red', 
                                    'Estimated price before renovation'='orange', 
                                    'Original price of home'='green')
      )
  })
  
# ----------------------------------------------------------------
# DATASET PANEL
# ----------------------------------------------------------------
  
  output$prediction_df <- renderDataTable(
    df_predictions,
    options = list(pageLength = 4,
                   scrollX = TRUE,
                   scrollY = TRUE))
  

# ----------------------------------------------------------------
# ALL REACTIVE FUNCTIONS
# ----------------------------------------------------------------
  
  ### note: these reactive functions dont reset when you change property
  # maybe put them inside the observe and do an if function ??
  # how to achieve?
  BedroomsVal = reactive({

    df_property()$BedroomAbvGr +
      input$bedrooms_up - input$bedrooms_down

  })
  
  BathroomsVal = reactive({
    
    df_property()$FullBath + df_property()$HalfBath / 2 +
      input$bathrooms_up - input$bathrooms_down
    
  })
  
  OverallCondVal = reactive({
    
    df_features()$OverallCond + 
      input$OvCond_up - input$OvCond_down
    
  })
  
  OverallQualVal = reactive({
    
    df_features()$OverallQual + 
      input$OvQual_up - input$OvQual_down
    
  })
  
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
  
  df_property = reactive({
    df_predictions %>%
      filter(Prop_Addr == input$address)
  })
  
  
  
  df_undervalued = reactive({
    df_neighborhood() %>%
      head(25) %>%
      select(Prop_Addr, Delta, SalePrice) %>%
      mutate(SalePrice = paste0(round(as.numeric(SalePrice / 1000)), 'K'),
             Value = case_when(as.numeric(Delta/1e3) <= -5  ~ 'undervalued by ',
                               as.numeric(Delta/1e3) >= 5 ~ 'overvalued by ',
                               TRUE ~ 'fair price'),
             Delta = ifelse(Value == 'fair price', '', 
                            paste0(abs(round(as.numeric(Delta/1e3))), 'K')))
  })
  
  df_features = reactive({
    df_feats %>%
      filter(Prop_Addr == input$address) %>%
      select(-Prop_Addr)
  })
  
  
}
