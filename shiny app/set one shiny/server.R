# ML Ames
# backend code

function(input, output, session) {
  
  
  # assignments if neighborhood changes
  
  
  
  # slider_range = reactive({
  #   c(input$budget[1], input$budget[2])
  # })
  # observeEvent(input$budget, {
  #   slider_min = input$budget[1]
  #   slider_max = input$budget[2]
  
  observeEvent(input$neighborhood, {
    
    
    ##### budget slider
    # min_price = min(df_neighborhood()$SalePrice)
    # max_price = max(df_neighborhood()$SalePrice)
    # mid_price = max_price - min_price
    # first_qtl = signif(min_price + 0.25 * mid_price, 2)
    # third_qtl = signif(max_price - 0.25 * mid_price, 2)
    
    # updateSliderInput(
    #   session,
    #   inputId = 'budget',
    #   min = signif(min_price, 2),
    #   max = signif(max_price, 2),
    #   value = c(first_qtl, third_qtl)
    # )
  

### timeseries
    if (input$neighborhood == 'All Neighborhoods') {
      sarima_model = './img/Ames_sarima_prediction.png'
    } else {
      sarima_model = paste0('./img/', input$neighborhood, 
                            '_sarima_prediction.png')
    }
  
    output$sarima = renderUI({
      HTML(paste0('<img width = "100%", height = "70%",
                    src= "', sarima_model, '"/>'))
    })
    
### addresses
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
    
  })
  
  observeEvent(input$address, {
    
    ## info boxes
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
    
    output$saleprice <- renderValueBox({
      
      price = paste0(round(df_property()$SalePrice / 1000), 'K')
      
      valueBox(
        value = tags$p('Sale Price', style = "font-size: 50%;"),
        subtitle=tags$p(price, style = "font-size: 200%;"),
        icon = icon('sack-dollar'),
        color = 'light-blue')
    })
    
    
    ## current home
    
    output$current_home <- renderUI({
      
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
    
  })
  
  
  ############ reactive functions ############
    
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
    
    ####################
  
  
}
