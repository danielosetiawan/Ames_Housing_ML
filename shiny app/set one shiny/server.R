# ML Ames
# backend code

function(input, output, session) {
  
  output$timeseries = renderPlot(
    expr = all_neighborhoods()
  )
  
  # assignments if neighborhood changes
  
  
  
  # slider_range = reactive({
  #   c(input$budget[1], input$budget[2])
  # })
  # observeEvent(input$budget, {
  #   slider_min = input$budget[1]
  #   slider_max = input$budget[2]
  
  observeEvent(input$neighborhood, {
    
    
    ##### budget slider
    min_price = min(df_neighborhood()$SalePrice)
    max_price = max(df_neighborhood()$SalePrice)
    mid_price = max_price - min_price
    first_qtl = signif(min_price + 0.25 * mid_price, 2)
    third_qtl = signif(max_price - 0.25 * mid_price, 2)
    
    updateSliderInput(
      session,
      inputId = 'budget',
      min = signif(min_price, 2),
      max = signif(max_price, 2),
      value = c(first_qtl, third_qtl)
    )
    
    # picker labels
  
    
    updatePickerInput(
      session,
      inputId = 'undervalued',
      # label = c(slider_min, slider_max),
      selected = NULL,
      choices = undervalued()$Prop_Addr,
      choicesOpt = list(
        subtext = paste0(
          c('\t\t'), 'Price: $', 
          undervalued()$SalePrice, ' (',
          undervalued()$Value,
          undervalued()$Delta, ')'
          )
        ),
      options = pickerOptions(virtualScroll = TRUE,
                              dropupAuto = FALSE,
                              width = '70%',
                              title = FALSE)
    )
  })
  ############ reactive functions ############
    
  # budget slider
    df_neighborhood = reactive({
      if (input$neighborhood == 'All Neighborhoods') {
        df
      } else {
        df %>%
          filter(Neighborhood == input$neighborhood)
      }})
    
    # dropdown predictions
    predictions = reactive({
      if (input$neighborhood == 'All Neighborhoods') {
        df_predictions %>%
          # filter(input$budget[2] < 2e5 | input$budget[1] > 5.7e5) %>%
          head(25)
      } else {
        picker_label = df_predictions %>%
          filter(Neighborhood == input$neighborhood,
                 input$budget[1] < 2e5 | input$budget[2] > 5.7e5) %>%
          head(25)
      }})
    
    undervalued = reactive({
      predictions() %>%
      select(Prop_Addr, Delta, SalePrice) %>%
      mutate(SalePrice = paste0(round(as.numeric(SalePrice / 1000)), 'K'),
             Value = case_when(as.numeric(Delta/1e3) <= -5  ~ 'undervalued by ',
                               as.numeric(Delta/1e3) >= 5 ~ 'overvalued by ',
                               TRUE ~ 'fair price'),
             Delta = ifelse(Value == 'fair price', '', 
                            paste0(abs(round(as.numeric(Delta/1e3))), 'K')))
    })
    

  
  
}
