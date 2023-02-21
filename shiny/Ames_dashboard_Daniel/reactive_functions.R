function(input, output, session) {

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