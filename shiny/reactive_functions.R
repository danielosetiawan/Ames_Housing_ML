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
             # between(LotArea, 0.5*df_property()$LotArea, 1.5*df_property()$LotArea),
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