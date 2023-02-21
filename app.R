if (interactive()) {
  
  data(exTaxonList)
  data(exPftList)
  
  ui <- fluidPage(
    
    radioMatrixInput(inputId = "rmi01", rowIDs = head(exTaxonList$Var), 
                     rowLLabels = head(
                       as.matrix(subset(exTaxonList, select = "VarName"))
                     ), 
                     choices = exPftList$ID, 
                     selected = head(exTaxonList$DefPFT)), 
    verbatimTextOutput('debug01')
  )
  
  server <- function(input, output, session) { 
    output$debug01 <- renderPrint({input$rmi01})
  }
  
  shinyApp(ui, server)
  
}

if (interactive()) {
  
  ui <- fluidPage(
    
    radioMatrixInput(inputId = "rmi02", rowIDs = c("Performance", "Statement A"),
                     rowLLabels = c("Poor", "Agree"), 
                     rowRLabels = c("Excellent", "Disagree"),
                     choices = 1:5,
                     selected = rep(3, 2),
                     labelsWidth = list("100px", "100px")),
    verbatimTextOutput('debug02')
  )
  
  server <- function(input, output, session) {
    output$debug02 <- renderPrint({input$rmi02})
  }
  
  shinyApp(ui, server)
}