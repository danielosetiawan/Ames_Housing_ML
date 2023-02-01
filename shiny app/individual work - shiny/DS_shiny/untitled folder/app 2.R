if (interactive()) {
  library("shiny")
  library("shinyWidgets")
  
  ui <- fluidPage(
    br(),
    actionGroupButtons(
      inputIds = c('OvCond_up', 'OvCond_down'),
      labels = list(tags$span(icon('arrow-up'), ''), 
                    tags$span(icon('arrow-down'), '')),
      status = c('success', 'danger'),
    ),
    verbatimTextOutput(outputId = 'OverallCondValue'),

  )
  
  server <- function(input, output, session) {
    
    
    output$OverallCondValue <- renderPrint(input$OvCond_up)
    
    
    
  }
  
  shinyApp(ui = ui, server = server)
}