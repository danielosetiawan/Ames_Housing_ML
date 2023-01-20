if (interactive()) {
  
  library("shiny")
  library("shinyWidgets")
  
  ui <- fluidPage(
    tags$h1("knob update examples"),
    br(),
    
    fluidRow(
      
      column(
        width = 6,
        knobInput(
          inputId = "knob1", label = "Update value:",
          value = 75, angleOffset = 90, lineCap = "round"
        ),
        verbatimTextOutput(outputId = "res1"),
        sliderInput(
          inputId = "upknob1", label = "Update knob:",
          min = 0, max = 100, value = 75
        )
      ),
      
      column(
        width = 6,
        knobInput(
          inputId = "knob2", label = "Update label:",
          value = 50, angleOffset = -125, angleArc = 250
        ),
        verbatimTextOutput(outputId = "res2"),
        textInput(inputId = "upknob2", label = "Update label:")
      )
      
    )
  )
  
  server <- function(input, output, session) {
    
    output$res1 <- renderPrint(input$knob1)
    
    observeEvent(input$upknob1, {
      updateKnobInput(
        session = session,
        # inputId = "knob1",
        value = 5
      )
    }, ignoreInit = TRUE)
    
    
    
    output$res2 <- renderPrint(input$knob2)
    observeEvent(input$upknob2, {
      updateKnobInput(
        session = session,
        inputId = "knob2",
        label = input$upknob2
      )
    }, ignoreInit = TRUE)
    
  }
  
  shinyApp(ui = ui, server = server)
  
}