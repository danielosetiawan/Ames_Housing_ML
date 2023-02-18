if (interactive()) {
  
  library(shiny)
  library(shinyWidgets)
  
  ui <- fluidPage(
    tags$h2("Awesome action button"),
    tags$br(),
    actionBttn(
      inputId = "bttn1",
      label = "Go!",
      color = "primary",
      style = "bordered"
    ),
    tags$br(),
    verbatimTextOutput(outputId = "res_bttn1"),
    tags$br(),
    actionBttn(
      inputId = "bttn2",
      label = "Go!",
      color = "success",
      style = "material-flat",
      icon = icon("sliders"),
      block = TRUE
    ),
    tags$br(),
    verbatimTextOutput(outputId = "res_bttn2")
  )
  
  server <- function(input, output, session) {
    output$res_bttn1 <- renderPrint(input$bttn1)
    output$res_bttn2 <- renderPrint(input$bttn2)
  }
  
  shinyApp(ui = ui, server = server)
  
}
