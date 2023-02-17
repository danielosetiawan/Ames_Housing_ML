library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # The original sortable
      uiOutput("sortable")
    ),
    mainPanel(
      # The new location for the sortable
      fluidRow(
        column(
          width = 12,
          uiOutput("sortable2")
        )
      )
    )
  )
)

server <- function(input, output) {
  # The original sortable
  output$sortable <- renderUI({
    # Create a sortable table
    table <- data.frame(
      Item = c("Item 1", "Item 2", "Item 3"),
      Value = c(1, 2, 3)
    )
    sortable("sortable_table", container = "table")
  })
  
  # The new location for the sortable
  output$sortable2 <- renderUI({
    # Create a new sortable table
    table <- data.frame(
      Item = c("Item A", "Item B", "Item C"),
      Value = c(10, 20, 30)
    )
    sortable("sortable_table2", container = "table")
  })
  
  # Update the content of the new sortable with the original sortable when the app is started
  observe({
    output$sortable_table2 <- renderUI({
      output$sortable
    })
  })
}

shinyApp(ui, server)
