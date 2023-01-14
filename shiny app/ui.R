library(shiny)
library(shinydashboard)





# # Define UI for application

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Homepage", 
        tabName = "homepage", 
        icon = icon("landmark"),
        menuSubItem("Maps", tabName = "landmark1", icon = icon("map")),
        menuSubItem("Maps2?", tabName = "landmark2", icon = icon("map"))
      ),
      menuItem("Dream Home", tabName = 'dreamhome', icon = icon('house')),
      menuItem("Home Flipping", tabName = 'flipping', icon = icon('wand-magic-sparkles')),
      menuItem("Machine Learning", tabName = 'ml', icon = icon('gears')),
      menuItem("About", tabName = 'about', icon = icon('user'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "landmark1",
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px);}"),
        box(
          title = "title goes here",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          leafletOutput("map")
        )
      ),
      tabItem(
        tabName = "landmark2",
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px);}"),
        box(
          title = "title goes here",
          collapsible = TRUE,
          width = "100%",
          height = "100%",
          leafletOutput("map2")
        )
      )
    )
  )
)
