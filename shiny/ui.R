library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(bs4Dash)
library(bslib)
library(thematic)
library(waiter)

css = HTML("
  .vbox1 .small-box {
    background-color: #FFFFFF !important;
  }
  .vbox2 .small-box {
    background-color: #F6FC00 !important;
  }
  .vbox3 .small-box {
    background-color: #D20000 !important;
  }
  .vbox4 .small-box {
    background-color: #D20000 !important;
  }
")


thematic_shiny()

spinner = 'double-bounce'

dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), 
                   color = "#343a40"),
  dark = TRUE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  dashboardHeader(
    'Dashboard: Ames, IA'
  ),
  dashboardSidebar(
    disable = TRUE
    ),
  dashboardBody(
# ------------------------------
# Menu Item: Homepage
# ------------------------------
      
    fluidPage(
          
      # ------------------------------
      # Homepage: Main Panel
      # ------------------------------
          
      fluidRow(
        useShinyjs(),
        uiOutput('pointer'),
        
        valueBoxOutput('valuebox1', width=3),
        valueBoxOutput('valuebox2', width=3),
        valueBoxOutput('valuebox3', width=3),
        valueBoxOutput('valuebox4', width=3),
      ),

      bs4TabCard(
        id = "tabcard",
        side='left',
        title = "",
        selected = 'Search',
        collapsible = FALSE,
        width = 12,
        search_tab,
        compare_tab,
        forecast_tab,
        flip_tab,
        data_tab,
        about_tab
      )
    )
  )
)
