library (shiny)
library (shinydashboard)
library (shinydashboardPlus)
library (ggplot2)
library (leaflet)
library (date)
library (tidyr)
library (dplyr)
library (data.table)
library (zoo)
library (tibble)
library (billboarder)
library (scales)
library (highcharter)
library (quantmod)
library (gplots)
library (RColorBrewer)
library (plotrix)
library (RODBC)
library (png)
library (rpivotTable)
library (lubridate)
library (timeDate)
library (shinycssloaders)
library (shinyjs)
library (DT)
library (rintrojs)
library (profvis)
library (bit64)
library (collapsibleTree)

rm(list=ls())

###########################/ui.R/##################################

#Header----
header <- dashboardHeaderPlus(
  title = tagList(
    span(class = "logo-lg", "MRO Dash"),
    imageOutput("HLogo")),
  tags$li(class = "dropdown",
          tags$a(htmlOutput("Refresh"))
  ),
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "sliders"
)

#Right SideBar----
rightsidebar <- rightSidebar()

#SideBar----
sidebar <- dashboardSidebar(
  #Sidebar Menu----
  div(id = "sidebarChoices",
      #style = "position: fxed; overflow: visible;", 
      sidebarMenu(id = "menuChoice",
                  menuItem("Functional Dashboards", tabName = "MetricMenu", icon = icon("dashboard"),
                           menuSubItem("Operations", tabName = "OpsMetricSubMenu", icon = icon("angle-double-right"))
                  )
      )
  )
  
  
  
  #End )----
) #dashboard sidebar end

#Body----
body <- dashboardBody(
  useShinyjs(),
  #CSS Formatting----
  #Background colors----
  #tags$head(tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto;}"))),
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")), 
  
  #   /* other links in the sidebarmenu when hovered */
  # .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #E4551F;}
  tags$head(tags$style(HTML('
                            /*** FORMATTING BACKGROUND COLORS ***/

                            /* Top Left of Header Background */
                            .skin-blue .main-header .logo {background-color: #000000;}

                            /*Top Left of Header when Hovered */
                            .skin-blue .main-header .logo:hover {background-color: #E4551F;}

                            /* Rest of the Header Background */
                            .skin-blue .main-header .navbar {background-color: #000000;}

                            /* Main SideBar Background */
                            .skin-blue .main-sidebar {background-color: #1A1A1A;}

                            /* Tabs in SideBar Background */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #1A1A1A;}

                            /* Active Tab in SideBar Background */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #E4551F;}

                            /* Left bar on Sidebar */
                            .skin-blue .sidebar-menu > li.active > a {border-left-color: #E4551F;}
                            .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a {border-left-color: #E4551F;}

                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #E4551F;}

                            /* Right SideBar Background */
                            .control-sidebar-dark+.control-sidebar-bg {background: #1A1A1A;}
                            .control-sidebar-dark+.nav.nav-tabs.nav-justified.control-sidebar-tabs {background: #1A1A1A;}
                            .control-sidebar-dark+.control-sidebar.control-sidebar-dark.control-sidebar-open {background: #1A1A1A;}

                            /* Body Background */ 
                            .content-wrapper, .right-side {background-color: #FFFFFF;}

                            '))),
  
  #Header Logo----
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            padding: 0px 0px;
                            }
                            '))),
  #Boxes----
  tags$head(tags$style(HTML('

                            .box.box-primary{
                            border-top-color:#E4551F;
                            border-bottom-color:#E4551F;
                            border-color: #E4551F
                            border-left-color:#E4551F;
                            border-right-color:#E4551F;
                            }

                            .box.box-solid.box-primary{
                            border-color: #E4551F
                            }

                            .box.box-solid.box-primary>.box-header{
                            background-color: #E4551F;
                            }


                            '))), #.nav.nav-tabs.shiny-tab-input.shiny-bound-input > li[class=active] > a {border-top-color:#E4551F;}
  #Icon----
  tags$style('.fa-plus-square-o {color:#E4551F}'),
  
  
  #OPS Page----
  tags$head(tags$style(HTML("
                            .small-box {background-color: #000000 !important;border-radius: 1vh !important; box-shadow: 0.3vh 0.3vh 0vh #CCCCCC;}
                            .small-box .icon-large {font-size: 8vh !important; bottom: -2vh !important; color: #999999 !important;}
                            .small-box h3 {font-size: 4vh !important;}
                            .small-box p {font-size: 1vh !important; color: #FFFFFF !important;}

                            .white .small-box h3{color: #FFFFFF !important;}
                            .yellow .small-box h3{color: #F6FC00 !important;}
                            .red .small-box h3{color: #D20000 !important;}

                            #DailyLinearityShip {height:25vh !important;}
                            #MonthlyLinearityShip {height:25vh !important;}
                            "))),
  
  
  #OPERATIONS KPI----
  tabItem(tabName = "OpsMetricSubMenu",
          #First Row: KPI Metrics----
          div(id = "Ops_FirstRow", 
              fluidRow(
                valueBoxOutput("Box1", width = 2),
                valueBoxOutput("Box2", width = 2),
                valueBoxOutput("Box3", width = 2),
                valueBoxOutput("Box4", width = 2)
              )
          ),
          #Third Row: Linearity----
          fluidRow(
            div(id = "DailyLinearityBox",
                box(
                  title = "Daily Shipment Linearity", status = "primary", solidHeader = FALSE,
                  highchartOutput("DailyLinearityShip") %>% withSpinner(color="#E4551F")
                )
            ),
            div(id = "MonthlyLinearityBox",
                box(
                  title = "Monthly Shipment Linearity", status = "primary", solidHeader = TRUE,
                  highchartOutput("MonthlyLinearityShip") %>% withSpinner(color="#E4551F")
                )
            )
          ),
          #Fourth Row: WIP----   
          div(id = "Ops_FourthRow", 
              fluidRow(
                div(id = "TimingBox",
                    tabBox(id = "Timing",
                           title = p("WIP Status",actionLink("WIPOnTimeLink", NULL, icon = icon("plus-square-o"))), width = 4
                    )
                )
              )
          )
  )
)
#Builds Dashboard Page----
ui <- dashboardPagePlus(header, sidebar, body, rightsidebar)

###########################/server.R/###############################
server <- function(input, output, session) {
  
  output$Box1 <- renderValueBox({
    
    Value <- 50
    
    lapply(c("white", "yellow", "red"), function(i) removeClass("Box1", i))
    
    if (Value <= 100 & Value >= 90) {Color = "white"
    } else if (Value < 90 & Value >= 80) {Color = "yellow"
    } else if (Value < 80) {Color = "red"
    } else {Color = "white"}
    
    addClass("Box1", Color)
    valueBox(value = paste0(Value, "%"), subtitle = "OTD DIH Commercial MTD /Goal: 90%", icon = icon("plane"), href = "#")
  })
  
  output$Box2 <- renderValueBox({
    
    Value <- 85
    
    lapply(c("white", "yellow", "red"), function(i) removeClass("Box2", i))
    
    if (Value <= 100 & Value >= 90) {Color = "white"
    } else if (Value < 90 & Value >= 80) {Color = "yellow"
    } else if (Value < 80) {Color = "red"
    } else {Color = "white"}
    
    addClass("Box2", Color)
    CommercialOTDBox <- valueBox(value = paste0(Value, "%"), subtitle = "OTD DIH Commercial MTD /Goal: 90%", icon = icon("plane"), href = "#")
    return(CommercialOTDBox)
  })
  
  output$Box3 <- renderValueBox({
    
    Value <- 110
    
    lapply(c("white", "yellow", "red"), function(i) removeClass("Box3", i))
    
    if (Value <= 100 & Value >= 90) {Color = "white"
    } else if (Value < 90 & Value >= 80) {Color = "yellow"
    } else if (Value < 80) {Color = "red"
    } else {Color = "white"}
    
    addClass("Box3", Color)
    CommercialOTDBox <- valueBox(value = paste0(Value, "%"), subtitle = "OTD DIH Commercial MTD /Goal: 90%", icon = icon("plane"), href = "#")
    return(CommercialOTDBox)
  })
  
  output$Box4 <- renderValueBox({
    
    Value <- 98
    
    lapply(c("white", "yellow", "red"), function(i) removeClass("Box4", i))
    
    if (Value <= 100 & Value >= 90) {Color = "white"
    } else if (Value < 90 & Value >= 80) {Color = "yellow"
    } else if (Value < 80) {Color = "red"
    } else {Color = "white"}
    
    addClass("Box4", Color)
    CommercialOTDBox <- valueBox(value = paste0(Value, "%"), subtitle = "OTD DIH Commercial MTD /Goal: 90%", icon = icon("plane"), href = "#")
    return(CommercialOTDBox)
  })
  
  
  output$MonthlyLinearityShip <- renderHighchart({
    
    SumIntake <- c(5,10,15,20,20,20,25,30,35,40,45,45,45)
    SumShip <- c(6,12,14,20,20,20,22,28,33,42,44,50,55)
    GoalShip <- c(7,14,21,25,25,25,30,35,40,45,55,60, 65)
    Index <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
    
    Linearity <- data.frame(SumIntake,SumShip,GoalShip,Index)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = Linearity$Index, labels = list(style = list(fontSize = "1.2vh"))) %>%
      hc_yAxis(gridLineWidth = 0, labels = list(style = list(fontSize = "1.2vh"))) %>%
      hc_add_series(data  = Linearity$SumIntake, name = "Intakes",  color = "#E4551F") %>%
      hc_add_series(data  = Linearity$SumShip, name = "Shipments",  color = "#000000") %>%
      hc_add_series(data = Linearity$GoalShip, name = "Plan", type = "line",  color = "#F2A900") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
      hc_legend(enabled = TRUE, verticalAlign = "top") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Day {point.x}</b><br>", allowDecimals = FALSE)
    
  })
  
  output$DailyLinearityShip <- renderHighchart({
    
    SumShip <- c(6,12,14,20,20,20,22,28,33,42,44,50,55)
    GoalShip <- c(7,14,21,25,25,25,30,35,40,45,55,60, 65)
    Index <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
    
    Linearity <- data.frame(SumShip,GoalShip,Index)
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = Linearity$Index, labels = list(style = list(fontSize = "1.2vh"))) %>%
      hc_yAxis(gridLineWidth = 0, labels = list(style = list(fontSize = "1.2vh"))) %>%
      hc_add_series(data  = Linearity$SumShip, name = "Shipments",  color = "#000000") %>%
      hc_add_series(data = Linearity$GoalShip, name = "Plan", type = "line",  color = "#F2A900") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
      hc_legend(enabled = TRUE, verticalAlign = "top") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Day {point.x}</b><br>", allowDecimals = FALSE)
    
  })
  
}

#Combines Dasboard and Data together----
shinyApp(ui, server)