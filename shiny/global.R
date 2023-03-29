library(sf)
library(tigris)
library(mapview)
library(leaflet)
library(shinycssloaders)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(shinyWidgets)
library(DT)
library(htmltools)
library(glue)

df_predictions = read.csv("./shiny_data/home_flipping.csv")
df_coefs = read_csv('./shiny_data/final_predictions.csv')
df_feats = read.csv('./shiny_data/final_features.csv')
df_places = read.csv('./shiny_data/Ames_places.csv')
df_demo = read.csv('./shiny_data/Ames_Demographics.csv')
df_summary = read.csv('./shiny_data/lasso_coeff.csv')


haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 3959 # Earth's radius in miles
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  a <- sin(dLat/2)^2 + sin(dLon/2)^2 * cos(lat1) * cos(lat2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

# neighborhood info
all_nb <- ' Ames is a city located in the state of Iowa in the United States, home to Iowa State University and a vibrant scientific community.'
IDOTTR <- ' is an affordable neighborhood with historic homes and easy access to downtown amenities, parks, and shopping areas.'
NAmes <- ' is an affordable neighborhood with a mix of mid-century and modern homes, mature trees, parks, and easy access to amenities.'
Gilbert <- " is a budget-friendly neighborhood known for its rural charm, community events, and convenient location to the city's amenities."
StoneBr <- ' is a mid-range neighborhood with newer homes, community pools and parks, and access to amenities such as shopping and dining.'
NWAmes <- ' is a family-friendly neighborhood known for its affordable homes, parks, and proximity to a variety of shopping / dining areas.'
Somerst <- ' is an upscale neighborhood with modern and traditional homes, walking trails, parks, and easy access to shopping and dining.'
BrDale <- ' is an affordable neighborhood with mid-century homes, mature trees, and access to amenities such as parks, shopping, and dining.'
NPkVill <- ' is a budget-friendly neighborhood known for its diverse housing options, green spaces, and proximity to shopping and dining.'
NridgHt <- ' is an upscale neighborhood with newer homes, walking trails, community pools and parks, and easy access to amenities and shopping.'
Blmngtn <- ' is a mid-range neighborhood with a mix of housing options, parks, and easy access to amenities such as shopping and dining.'
NoRidge <- ' is an upscale neighborhood with modern and traditional homes, walking trails, parks, and easy access to shopping and dining.'
SawyerW <- ' is an affordable neighborhood with a mix of mid-century and modern homes, parks, and easy access to many city amenities.'
Sawyer <- ' is an affordable neighborhood  known for its diverse housing options, green spaces, and convenient location to many city amenities.'
Veenker <- ' is an upscale neighborhood with large, traditional homes, mature trees, and easy access to amenities such as shopping and dining.' 
Greens <- ' is a mid-range neighborhood with a mix of housing options, parks, and easy access to amenities such as shopping and dining.'
BrkSide <- ' is a budget-friendly neighborhood known for its small, mid-century homes, parks, and convenient location to many amenities.' 
OldTown <- ' is an affordable neighborhood with a mix of historic and modern homes, a vibrant downtown area, and easy access to parks and trails.'
ClearCr <- ' is a budget-friendly neighborhood known for its small, mid-century homes, parks, and easy access to amenities and shopping areas.'
Edwards <- ' is an affordable neighborhood with a mix of mid-century and modern homes, parks, and easy access for shopping and dining.'
SWISU <- ' is a budget-friendly neighborhood known for its diverse housing options, parks, and convenient location to many shopping areas.'
CollgCr <- ' is a mid-range neighborhood with newer homes, community pools and parks, and easy access to amenities such as shopping and dining.'
Crawfor <- ' is an affordable neighborhood with a mix of small, mid-century homes, parks, and easy access to amenities such as shopping and dining.'
Mitchel <- ' is a budget-friendly neighborhood known for its diverse housing options, parks, and convenient location to shopping areas.'
Timber <- ' is a mid-range neighborhood with a mix of housing options, community pools and parks, and easy access for shopping and dining.'
MeadowV <- ' is an affordable neighborhood with small, mid-century homes, parks, and convenient location to amenities and shopping areas'


# ------------------------------
# Search Tab
# ------------------------------

search_tab <- tabPanel(title = 'Search', fluidRow(
  column(
    width = 6,
    leafletOutput('map', height=400) 
  ),
  column(
    width = 6,
    fluidRow(
      style='margin-top: -20px',
      column(
        width = 6,
        tags$p('Neighborhood', style='font-size: 80%; 
                           font-style: italic; margin-left: 0px; margin-top: 0px;
                           margin-bottom: -20px'),
        pickerInput(
          inputId = 'neighborhood', 
          label = tags$p('', style='margin-top: -80px'),
          choices = c('All Neighborhoods', 
                      unique(df_predictions$Neighborhood)),
          selected = 'Neighborhood'),
        fluidRow(
          tags$head(
            tags$style(
              ".fa { color: black}; .fa-3x {font-size: 300%}")),
          column(
            width = 6,
            infoBoxOutput('income', width = NULL)),
          column(
            width = 6,
            infoBoxOutput('appreciation', width = NULL)),
          column(
            width = 12,
            infoBoxOutput('nb_info', width = NULL)))),
        column(
          width = 6,
          style = 'margin-top: -2px',
          fluidRow(
            column(
              width = 6,
              tags$p('Property:', style='font-size: 80%; 
                               font-style: italic; margin-left: 0px; margin-top: 0px;
                               margin-bottom: -25px')),
          column(
            width = 6,
            style = 'text-align: right',
            uiOutput('property_label'))),
        pickerInput(
          inputId = 'address',
          label = tags$p('', style='margin-top: -95px; 
                                     margin-bottom: 5px'),
          choices = df_predictions$Prop_Addr,
          options = pickerOptions(
            `live-search` = TRUE,
            virtualScroll = TRUE,
            dropupAuto = FALSE)),
        valueBoxOutput('property', width=NULL),
      )))))

# ------------------------------
# Compare Tab
# ------------------------------

compare_tab <- tabPanel(title = 'Compare', fluidRow(
  column(width = 5,
    pickerInput(
      inputId = 'compare_1',
      label = 'Property #1',
      choices = df_predictions$Prop_Addr,
      selected = '209 HARTFORD DR',
      options = pickerOptions(
        `live-search` = TRUE,
        virtualScroll = TRUE,
        dropupAuto = FALSE)),
    leafletOutput('map1', height=300)),
  column(width = 2,
    style = "text-align:center;",
    checkboxGroupButtons(
      inputId = "check_box",
      label = "",
      choices = list("Shops" = 1, 
                    "Restaurants" = 2, 
                    "Parks" = 3,
                    "Church" = 4),
      direction = 'vertical',
      status = "info",
      checkIcon = list(
      yes = icon("ok", lib = "glyphicon")))),
  column(width = 5,
    pickerInput(
      inputId = 'compare_2',
      label = 'Property #2',
      choices = df_predictions$Prop_Addr,
      selected = '2527 CLAYTON DR',
      options = pickerOptions(
      `live-search` = TRUE,
      virtualScroll = TRUE,
      dropupAuto = FALSE)),
    leafletOutput('map2', height=300)
  )))

# ------------------------------
# Forecast Tab
# ------------------------------

forecast_tab <- tabPanel(title = 'Forecast', fluidRow(
  column(8, uiOutput(outputId = "sarima")),
  column(4,
    fluidRow(
      column(6, 
      style = 'margin-top: -50px',
      materialSwitch(
        status = 'success',
        inputId = 'ts_ci',
        label = "Confidence Interval",
        value = TRUE,
        right = TRUE),
      ),
      column(6, 
        style = 'margin-top: -80px',
        pickerInput(
          inputId = 'neighborhood_timeseries',
          label = '',
          choices = c('All Neighborhoods',
                      unique(df_predictions$Neighborhood)),
          selected = 'Neighborhood')
        ),
      fluidRow(
        bs4TabCard(
          side='left',
          title = "",
          collapsible = FALSE,
          width=12,
          selected = 'Info',
          tabPanel(
            title='Info',
            uiOutput(outputId = "info")),
          tabPanel(
           title='Trend',
           uiOutput(outputId = "trend")),
          tabPanel(
           title='Seasonality',
           uiOutput(outputId = "season"))
        ))))))

# ------------------------------
# Flip Tab
# ------------------------------

flip_tab <- tabPanel(title = 'Flip', box(
  solidHeader = FALSE,
  collapsible = FALSE,
  title = uiOutput('current_price'),
  background = NULL,
  width = 12,
  uiOutput('predicted_price'),
  tags$p('', style='margin-top:-30px; 
                   font-size: 12px; margin-bottom: -100px; margin-top: -10px;'
  ),
  
  footer = fluidRow(
    column(
      width = 6,
      boxPad(
        color = 'info',
        gradient = TRUE,
        tags$b('How did we come to our conclusion?'),
        tags$br(),tags$br(),
        uiOutput('conclusion'),
        splitLayout(
          cellWidths = c("55%", "45%"),
          div(tableOutput("table"), style = "font-size: 75%; width: 75%"),
          div(plotOutput("rank_bar")), 
          style = "height: 280px;"))),
      column(
        width = 6,
        style='margin-top: -25px',
        fluidRow(
          column(
            width = 4,
            offset = 2,
            style = 'margin-top: -70px;margin-bottom: 0px;',
            tags$p('Neighborhood', style='margin-top: 5px; margin-bottom: -20px;
                  font-style: italic;font-size: 10px;'),
            pickerInput(
              inputId = 'neighborhood_flip', 
              label = '',
              choices = c('All Neighborhoods', 
                          unique(df_predictions$Neighborhood)),
              selected = 'Neighborhood'
          )),
          column(
            width = 6,
            style = 'margin-top: -70px;margin-bottom: 0px;',
            tags$p('Property (ranked by most undervalued)', style='margin-top: 5px; margin-bottom: -20px;
                  font-style: italic;font-size: 10px;'),
            pickerInput(
              inputId = 'address_flip', 
              label = '',
              choices = df_predictions$Prop_Addr
            ))),
        
        boxPad(
          chooseSliderSkin('Flat', color = "#112446"),
          column(
            offset = 6,
            width = 6,
            tags$p('Gross Square Footage', style='margin-bottom: -40px; font-style: italic;font-size: 12px; font-weight: bold')
          ),
          sliderInput(
            inputId = 'sqft_slider',
            label = '',
            min = 1, max = 3000,
            value = 1, step = 10,
            animate =
              animationOptions(interval = 550, loop = TRUE)
          ),
          fluidRow(
            column(
              width = 9,
              fluidRow(
                column(
                  width = 4, 
                  align = 'center',
                  style = 'margin-right: -10px; margin-left: -20px; margin-top: -30px;',
                  uiOutput(outputId = 'bedrooms'),
                  tags$p('BEDS', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px;'),
                  uiOutput(outputId = 'kitchen'),
                  tags$p('KITCHEN', style='font-size: 12px; margin-top: -20px;')
                ),
                column(
                  width = 4,
                  align = 'center',
                  style = 'margin-right: -10px;margin-top: -30px;',
                  uiOutput(outputId = 'bathrooms'),
                  tags$p('BATHS', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px;'),
                  uiOutput(outputId = 'basement'),
                  tags$p('BASEMENT', style='font-size: 12px; margin-top: -20px;')
                ),
                column(
                  width = 4,
                  align = 'center',
                  style = 'margin-right: -10px;margin-top: -30px;',
                  uiOutput(outputId = 'quality'),
                  tags$p('QUALITY', style='font-size: 12px; margin-top: -20px; margin-bottom: -10px'),
                  uiOutput(outputId = 'garage'),
                  tags$p('GARAGE', style='font-size: 12px; margin-top: -20px;')
                ))),
            column(
              width = 3,
              style='margin-top: -25px; margin-left: -25px;',
              uiOutput('months', inline=TRUE),
              tags$p('SELL MONTH', style='margin-bottom: -40px; 
                         font-size: 12px'))),
          rightBorder = FALSE,
          marginBottom = FALSE
        )))))

# ------------------------------
# Data Tab
# ------------------------------

data_tab <- tabPanel(title = 'Data',
  dataTableOutput(
    outputId = 'df',
    height = "100%")
)

# ------------------------------
# About Page
# ------------------------------

about_me <- function(name, github, linkedin, image, output_id) {
  column(
    width = 4,
    fluidRow(
      box(
        title = div(
          a(href = paste0('https://github.com/', github),
            icon('github')),
          a(href = paste0('https://www.linkedin.com/in/', linkedin),
            icon('linkedin'))),
        width = 12,
        status = 'primary',
        boxProfile(
          image = paste0('./img/about_me/', image),
          title = name,
          subtitle = 'Data Science Fellow',
          bordered = TRUE,
          uiOutput(align = 'center',
                   outputId = output_id)
        )
      )
    )
  )
}

about_project <- column(
  width = 4,
    fluidRow(
      box(
        title = div(
        ),
        width = 12,
        status = 'warning',
        boxProfile(
          title = 'About Project',
          bordered = TRUE,
          uiOutput(align = 'center',
                   outputId = 'about_project')
        )
      )
    )
  )

about_tab <- tabPanel(title = 'About',
  fluidRow(
    about_project,
    about_me('Laurel He', 'LaurelHe1', 
             'cheng-laurel-he-b04a59104', 
             'laurel.jpg', 'laurel_bio'),
    about_me('Daniel Setiawan', 'set-one', 
             'danielosetiawan', 
             'daniel_s.jpeg', 'daniels_bio'),
  )
)