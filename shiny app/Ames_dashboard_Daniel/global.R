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

df_predictions = read.csv("./home_flipping.csv")
undervalued25 = read.csv('./undervalued_homes.csv')
df_coefs = read_csv('./final_predictions.csv')
df_feats = read.csv('./final_features.csv')
df_places = read.csv('./Ames_places.csv')

# source('reactive_functions.R')

### header
rightUi = tags$li(class = "dropdown",
                  
                  tags$li(
                    class = "dropdown", 
                    # fluidRow(
                    actionBttn(
                      inputId = "Id111",
                      label = icon('house'),
                      style = "minimal",
                      color = "success"
                    ),
                    actionBttn(
                      inputId = "Id111",
                      label = icon('hammer'),
                      style = "stretch",
                      color = "primary"
                    )
                        # switchInput(
                        #   inputId = "Id018",
                        #   label = "<i class=\"fa fa-thumbs-up\"></i>"
                        # )
                      # )
                    # )
                  )
                      # style= "width: 60%; margin-left: auto; margin-right: 0px; 
                      # margin-top:-20px; margin-bottom:-20px;")),
                  
                  # tags$li(class = "dropdown", div(
                  #   # selectInput(
                  #   #   inputId = 'address',
                  #   #   label = '',
                  #   #   choices = df_predictions$Prop_Addr,
                  #   # ),
                  #   style= "width: 90%; margin-left: 10px; margin-right: -80px; 
                  #     margin-top:-20px; margin-bottom:-20px;"))
                  
                  
                  
)
