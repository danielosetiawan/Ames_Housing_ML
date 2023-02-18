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
                    actionBttn(
                      inputId = "live",
                      label = icon('house'),
                      style = "stretch",
                      color = "success"
                    ),
                  
                    actionBttn(
                      inputId = "flip",
                      label = icon('hammer'),
                      style = "stretch",
                      color = "default"
                    )
                  )
