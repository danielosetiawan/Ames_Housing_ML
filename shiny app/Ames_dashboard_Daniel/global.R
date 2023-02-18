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