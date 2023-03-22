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
# devtools::install_github("antonmalko/shinyRadioMatrix")
# library(shinyRadioMatrix)

df_predictions = read.csv("./shiny_data/home_flipping.csv")
# df_predictions2 = read.csv("./flipping_predictions.csv") # you may not need this if you alter df_predictions to not have garagearea
# undervalued25 = read.csv('./undervalued_homes.csv')
df_coefs = read_csv('./shiny_data/final_predictions.csv')
df_feats = read.csv('./shiny_data/final_features.csv')
df_places = read.csv('./shiny_data/Ames_places.csv')
df_demo = read.csv('./shiny_data/Ames_Demographics.csv')
df_summary = read.csv('./shiny_data/lasso_coeff.csv')
# source('reactive_functions.R')


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