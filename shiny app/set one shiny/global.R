# AirBNB RProject

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


location = na.omit(read.csv("./../../data/Ames_loc.csv"))
df = read.csv('./../../data/cleaned_housing.csv')
housing = merge(df, location, by="PID")
df_predictions = read.csv("./../../data/Predicted_vs_Sales.csv")
undervalued25 = read.csv('./undervalued25.csv')
