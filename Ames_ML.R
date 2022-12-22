library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(astsa)
library(tsdl)

df_housing = read.csv('./data/Ames_Housing_Price_Data.csv')
df_estate = read.csv('./data/Ames_Real_Estate_Data.csv')

df_housing
df_estate
