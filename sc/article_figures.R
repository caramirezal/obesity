## data analysis on the roll of inflammation in obesity 
## based on a cohort of eutrofic and obese patients

## Dependencies
library(googlesheets)
library(dplyr)
library(ggplot2)

## loading data
data <- gs_title('ObesidadHGMcompleto20191004') %>%
              gs_read('BASE DE DATOS A')

## Data preprocessing
data$Obeso
