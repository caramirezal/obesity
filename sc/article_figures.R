## obese analysis on the roll of inflammation in obesity 
## based on a cohort of eutrofic and obese patients
setwd(getwd())

## Dependencies
library(googlesheets)
library(dplyr)
library(ggplot2)

## loading obese chorot data from google spreadsheet
obese <- gs_title('ObesidadHGMcompleto20191004') %>%
              gs_read('BASE DE DATOS A')

## obese data preprocessing
## removing empty spaces in col names
names(obese) <- gsub(' ', '_', names(obese)) 
## dropping non sense variables as ids
obese.p <- select(obese, 
                 -FCS,
                 -CLAVE,
                 -contains('FECHA')
                 )    %>%
          mutate(
                  ## defining obese category from Tipo de paciente col
                 obesity = grepl('obeso', tolower(Tipo_de_paciente)),
                  ## getting adult category from tipo de paciente col
                 adult = grepl('adult', tolower(Tipo_de_paciente))
          )

vals <- with(obese.p,
    tapply(NEUTROFILOS, obesity, as.vector)        
)
tt <- t.test(vals$'FALSE', vals$'TRUE')
tt$p.value

with(obese.p,
     boxplot(NEUTROFILOS ~ obesity,
             col = c('green', 'red'))
)
