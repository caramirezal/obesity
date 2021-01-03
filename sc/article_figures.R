## obese analysis on the roll of inflammation in obesity 
## based on a cohort of eutrofic and obese patients

## Dependencies
library(tidyverse)
library(VIM)            ## Variable imputation
library(missForest)     ## imputation using random forest
library(Hmisc)


## Reading data
obese <- read.table('../data/Data_obesidadHGM_de trabajo.xlsx - BASE DE DATOS A.tsv', 
                    sep = '\t', header = TRUE,
                    stringsAsFactors = FALSE)
str(obese)

## Removing annoying non alpha numeric characters in names
colnames(obese) <- gsub('[^[:alnum:]]', '', colnames(obese))

## obese data preprocessing
## removing empty spaces in col names
names(obese) <- gsub(' ', '_', names(obese))
names(obese) <- gsub('\\.', '_', names(obese)) 
## dropping non sense variables as ids
obese.p <- obese[, 3:ncol(obese)]  

###########################################################################
## Assessing the number of NAs in data

## visualising the content of NAs
aggr(obese.p, 
     col=c('navyblue','yellow'),
     numbers=TRUE, 
     sortVars=TRUE,
     labels=names(obese.p), 
     cex.axis=.7,
     gap=3, 
     ylab=c("Missing data","Pattern"))


## calculating the fraction of NAs in cols
na_fracts <- apply(obese.p, 2, function(x) sum(is.na(x)) / length(x))

## removing cols with fraction of NAs > 0.13
threshold <- 0.13
obese.filtered <- obese.p[, na_fracts < threshold]

#############################################################################
## Assessing variable imputation

patient_type <- obese.filtered$Tipodepaciente
obese.filtered <- select(obese.filtered, 
                         -Tipodepaciente,
                         -Clave) %>%
                  select_if(is.numeric)
obese.m <- as.matrix(obese.filtered)

## Imputing data using random forest
obese.imputed <- missForest(obese.m)
obese.imputed$OOBerror

## Imputation with median values
obese_Hmisc <- as.data.frame(sapply(obese.filtered, 
                                        impute, mean))

write.table(as.data.frame(obese.imputed$ximp), 
            '../data/obesity_forest_imputed.tsv',
            row.names = FALSE,
            sep = '\t')

write.table(obese_Hmisc, 
            '../data/obesity_mean_imputed.tsv',
            row.names = FALSE,
            sep = '\t')






