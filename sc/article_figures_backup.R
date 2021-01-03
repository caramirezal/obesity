## obese analysis on the roll of inflammation in obesity 
## based on a cohort of eutrofic and obese patients

## Dependencies
library(googlesheets)   ## connection to google spreadsheets
library(dplyr)          ## Data processing
library(ggplot2)        ## Plotting
library(VIM)            ## Variable imputation
library(missForest)     ## imputation using random forest
library(Hmisc)

## loading obese chorot data from google spreadsheet
obese <- gs_title('ObesidadHGMcompleto20191004') %>%
              gs_read('BASE DE DATOS A')

## Removing annoying non alpha numeric characters in names
colnames(obese) <- gsub('[^[:alnum:]]', '', colnames(obese))

## obese data preprocessing
## removing empty spaces in col names
names(obese) <- gsub(' ', '_', names(obese)) 
## dropping non sense variables as ids
obese.p <- select(obese, 
                  LymphocytesCount:LogCD8TEMQ3HLADRCD38MedianPEDZL
                 )   %>%
        select_if(is.numeric)

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




