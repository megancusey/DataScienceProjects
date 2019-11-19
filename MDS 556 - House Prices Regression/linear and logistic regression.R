rm(list = ls())
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')

## load in user defined functions.
source("data preprocessing.R", local = TRUE)
reg.data <- dataPreprocessing()
