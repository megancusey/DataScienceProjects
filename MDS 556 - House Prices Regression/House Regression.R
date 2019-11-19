setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')

## data preprocessing
source("data preprocessing.R", local = TRUE)
data <- dataPreprocessing()

## feature engineering
source("feature engineering.R", local = TRUE)
model.data <- featureEngineering(data)
