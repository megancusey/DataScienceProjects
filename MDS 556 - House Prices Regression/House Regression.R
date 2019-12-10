rm(list = ls())
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')

## data preprocessing
source("data preprocessing.R", local = TRUE)
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')
options(scipen = 999)

data <- read.csv(file='train.csv', header=TRUE, stringsAsFactors = FALSE)
data_raw <- data

## DATA EXPLORATION

## cleaner - apply validations according to data definitions
data_clean <- data_cleaner(data_raw)


## data exploration

plot(data$MSSubClass)
hist(data$MSSubClass)
plot(sort(data$MSZoning, decreasing=FALSE))
data <- dataPreprocessing()

## feature engineering
source("feature engineering.R", local = TRUE)
model.data <- featureEngineering(data)

write.csv(model.data,"output model data.csv")
## linear regressio
source("linear engineering.R", local = TRUE)
linear.regression <- linearRegression(model.data)

## logistic regression
## For logistic regression, qualtative variables need to be turned
## into binary variables. For week 4, I'm just going to exclude
## categorical variables and come back to it for the final project.
source("logistic regression.R", local = TRUE)
logistic.regression <- logisticRegression(model.data)
