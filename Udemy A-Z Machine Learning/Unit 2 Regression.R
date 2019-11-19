###############################################################
## DATA PREPROCESSING
getwd()
setwd("C:/Users/cusey/source/repos/DataScienceProjects/Udemy A-Z Machine Learning")

dataset <- read.csv("Salary_Data.csv")

## Create test and train data sets
library(caTools)
set.seed(123)

## Put % in for the training set ...Params: Dependent Variable , spit %
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

## Feature Scaling
## NO NEED IN LINEAR REGRESSION, R TAKES CARE OF THIS FOR US
## Exclude factors from scale
##training_set[,2:3] = scale(training_set)
##test_set[, 2:3] = scale(test_set)

################################################################

## REGRESSION
## PARAM: 
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)

## *** indicate high statistical significance
## The lowest the p value, the more highly significant
## the effect is. Usually want it lower than 5% (.05)
summary(regressor)

## Predicting the test set results
.predict <- predict(regressor, newdata=test_set)

.predict

library(ggplot2)
## remove scientic notation
## options(scipen = 999)
ggplot() +
  ## Train
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
              color='red') +
  ## Fitted
    geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata=training_set)),
              color="blue") +
    ggtitle('Salary vs Experience (Training Set') +
    xlab('Years of Experience') +
    ylab('Salary')

ggplot() +
  ## TEST
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color='red') +
  ## Fitted
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata=training_set)),
            color="blue") +
  ggtitle('Salary vs Experience (Test Set') +
  xlab('Years of Experience') +
  ylab('Salary')


##rm(list = ls())