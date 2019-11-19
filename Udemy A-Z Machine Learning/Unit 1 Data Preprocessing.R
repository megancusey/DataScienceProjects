getwd()
setwd("C:/Users/cusey/source/repos/DataScienceProjects/Udemy A-Z Machine Learning")

dataset <- read.csv("Data.csv")

## Missing Data in Age and Salary Column
dataset

## Replace missing value by mean of the value
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x,na.rm=TRUE)),
                     dataset$Age
                     )
dataset$Salary = ifelse(is.na(dataset$Salary),
                      ave(dataset$Salary, FUN = function(x) mean(x,na.rm=TRUE)),
                      dataset$Salary)
                      
dataset

##Categorical Data - Encode

dataset$Country = factor(dataset$Country,
                         levels = c('France','Spain','Germany'),
                         labels = c(1,2,3))

dataset$Purchased = factor(dataset$Purchased,
                           levels= c('Yes','No'),
                           labels= c(1,0))

## Create test and train data sets
library(caTools)
set.seed(123)

## Put % in for the training set
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

## Feature Scaling

## Exclude factors from scale
training_set[,2:3] = scale(training_set)
test_set[, 2:3] = scale(test_set)
