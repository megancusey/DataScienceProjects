rm(list = ls())
## load in user defined functions.
##backwardElimination <- function(x, sl) {

##  numVars = length(x)
##  for (i in c(1:numVars)){
##    regressor = lm(formula = SalePrice ~ ., data = x)
##    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
##    if (maxVar > sl){
##      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
##      x = x[, -j]
##    }
##    numVars = numVars - 1
##  }
##  return(summary(regressor))
##}
featureEngineering <- function(data){

## comment this code after I'm satisfied with 
## feature engineering portion
  
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')
source("data preprocessing.R", local = TRUE)
x <- dataPreprocessing()
sl <- .05
##sapply(data,function(x) sum(is.na(x)))

## Backwards Elimination
numVars = length(x)

for (i in c(1:numVars)){
  regressor = lm(formula = SalePrice ~ ., data = x)
  maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
  if (maxVar > sl){
    j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
    x = x[, -j]
  }
  numVars = numVars - 1
  }

}

summary(regressor)
