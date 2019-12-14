rm(list = ls())
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')

## SECTION 1 - data preprocessing
## OBJECTIVE - clean each feature according to data definitions
source("data preprocessing.R", local = TRUE)

options(scipen = 999)

data <- read.csv(file='train.csv', header=TRUE, stringsAsFactors = FALSE)
data_raw <- data

## cleaner - apply validations according to data definitions
data_clean <- data_cleaner(data_raw)
data <- data_clean
rm("data_raw")

## sapply(data_FS,function(x) sum(is.na(x)))
## No missing values

## DATA EXPLORATION - See Data Exploration.R
source("data exploration.R", local = TRUE)

## feature engineering
source("feature engineering.R", local = TRUE)
## create a version of the data w/ feature selection applied.
data_FE <- featureEngineering(data_clean)
data <- data_FE
rm("data_clean")
rm("data_FE")

important_vars <- RFE(data)
k <- 8
keep_vars <- head(important_vars,k)
keep_vars <- c(keep_vars,"Target")
data_FS <- data[keep_vars]
train <- splitData("train", 0.8, data_FS)
test <- splitData("test", 0.8, data_FS)


##numeric_correlation_plot(data)
##importantvariables_rf(data)

##write.csv(model.data,"output model data.csv")

## linear regression
## source("linear regression.R", local = TRUE)
## linear.regression <- linearRegression(train)
## linear.regression
## w/ top 1 Variables: ADJ R-Squared = .6931
## w/ top 2 Variables: ADJ R-Squared = .7399
## w/ top 3 Variables: ADJ R-Squared = .807
## w/ top 4 Variables: ADJ R-Squared = .8215
## w/ top 5 Variables: ADJ R-Squared = .8277
## w/ top 6 Variables: ADJ R-Squared = .8338
## w/ top 7 variables: ADJ R-Squared = .8535
## w/ top 8 variables: ADJ R-Squared = .8537
## write.csv(data_FS, file="FE.csv", row.names = FALSE)
## summary(data_FS$Target)
## important_vars
## train$Target_Category = +(train$Target >= 214000)
## test$Target_Category = +(test$Target >= 214000)



## train.actual_observations <- train$Target
## test.actual_observations <- test$Target

## train$Target <- NULL
## test$Target <- NULL

## logistic regression
## For logistic regression, qualtative variables need to be turned
## into binary variables. For week 4, I'm just going to exclude
## categorical variables and come back to it for the final project.
## log_reg.train <- train
## log_reg.test <- test

##Feature Scaling?
## log_reg.train$TotalSF = scale(log_reg.train$TotalSF)
## log_reg.train$OverallQual2 = scale(as.numeric(log_reg.train$OverallQual2))
## log_reg.train$YearBuilt = scale(as.numeric(log_reg.train$YearBuilt))
## log_reg.train$GarageCars = scale(as.numeric(log_reg.train$GarageCars))
## log_reg.train$LotArea = scale(as.numeric(log_reg.train$LotArea))
## log_reg.train$KitchenQual = scale(as.numeric(log_reg.train$KitchenQual))
## log_reg.train$GarageType = scale(as.numeric(log_reg.train$GarageType))
## log_reg.train$FireplaceQu = scale(as.numeric(log_reg.train$FireplaceQu))
## log_reg.test$TotalSF = scale(log_reg.test$TotalSF)
## log_reg.test$OverallQual2 = scale(as.numeric(log_reg.test$OverallQual2))
## log_reg.test$YearBuilt = scale(as.numeric(log_reg.test$YearBuilt))
## log_reg.test$GarageCars = scale(as.numeric(log_reg.test$GarageCars))
## log_reg.test$LotArea = scale(as.numeric(log_reg.test$LotArea))
## log_reg.test$KitchenQual = scale(as.numeric(log_reg.test$KitchenQual))
## log_reg.test$GarageType = scale(as.numeric(log_reg.test$GarageType))
## log_reg.test$FireplaceQu = scale(as.numeric(log_reg.test$FireplaceQu))

## fit training set
## logistic_classifier = glm(formula = Target_Category ~ . ,
##                  family=binomial,
##                  data=log_reg.train)

## logistic_classifier

## Predicting Test Set Results
## prob_pred  = predict(logistic_classifier, type="response", newdata = log_reg.test[-9])
## prob_pred
## y_pred = ifelse(prob_pred > .95, 1,0)

## Making the Confusion Matrix
## cm = table(test$Target_Category,y_pred)
## cm

## library(e1071)
## SUPPORT VECTOR MACHINE
## svm_classifier <- svm(formula = Target_Category ~ .,
##                       data=log_reg.train,
##                       type="C")

## Predicting Test Set Results
## prob_pred_svm  = predict(svm_classifier, type="response", newdata = log_reg.test[-9])
## prob_pred_svm


## Making the Confusion Matrix
## svm_cm = table(test$Target_Category,prob_pred_svm)
## svm_cm

## SUPPORT VECTOR MACHINE - Kernal
## svm_classifier_k <- svm(formula = Target_Category ~ .,
##                       data=log_reg.train,
##                       type="C",
##                       kernal="sigmoid")


## Predicting Test Set Results
## prob_pred_svm_k  = predict(svm_classifier_k, type="response", newdata = log_reg.test[-9])
## prob_pred_svm_k


## Making the Confusion Matrix
## svm_cm_k = table(test$Target_Category,prob_pred_svm_k)
## svm_cm_k

