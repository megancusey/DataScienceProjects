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
k <- 1
keep_vars <- head(important_vars,k)
keep_vars <- c(keep_vars,"Target")
data_FS <- data[keep_vars]
train <- splitData("train", 0.8, data_FS)
test <- splitData("test", 0.8, data_FS)


##data_PCA <- PCA(data)

numeric_correlation_plot(data)
importantvariables_rf(data)

##write.csv(model.data,"output model data.csv")

## linear regression
source("linear regression.R", local = TRUE)
linear.regression <- linearRegression(train)
linear.regression
## w/ top 1 Variables: ADJ R-Squared = .6931
## w/ top 2 Variables: ADJ R-Squared = .7399
## w/ top 3 Variables: ADJ R-Squared = .807
## w/ top 4 Variables: ADJ R-Squared = .8215
## w/ top 5 Variables: ADJ R-Squared = .8277
## w/ top 6 Variables: ADJ R-Squared = .8338
## w/ top 7 variables: ADJ R-Squared = .8535
## w/ top 8 variables: ADJ R-Squared = .8537
## logistic regression
## For logistic regression, qualtative variables need to be turned
## into binary variables. For week 4, I'm just going to exclude
## categorical variables and come back to it for the final project.

## source("logistic regression.R", local = TRUE)
## logistic.regression <- logisticRegression(model.data)
