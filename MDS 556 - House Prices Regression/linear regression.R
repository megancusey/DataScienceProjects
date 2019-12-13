
linearRegression <- function(data){
  
##source("data preprocessing.R", local = TRUE)
##data <- dataPreprocessing()

##train <- splitData("train",.8,data)
##test <- splitData("test",.8,data)

## FITTING MULTIPLE LINEAR REGRESSION TO THE TRAINING SET
regressor = lm(formula = Target ~ .,
               data = data)

##summary(regressor)
## R Creates Dummy variables for the categorical variables which is 
## clearly too much.

## R chose to exclude Alley Graveled when Alley Not Applicable doesn't mean the same
## thing or have equal weight.

## Theres several indicators of significant values here, but I think the 
## amount of dimensions are going to through that off. I think that Lot Frontage, LotARea
## and some of the neighborhoods could be truly significant but there's too much here to tell.

## According to the F statistic, the model does better than a constant model.

## CREATE COLUMN W/ PREDICTION VALUES AGAINST TEST SET
##test$predSalePrice <- predict(regressor, newdata=test)

## CREATE COLUMN W/ PREDICTION VALUES AGAINST TRAIN SET
##train$predSalePrice <- predict(regressor, newdata=train)

## Multiple R-Squared suggests we've explained .6623 of the Y variable (Sale Price)

## filter top K features

##top_k_features <- cutoff.k(ig_values,2)
##f<- as.simple.formula(top_k_features,"SalePrice")
##f
## SalePrice ~ Neighborhood + MSSubClass
## rerun with less dimensions
## FITTING MULTIPLE LINEAR REGRESSION TO THE TRAINING SET
##regressor2 = lm(formula = f,
##               data = train)

##summary(regressor2)

## R-Squared is now .6157 so we took away a lot of dimensions and didn't loose much of
## the variation explained for SalePrice.

return (summary(regressor))
}
