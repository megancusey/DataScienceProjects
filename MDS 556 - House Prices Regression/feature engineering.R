##rm(list = ls())
## load in user defined functions.
recursiveFeatureElimination <- function(data, sl) {
  library(caret)
  rfe_controller <- rfeControl(functions=lmFuncs, method="repeatedcv", repeats=5, verbose=FALSE)
  size <- c(1:30)
  
  sapply(data,function(x) sum(is.na(x)))
  lm_Profiler <- rfe(data[,1:38]
                     ,data[,39]
                     ,metric = ifelse(is.factor(data[,39]), "Accuracy", "RMSE")
                     ,sizes= size, rfeControl = rfe_controller)
  lm_Profiler
  warnings()
  return(summary(regressor))
}

featureEngineering <- function(data) {
  source("feature engineering.R", local = TRUE)
  ## perform some data manipulation tasks indicated to be done 
  ## prior to backwards feature selection
  return(data <- manual_feature_engineering(data))
  
  #reg.sum <- backwardElimination(data,.05)  
  #reg.sum
}

manual_feature_engineering <- function(data) {

  ## MAKE DECISIONS FROM THE FIRST SET OF SUMMARY RESULTS FROM LINEAR REGRESSION
  ## regressor = lm(formula = SalePrice ~ ., data = data)
  ## summary(regressor)  
  
  ## LETS CUT OUT SOME VARIABLES BEFORE APPLYING FEATURE ENGINEERING
  
  ## FEATURE: NEIGHBORHOODS
  ## SIGNIFICANT NEIGHBOORHOODS:
  ##   STONE BROOK
  ##   NORTHRIDGE HEIGHTS
  ##   NORTHRIDGE
  ##   CRAWFORD
  
  data$StoneBrook = +(data$Neighborhood == "Stone Brooke")
  data$NorthridgeHeights = +(data$Neighborhood == "Northridge Heights")   
  data$Northridge = +(data$Neighborhood == "Northridge") 
  data$Crawford = +(data$Neighborhood == "Crawford")

  ## DROP NEIGHBORHOOD, LOT SHAPE, LAND COUNTOUR, ROOF STYLE DUE TO INSIGNIFICANCE
  drop <- c("Neighborhood","LandContour","LotShape","RoofStyle", "MSZoning")
##  data <- data[ , !(names(data) %in% drop)]  
  
  ## OVERALLCOND - doesn't have a high significance but there are a lot of variables.
  ## What if we split into 3 groups: Above Average, Average, and Below Average?
  
  Above_Average_Vector = c("Excellent","Very Good","Good","Above Average")
  Below_Average_Vector = c("Below Average", "Fair","Poor","Very Poor")
  
  data$OverallCond2 = ifelse(data$OverallCond %in% Above_Average_Vector,"Above Average",
                             ifelse(data$OverallCond %in% Below_Average_Vector, "Below Average",
                                    "Average"))
  

  data$OverallCond2 = factor(data$OverallCond2, ordered = TRUE,
                          levels = c('Below Average',
                                     'Average',
                                     'Above Average'
                          )
                        )
  
  rm("Above_Average_Vector")
  rm("Below_Average_Vector")  
  
  ## Lets do something similar with Overall Quality
  summary(data$OverallQual)
  
  VExcellent_to_VGood = c("Very Excellent","Excellent","Very Good") 
  BAverage_to_VPoor = c("Below Average", "Fair","Poor","Very Poor")
  
  data$OverallQual2 = ifelse(data$OverallQual %in% VExcellent_to_VGood,"Above Average",
                             ifelse(data$OverallCond %in% BAverage_to_VPoor, "Below Average",
                                    "Average"))
  
  
  data$OverallQual2 = factor(data$OverallQual2, ordered = TRUE,
                             levels = c('Below Average',
                                        'Average',
                                        'Above Average'
                             )
  )
  
  rm("VExcellent_to_VGood")
  rm("BAverage_to_VPoor") 
  
  drop <- c(drop,"OverallCond","OverallQual")  
  
  ## EXTERIOR QUALITY - LETS COMBINE SOME THINGS.
  ## ONLY 1 INSTANCE OF POOR, 3 OF EXCELL, 28 OF FAIR.
  ##summary(data$ExterQual)
  Excellent_to_Good = c("Excellent","Good") 
  Fair_to_Poor = c("Fair","Poor")
  
  data$ExterQual2 = ifelse(data$ExterQual %in% Excellent_to_Good,"Above Average",
                             ifelse(data$ExterQual %in% Fair_to_Poor, "Below Average",
                                    "Average"))
  data$ExterQual2 <- factor(data$ExterQual2, ordered=TRUE,
                           levels = c('Below Average',
                                      'Average',
                                      'Above Average'
                                    )
                      )
  
  rm("Excellent_to_Good")
  rm("Fair_to_Poor") 
  
  drop <- c(drop,"ExterQual")

  ## Foundation: Type of Foundation - Lets Combine Slab, Stone,
  ## and Wood into "Others" Category
  ## summary(data$Foundation)
  
  data$Foundation2 <- ifelse(data$Foundation == "BrkTil", "Brick/Tile",
                        ifelse(data$Foundation == "CBlock","Cinder Block",
                          ifelse(data$Foundation == "PConc", "Poured Concrete",
                            "Other")))
  
  data$Foundation2 <- factor(data$Foundation2,
                           levels = c('Brick/Tile',
                                      'Cinder Block',
                                      'Poured Concrete',
                                      'Other'
                                    )
                              )
  
  drop <- c(drop,"Foundation")  
  
  ## Move Sale Price from middle of the dataset to the back
  
  data$SalePriceT <- data$SalePrice
  
  drop <- c(drop,"SalePrice")
  
  data <- data[ , !(names(data) %in% drop)]
  
  return (data)
  
}

##summary(regressor)

##nlevels(data$Neighborhood)
##table.Neighborhood <- table(cut())

##summary(data$Neighborhood)
##barplot(table(data$Neighborhood))
##regressor = lm(formula = SalePrice ~ ., data = data)
##summary(regressor)

## SIGNIFICANT NEIGHBOORHOODS:
## STONE BROOK
## NORTHRIDGE HEIGHTS
## NORTHRIDGE
## CRAWFORD

## CALCULATE THE LOG LIKELIHOOD
loglikelihood <- function(y,py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1,1-1e-12,py))
  
  sum(y*log(pysmooth) + (1-y)*log(1-pysmooth))
}
accuracyMeasures <- function(pred, truth, name="model"){
  ## Normalizethe deviance by th number of data points so we can compare the deviance
  ## across the test and training sets
  dev.norm <- -2*loglikelihood(as.numeric(truth),pred)/length(pred)
  ## Convert the class probability estimator into a classifer 
  ## by labeling documents that score greater than .5 as spam.
  ctable <- table(truth=truth,
                  pred=(pred >.5))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/ sum(ctable[2,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}
splitData <- function(type, ratio, data){
  library(caTools)
  set.seed(100)
  
  
  split <-sample.split(data$SalePrice, SplitRatio=ratio)
  
  if (type == "train") {
    returnData = subset(data, split == TRUE)
    
  } else {
    returnData = subset(data, split==FALSE)
    
  }
  rm("split")
  return (returnData)
  
}