##rm(list = ls())
## load in user defined functions.
RFE <- function(data, sl) {
  set.seed(7)
  
  data_feature_selection <- data
  
  # load the library
  #installed.packages("mlbench")
  library(mlbench)
  library(caret)
  
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results <- rfe(data_feature_selection[,1:66], data_feature_selection[,67], sizes=c(1:10), rfeControl=control)

  ##print(results)  
  # summarize the results
  ##optVars <- head(results$optVariables,5)

  ##optVars <- c(optVars,"Target")
  
  ##summary(results)
  # list the chosen features
  ##predictors(results)
  # plot the results
  ##plot(results, type=c("g", "o"))


  return(results$optVariables)
}

featureEngineering <- function(data) {
  source("feature engineering.R", local = TRUE)
  ## perform some data manipulation tasks indicated to be done 
  ## prior to backwards feature selection

  data_v1 <- multicollinearity(data)
  
  data_v2 <- categorical_Variables(data_v1)
  
  data_v2$Target <- data_v2$SalePrice
  data_v2$SalePrice <- NULL
  
  return(data_v2)

}

## TACKLE THE MULTICOLLINEARITY PROBLEM FOR MANY SQ FT VARIABLES.
multicollinearity <- function(data){
  
  ## Data definition(s)
  ## GrLivArea: Above grade (ground) living area square feet
  ## 1stFlrSF: First Floor square feet
  ## 2ndFlrSF: Second floor square feet
  ## LowQualFinSF: Low quality finished square feet (all floors)
  
  ## The GrLiveArea is just the sum of the 1st and 2nd floor square ft. Check the correlation between variables:
  cor(data$GrLivArea, (data$X1stFlrSF + data$X2ndFlrSF + data$LowQualFinSF))
  ## [1] 1
  
  data$X1stFlrSF <- NULL
  data$X2ndFlrSF <- NULL
  data$LowQualFinSF <- NULL
  
  ggplot(data=data[!is.na(data$SalePrice),], aes(x=GrLivArea, y=SalePrice)) + 
    geom_point(col="red") + geom_smooth(method="lm", se=FALSE, aes(group=1)) + 
    scale_y_continuous(breaks=seq(0,800000, by=100000), labels=comma) 
  
  which(data$GrLivArea > 4500) ##remove outliers
  data <- data[-c(524,1299),]
  
  ## What about Total Basement Sq Ft
  cor(data$TotalBsmtSF, (data$BsmtUnfSF + data$BsmtFinSF2 + data$BsmtFinSF1))
  ## [1] 1
  
  data$BsmtUnfSF <- NULL
  data$BsmtFinSF2 <- NULL
  data$BsmtFinSF1 <- NULL
  
  cor(data$SalePrice,data$GrLivArea)
  ##[1] 0.7349682
   cor(data$SalePrice,data$GrLivArea+data$TotalBsmtSF)
  ## [1] 0.829042
  
  data$TotalSF = data$GrLivArea + data$TotalBsmtSF
  data$TotalBsmtSF <- NULL
  data$GrLivArea <- NULL
  
  ## What about GarageYrBlt & YearBuilt
  cor(data$GarageYrBlt, data$YearBuilt)
  ## [1] 0.8451407
  
  ## Closely Corrolated, I don't think we need Garage Yr Built  
  data$GarageYrBlt <- NULL
  
  ## Garage Area & Garage Cars
  cor(data$GarageArea, data$GarageCars)
  ##  [1] 0.8824754
  
  ## Closely Corrolated, I don't think we need Garage area 
  data$GarageArea <- NULL 
  

  return(data)
  
}



categorical_Variables <- function(data_v1){
  
  summary(data_v1$SalePrice)
 
  ggplot(data[!is.na(data_v1$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
    geom_bar(stat="summary", fun.y="median", fill="red") +
    scale_y_continuous(breaks=seq(0,800000, by=50000), labels=comma) +
    geom_label(stat="count", aes(label= ..count.., y=..count..), size=3) + 
    geom_hline(yintercept=214000, linetype="dashed", color="black")
  
  data_v1$HighEndNeighborhood <- ifelse(data_v1$Neighborhood == "Northridge" | 
                                        data_v1$Neighborhood == "Northridge Heights" |  
                                        data_v1$Neighborhood == "Stone Brook", "Very Rich",
                                     ifelse(
                                        data_v1$Neighborhood == "Somerset" | 
                                        data_v1$Neighborhood == "Veenker" |  
                                        data_v1$Neighborhood == "Timberland", "Rich", "Other"
                                     ))
  data_v1$HighEndNeighborhood <- factor(data_v1$HighEndNeighborhood,ordered=TRUE, level=c("Other","Rich","Very Rich"))
  data_v1$Neighborhood <- NULL
  
  ## Foundation: Type of Foundation - Lets Combine Slab, Stone,
  ## and Wood into "Others" Category
  ## summary(data$Foundation)
  
  data_v1$Foundation2 <- ifelse(data_v1$Foundation == "BrkTil", "Brick/Tile",
                             ifelse(data_v1$Foundation == "CBlock","Cinder Block",
                                    ifelse(data_v1$Foundation == "PConc", "Poured Concrete",
                                           "Other")))
  
  data_v1$Foundation2 <- factor(data_v1$Foundation2,
                             levels = c('Brick/Tile',
                                        'Cinder Block',
                                        'Poured Concrete',
                                        'Other'
                             )
  )
  
  data_v1$Foundation <- NULL
  
  ## OVERALLCOND - doesn't have a high significance but there are a lot of variables.
  ## What if we split into 3 groups: Above Average, Average, and Below Average?
  
  Above_Average_Vector = c("Excellent","Very Good","Good","Above Average")
  Below_Average_Vector = c("Below Average", "Fair","Poor","Very Poor")
  
  data_v1$OverallCond2 = ifelse(data_v1$OverallCond %in% Above_Average_Vector,"Above Average",
                             ifelse(data_v1$OverallCond %in% Below_Average_Vector, "Below Average",
                                    "Average"))
  
  
  data_v1$OverallCond2 = factor(data_v1$OverallCond2, ordered = TRUE,
                             levels = c('Below Average',
                                        'Average',
                                        'Above Average'
                             )
  )
  
  rm("Above_Average_Vector")
  rm("Below_Average_Vector")  
  
  VExcellent_to_VGood = c("Very Excellent","Excellent","Very Good") 
  BAverage_to_VPoor = c("Below Average", "Fair","Poor","Very Poor")
  
  data_v1$OverallQual2 = ifelse(data_v1$OverallQual %in% VExcellent_to_VGood,"Above Average",
                             ifelse(data_v1$OverallCond %in% BAverage_to_VPoor, "Below Average",
                                    "Average"))
  
  
  data_v1$OverallQual2 = factor(data_v1$OverallQual2, ordered = TRUE,
                             levels = c('Below Average',
                                        'Average',
                                        'Above Average'
                             )
  )
  
  rm("VExcellent_to_VGood")
  rm("BAverage_to_VPoor") 
  
  data_v1$OverallCond <- NULL
  data_v1$OverallQual <- NULL
  
  ## EXTERIOR QUALITY - LETS COMBINE SOME THINGS.
  ## ONLY 1 INSTANCE OF POOR, 3 OF EXCELL, 28 OF FAIR.
  ##summary(data$ExterQual)
  Excellent_to_Good = c("Excellent","Good") 
  Fair_to_Poor = c("Fair","Poor")
  
  data_v1$ExterQual2 = ifelse(data_v1$ExterQual %in% Excellent_to_Good,"Above Average",
                           ifelse(data$ExterQual %in% Fair_to_Poor, "Below Average",
                                  "Average"))
  data_v1$ExterQual2 <- factor(data_v1$ExterQual2, ordered=TRUE,
                            levels = c('Below Average',
                                       'Average',
                                       'Above Average'
                            )
  )
  
  rm("Excellent_to_Good")
  rm("Fair_to_Poor") 
  
  data_v1$ExterQual <- NULL
  
  return(data_v1)
}


splitData <- function(type, ratio, data){
  library(caTools)
  set.seed(100)
  
  
  split <-sample.split(data$Target, SplitRatio=ratio)
  
  if (type == "train") {
    returnData = subset(data, split == TRUE)
    
  } else {
    returnData = subset(data, split==FALSE)
    
  }
  rm("split")
  return (returnData)
  
}

##PCA <- function(data) {
##  library(caret)
##  library(e1071)
  
##  na.action(na.omit(data))
##  pca2 = prcomp(data)
##  summary(pca)
  
##}