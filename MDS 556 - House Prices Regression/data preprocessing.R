##install.packages("knitr")
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
##install.packages("corrplot")
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

##rm(list = ls())
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
data_cleaner <- function(data_raw){

  ## 1 - FEATURE: ID = REMOVE
  drop <- c("Id")  
  
  ## 2 - FEATURE: MSSUBCLASS
  data_raw$MSSubClass = factor(data_raw$MSSubClass,
                           levels = c('20','30','40','45','50','60','70','75','80','85','90','120','150','160','180','190'),
                           labels = c('1-STORY 1946 & NEWER ALL STYLES',
                                      '1-STORY 1945 & OLDER',
                                      '1-STORY W/FINISHED ATTIC ALL AGES',
                                      '1-1/2 STORY - UNFINISHED ALL AGES',
                                      '1-1/2 STORY FINISHED ALL AGES',
                                      '2-STORY 1946 & NEWER',
                                      '2-STORY 1945 & OLDER',
                                      '2-1/2 STORY ALL AGES',
                                      'SPLIT OR MULTI-LEVEL',
                                      'SPLIT FOYER',
                                      'DUPLEX - ALL STYLES AND AGES',
                                      '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
                                      '1-1/2 STORY PUD - ALL AGES',
                                      '2-STORY PUD - 1946 & NEWER',
                                      'UD - MULTILEVEL - INCL SPLIT LEV/FOYER',
                                      '2 FAMILY CONVERSION - ALL STYLES AND AGES'))
  
  ## 3 - FEATURE: MSZONING, turn to factor
  ## For now lets assume that C (all) is Commercial
  
  data_raw$MSZoning = factor(data_raw$MSZoning,
                         levels = c(  'A'
                                      ,'C (all)'
                                      ,'FV'
                                      ,'I'
                                      ,'RH'
                                      ,'RL'
                                      ,'RP'
                                      ,'RM'
                         ),
                         labels = c( 'Agriculutre'
                                     ,'Commercial'
                                     ,'Floating Village Residential'
                                     ,'Industrial'
                                     ,'Residential High Density'
                                     ,'Residential Low Density'
                                     ,'Residential Low Density Park'
                                     ,'Residential Medium Density'))
  
  
  ## 4 - FEATURE: LotFrontage, for now replace with 0, no street connected to property
  data_raw$LotFrontage[is.na(data_raw$LotFrontage)] <- 0
  
  ## 5 - FEATURE: LotArea, for now replace missing values w/ mean if any
  ## since LotArea could not be 0. There are no instances of this in the data
  ## but in case they were, we'd catch it here.
  data_raw$LotArea = ifelse(is.na(data_raw$LotArea),
                        ave(data_raw$LotArea, FUN = function(x) mean(x,na.rm=TRUE)),
                        data_raw$LotArea)
  
  ## 6 - FEATURE: STREET, turn to factor
  data_raw$Street = factor(data_raw$Street,
                       levels = c(  'Grvl'
                                    ,'Pave'
                       ),
                       labels = c("Gravel","Paved"))
  
  ## 7 - FEATURE: Alley - Lets change this just to indicate if there is an alley or not
  data_raw$Alley = +(!is.na(data_raw$Alley))
  
  ## 8 - FEATURE: LotShape, turn to factor
  data_raw$LotShape = factor(data_raw$LotShape,
                         levels = c(    'Reg'
                                        ,'IR1'
                                        ,'IR2'
                                        ,'IR3'
                         ),
                         labels = c( "Regular"
                                     ,"Slightly Irregular"
                                     ,"Moderately Irregular"
                                     ,"Irregular"))

  ## 9 - FEATURE: LandContour, turn to factor
  data_raw$LandContour = factor(data_raw$LandContour,
                                levels = c(    'Lvl'
                                               ,'Bnk'
                                               ,'HLS'
                                               ,'Low'
                                ),
                                labels = c( "Near Flat/Level"
                                           ,"Banked"
                                           ,"Hillside"
                                           ,"Low"))
  sapply(data_raw,function(x) sum(is.na(x)))
  ## 10 - FEATURE: Utilities, turn to factor
  data_raw$Utilities = factor(data_raw$Utilities,
                              levels = c('AllPub',
                                         'NoSewr',
                                         'NoSeWa',
                                         'ELO'
                              ),
                              labels = c( "All Public Utilities"
                                         ,"Electricity, Gas, and Water"
                                         ,"Electricity and Gas Only"
                                         ,"Electricity Only"))
  
  ## 11 - FEATURE: LotConfig, turn to factor
  data_raw$LotConfig = factor(data_raw$LotConfig,
                              levels = c('Inside',
                                         'Corner',
                                         'CulDSac',
                                         'FR2',
                                         'FR3'
                              ),
                              labels = c( "Inside Lot"
                                         ,"Corner Log"
                                         ,"Cul-de-sac"
                                         ,"Frontage on 2 sides of property"
                                         ,"Frontage on 3 sides of property"))
  
  ## 12 - FEATURE: LandSlope, turn to factor
  data_raw$LandSlope = factor(data_raw$LandSlope,
                              levels = c('Gtl',
                                         'Mod',
                                         'Sev'
                              ),
                              labels = c( "Gentle Slope"
                                         ,"Moderate Slope"
                                         ,"Sever Slope")
                              )

  ## 13 - FEATURE: Neighborhood, turn to factor
  data_raw$Neighborhood = factor(data_raw$Neighborhood,
                                 levels = c('Blmngtn',
                                            'Blueste',
                                            'BrDale',
                                            'BrkSide',
                                            'ClearCr',
                                            'CollgCr',
                                            'Crawfor',
                                            'Edwards',
                                            'Gilbert',
                                            'IDOTRR',
                                            'MeadowV',
                                            'Mitchel',
                                            'Names',
                                            'NoRidge',
                                            'NPkVill',
                                            'NridgHt',
                                            'NWAmes',
                                            'OldTown',
                                            'SWISU',
                                            'Sawyer',
                                            'SawyerW',
                                            'Somerst',
                                            'StoneBr',
                                            'Timber',
                                            'Veenker',
                                            'Other'
                                 ),
                                 labels = c( "Bloomington Heights"
                                           ,"Bluestem"
                                           ,"Briardale"
                                           ,"Brookside"
                                           ,"Clear Creek"
                                           ,"College Creek"
                                           ,"Crawford"
                                           ,"Edwards"
                                           ,"Gilbert"
                                           ,"Iowa DOT and Rail Road"
                                           ,"Meadow Village"
                                           ,"Mitchell"
                                           ,"North Ames"
                                           ,"Northridge"
                                           ,"Northpark Villa"
                                           ,"Northridge Heights"
                                           ,"Northwest Ames"
                                           ,"Old Town"
                                           ,"South & West of Iowa State University"
                                           ,"Sawyer"
                                           ,"Sawyer West"
                                           ,"Somerset"
                                           ,"Stone Brook"
                                           ,"Timberland"
                                           ,"Veenker"
                                           ,"Other")
                             )
  data_raw$Neighborhood[is.na(data_raw$Neighborhood)] <- "Other"

  #14 & #15 Condition1 and Condition2
  ## Artial Street = Highway
  ## Feeder Street = Important Road that feeds into high traffic area
  ## Combine Condition1 and Condition2
  ## binary = ByHWY
  ##          ByFeeder
  ##          ByRR
  ##          ByPositiveFeature
  ## Normal I Think would be a duplicate dummy variable.
  RR_List = c("RRNn","RRAn","RRNe", "RRAe")
  data_raw$ByRR <- +(data_raw$Condition1 %in% RR_List | data_raw$Condition2 %in% RR_List)
  rm("RR_List")
  data_raw$ByHWY = +(data_raw$Condition1 == "Artery" | data_raw$Condition2 == "Artery")
  
  data_raw$ByFeeder = +(data_raw$Condition1 == "Feedr" | data_raw$Condition2 == "Feedr")
  
  data_raw$ByPositiveFeature = +(data_raw$Condition1 == "PosN" | data_raw$Condition2 == "PosN")  
  
  drop <- c(drop,"Condition1","Condition2")  
  ## Remove Condition1 and Condition2

  ## FEATURE 16 BldgType = Type of Dwelling
  ## Maybe combine townhouse factor
  data_raw$BldgType = factor(data_raw$BldgType,
                          levels = c('1Fam',
                                     '2FmCon',
                                     'Duplx',
                                     'Twnhse',
                                     'TwnhsI'
                          ),
                          labels = c(  "Single-Family Detached"
                                      ,"Two-Family Conversion"
                                      ,"Duplex"
                                      ,"Townhouse End Unit"
                                      ,"Townhouse Inside Unit")
                       )
  
  ggplot(data[!is.na(data$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  
  sapply(data_raw,function(x) sum(is.na(x)))
  ## FEATURE 17 HouseStyle = Style of dwelling
  ## Thinking about doing story as numeric
  ## Finished as boolean
  ## Split Foyer as boolean
  ## Split Level as boolen 
  ## If Split Foyer/Level - what is the # of stories?
  ## Factor for now
  ## What to do with unknown values?
  data_raw$HouseStyle = factor(data_raw$HouseStyle,
                         levels = c('1Story',
                                    '1.5Unf',
                                    '2Story',
                                    '2.5Fin',
                                    '2.5Unf',
                                    'SFoyer',
                                    'SLvl'
                         ),
                         labels = c(   "1 Story"
                                      ,"1.5 Story Unfinished"
                                      ,"2 Story"
                                      ,"2.5 Finished"
                                      ,"2.5 Unfinished"
                                      ,"Split Foyer"
                                      ,"Split Level")
                  )
  
  ## FEATURE 18 OverallQuality = Rates the overall material and finish of house
  ## Order matters here - Ordinal data... add ordered = TRUE param
  data_raw$OverallQual = factor(data_raw$OverallQual, ordered = TRUE,
                           levels = c('10',
                                      '9',
                                      '8',
                                      '7',
                                      '6',
                                      '5',
                                      '4',
                                      '3',
                                      '2',
                                      '1'
                           ),
                           labels = c(   "Very Excellent"
                                         ,"Excellent"
                                         ,"Very Good"
                                         ,"Good"
                                         ,"Above Average"
                                         ,"Average"
                                         ,"Below Average"
                                         ,"Fair"
                                         ,"Poor"
                                         ,"Very Poor")
  )
  ## FEATURE 19 OverallCond = Rates the overall condition of the house
  ## Order matters here - Ordinal data... add ordered = true param
  data_raw$OverallCond = factor(data_raw$OverallCond, ordered = TRUE,
                            levels = c('10',
                                       '9',
                                       '8',
                                       '7',
                                       '6',
                                       '5',
                                       '4',
                                       '3',
                                       '2',
                                       '1'
                            ),
                            labels = c(   "Very Excellent",
                                          "Excellent",
                                          "Very Good"
                                          ,"Good"
                                          ,"Above Average"
                                          ,"Average"
                                          ,"Below Average"
                                          ,"Fair"
                                          ,"Poor"
                                          ,"Very Poor")
  )
  
  ## FEATURE 20 YearBuilt: Original construction date
  
  ## FEATURE 21 YearRemodAdd: Remodel data (same as construction data if 
  ## if no remodeling or additons)
  ## Duplicate info... maybe turn into boolean
  
  data_raw$Remodeled <- +(data_raw$YearRemodAdd != data_raw$YearBuilt)
  
  ## FEATURE 22: ROOFSTYLE:
  data_raw$RoofStyle = factor(data_raw$RoofStyle,
                         levels = c('Flat',
                                    'Gable',
                                    'Gambrel',
                                    'Hip',
                                    'Mansard',
                                    'Shed'
                         ),
                         labels = c(  "Float"
                                      ,"Gable"
                                      ,"Gable-Barn"
                                      ,"Hip"
                                      ,"Mansard"
                                      ,"Shed")
  )
  
  ## FEATURE 22: Foot Material:
  data_raw$RoofMatl = factor(data_raw$RoofMatl,
                          levels = c('ClyTile',
                                     'CompShg',
                                     'Membran',
                                     'Metal',
                                     'Roll',
                                     'Tar&Grv',
                                     'WdShake',
                                     'WdShngl'
                          ),
                          labels = c(  "Clay or Tile"
                                       ,"Standard (Composite) Shingle"
                                       ,"Membrane"
                                       ,"Metal"
                                       ,"Roll"
                                       ,"Gravel & Tar"
                                       ,"Wood Shakes"
                                       ,"Wood Shingles")
  )

  ## FEATURE 23 & 24 Exterior1st/Exterior2nd - Exterior Covering on House
  ## Start by splitting into separate columns I suppose.
  ## Take the most popular 5 Exterior Material, Put the rest into "Other" Category
  ## VinylSd, HdBoard, MetalSd, Wd Sdng, Plywood
  barplot(tail(sort(table(data$Exterior1st)),5))
  ##summary(data$Exterior1st)
  ##barplot(tail(sort(table(data$Exterior2nd)),5))
  ##summary(data$Exterior2nd)
  
  data$Exterior = ifelse(
                    data$Exterior1st == "VinylSd" | 
                    data$Exterior2nd == "VinylSd",
                        "Vinyl",
                  ifelse(
                    data$Exterior1st == "HdBoard" |
                    data$Exterior2nd == "HdBoard",
                        "Hard Board",
                  ifelse(
                    data$Exterior1st == "MetalSd" |
                    data$Exterior2nd == "MetalSd",
                        "Metal",
                  ifelse(
                    data$Exterior1st == "Wd Sdng" |
                    data$Exterior2nd == "Wd Sdng",
                      "Wood Siding",
                  ifelse(
                    data$Exterior1st == "Plywood" |
                    data$Exterior2nd == "Plywood",
                      "Plywood","Other"
                  )))))
  
  data$Exterior <- factor(data$Exterior,
                          levels = c('Vinyl',
                                     'Hard Board',
                                     'Metal',
                                     'Wood Siding',
                                     'Plywood',
                                     'Other'
                            )
                        )
  
  drop <- c(drop, "Exterior1st","Exterior2nd")
  
  ## FEATURE 25 MsnVnrType: Mason Veneer Type
  ## What is this? On the exterior of the home to add curb
  ## appeal - (example, half brink on the exterior of a home)
  ## I would assume if a house has one or not plays more into the 
  ## values of the home versus what it's made of/sq ft.

  data$HasMasonVeneer = +(data$MasVnrType != "None")
  data$HasMasonVeneer <- +(!is.na(data$HasMasonVeneer))
  
  drop <- c(drop, "MasVnrType","MasVnrArea")
  
  ## FEATURE 26: Exterior Quality  
  data$ExterQual <- factor(data$ExterQual, ordered=TRUE,
                          levels = c("Fa",
                                     "TA",
                                     "Gd",
                                     "Ex"
                                     ##'Po', No Poor
                            ),
                          labels = c("Fair",
                                     "Average",
                                     "Good",
                                     "Excellent")
  )
  ## FEATURE 26: Exterior Quality  
  data$ExterCond <- factor(data$ExterCond, ordered=TRUE,
                           levels = c("Po",
                                      "Fa",
                                      "TA",
                                      "Gd",
                                      'Ex'
                           ),
                           labels = c("Poor",
                                      "Fair",
                                      "Average",
                                      "Good",
                                      "Excellent"
                                    )
                    )
  drop <- c(drop,"ExterCond","RoofMatl") ## TEST
  ## FEATURE 27: Foundation: Type of Foundation - OK, will
  ## revisit the factors/labels in feature engineering
  summary(data$Foundation)
  
  
  ## BASEMENT - FOR NOW, LETS JUST CAPTURE IF THE HOUSE HAS
  ## A BASEMENT.
  data$HasBasement <- +(data$BsmtQual != "NA")
  data$HasBasement <- +(!is.na(data$HasBasement))  
  ## Feature 28: BsmtQual - Basement Quality- REMOVE

  ## Feature 29: BsmtCond - Evaluates the general condition of the
  ## basement - Remove
  
  ## Feature 30: BsmtExposure: Refers to walkout or garden level walls
  ## Remove
  
  ## Feature 31: BsmtFinType1: Rating of basement Finished Area - Remove
  
  ## Feature 32: BsmtFinSF1: Type 1 finished square feet - Remove
  
  ## Feature 33: BsmtFinType2: Rating of basement finished area 
  ##  if multiple types - Remove
  
  ## Feature 34: BsmtFinSF2: Type 2 Finished square feet - Remove
  
  ## Feature 35: BsmtUnfSF: Unfinished Square Feet of Basement Area - Remove

  ## Feature 36: TotalBsmtSF - Total sq ft of basement: This might be a better
  ## choice than a binary, but removing for now.
  
   drop <- c(drop, "BsmtQual","BsmtCond", "BsmtExposure"
            ,"BsmtFinType1","BsmtFinSF1","BsmtFinType2"
            ,"BsmtFinSF2", "BsmtUnfSF","TotalBsmtSF")

  ## FEATURE 37: Heating
  ## Feature 38: HeatinQC
  ## Feature 39: CentralAir
  ## Feature 40: Electrical
  ## Feature 41: 1stFlrSF - not a factor
  ## Feature 42: 2ndFlrSF - not a factor
  ## Feature 43: LowQualFinSF ?? What, removed
  ## Feature 44: GrLivArea -remove
  ## Feature 45: BsmtFullBath - removed
  ## Feature 46: BsmtHalfBath - removed 
  ## Feature 47: Full Bath - above grade
  ## Feature 48: Half Bath - above grade
  ## Feature 49: Bedroom - above grade 
  ## Feature 50: Kitchens above grade
  ## Feature 51: KitchenQual 
  ## Feature 52: TotRmsAbvGrd
  ## Feature 53: Functional
  ## Feature 54: Fireplaces - # of fireplaces
  ## Feature 55: FirplaceQu - Fireplace quality
   drop <- c(drop, "Heating","HeatinQC", "CentralAir"
             ,"Electrical","LowQualFinSF","GrLivArea"
             ,"BsmtFullBath", "BsmtHalfBath","KitchenQual"
             ,"Functional","FireplaceQu","BldgType","HouseStyle")   
  ## GARAGE: Lets indicate if there is a garage and 
  ## figure out how to incorporate other features later 
  ##data$HasGarage <- +(data$GarageQual != "NA")
  ## Feature 56: GarageType - Type of Garage 
  ## Feature 56: GarageYrBlt - Year garage was built 
  ## Feature 57: GarageFinish: Interior finish of garage 
  ## Feature 58: GarageCars: Size of garage in car captacity
  ## Feautre 59: GarageArea
  ## Feature 60: GarageQual
  ## Feature 61: GarageCond
    data$HasGarage <- ifelse(data$GarageQual != "NA", TRUE,FALSE)
    data$HasGarage <- +(!is.na(data$HasGarage))

  summary(data$HasGarage)
  drop <- c(drop,"GarageType","GarageYrBlt","GarageFinish"
             ,"GarageCars","GarageArea", "GarageQual","GarageCond")
  
  ## Feature 62: PavedDrive
  ## Feature 63: WoodDeckSF
  ## Feature 64: OpenPorchSF   
  ## Feature 65: EnclosedPorch
  ## Feature 66: 3SsnPorch
  ## Feature 67: ScreenPorch

  drop <- c(drop,"PavedDrive","WoodDeckSF","OpenPorchSF",   
            "EnclosedPorch","3SsnPorch","ScreenPorch")
     
  ## POOL: Lets indicate if there is a garage and 
  ## figure out how to incorporate other features later 
  
  ## data$HasPool <- +(data$PoolQC != "NA")
   
  ## Feature 68: PoolArea
  ## Feature 69: PoolQC
  
  drop <- c(drop,"PoolArea","PoolQC")
  
  ## Feature 70: Fence
  ## Feature 71: MiscFeature  
  ## Feature 72: MiscVal
  ## Feature 73: MoSold
  ## Feature 74: YrSold
  ## Feature 75: SaleType
  ## Feature 76: SaleCondition 
  
  drop <-c(drop,"Fence","MiscFeature","MiscVal",
           "SaleType","SaleCondition")
  
  data <- data[ , !(names(data) %in% drop)]
  
  return (data)
}
