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

data_cleaner <- function(data_raw){

  ## 1 - FEATURE: ID =  REMOVE
  data_raw$Id <- NULL
  
  ## 2 - FEATURE: MSSUBCLASS
  data_raw$MSSubClass = factor(data$MSSubClass,
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
  
  
  ggplot(data[!is.na(data_raw$SalePrice),], aes(x=as.factor(MSSubClass), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  ## 3 - FEATURE: MSZONING, turn to factor
  ## For now lets assume that C (all) is Commercial
  data_raw$MSZoning = factor(data$MSZoning,
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
  data_raw$Street = factor(data$Street,
                       levels = c(  'Grvl'
                                    ,'Pave'
                       ),
                       labels = c("Gravel","Paved"))
  
  ## 7 - FEATURE: Alley - Lets change this just to indicate if there is an alley or not
  data_raw$Alley = +(!is.na(data$Alley))
  
  ## 8 - FEATURE: LotShape, turn to factor
  data_raw$LotShape = factor(data$LotShape,
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
  data_raw$LandContour = factor(data$LandContour,
                                levels = c(    'Lvl'
                                               ,'Bnk'
                                               ,'HLS'
                                               ,'Low'
                                ),
                                labels = c( "Near Flat/Level"
                                           ,"Banked"
                                           ,"Hillside"
                                           ,"Low"))

  ## 10 - FEATURE: Utilities, turn to factor
  data_raw$Utilities = factor(data$Utilities,
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
  data_raw$LandSlope = factor(data$LandSlope,
                              levels = c('Gtl',
                                         'Mod',
                                         'Sev'
                              ),
                              labels = c( "Gentle Slope"
                                         ,"Moderate Slope"
                                         ,"Sever Slope")
                              )

  ## 13 - FEATURE: Neighborhood, turn to factor
  data_raw$Neighborhood = factor(data$Neighborhood,
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
  
  ggplot(data[!is.na(data_raw$SalePrice),], aes(x=as.factor(Neighborhood), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
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
  
  data_raw$Condition1 <- NULL
  data_raw$Condition2 <- NULL

  ## Remove Condition1 and Condition2

  ## FEATURE 16 BldgType = Type of Dwelling
  ## Combine townhouse factor - The Data has 43 Twnhs (no I or E). 
  data_raw$BldgType <- NULL
  data_raw$BldgType = ifelse(data$BldgType == "1Fam","Single-Family Detached",
                             ifelse(data$BldgType == "2fmCon","Two-Family Conversion",
                                    ifelse(data$BldgType == "Duplex","Duplex",
                                           ifelse(data$BldgType == "TwnhsE" | data$BldgType == "Twnhs","Townhouse","NA"))))
  data_raw$BldgType = factor(data_raw$BldgType)
  
  ## See if oridinal in nature - it's not
  ggplot(data[!is.na(data_raw$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  
  summary(data_raw$BldgType)
  ## Duplex Single-Family Detached              Townhouse  Two-Family Conversion 
  ## 52                   1220                    157                     31 
  
  ## FEATURE 17 HouseStyle = Style of dwelling
  ## Thinking about doing story as numeric
  ## Finished as boolean
  ## Split Foyer as boolean
  ## Split Level as boolen 
  ## If Split Foyer/Level - what is the # of stories?
  ## Factor for now
  ## What to do with unknown values?
  data_raw$HouseStyle = factor(data$HouseStyle,
                         levels = c('1Story',
                                    '1.5Fin',
                                    '1.5Unf',
                                    '2Story',
                                    '2.5Fin',
                                    '2.5Unf',
                                    'SFoyer',
                                    'SLvl'
                         ),
                         labels = c(   "1 Story"
                                      ,"1.5 Story Finished"
                                      ,"1.5 Story Unfinished"
                                      ,"2 Story"
                                      ,"2.5 Finished"
                                      ,"2.5 Unfinished"
                                      ,"Split Foyer"
                                      ,"Split Level")
                  )
  

  ## FEATURE 18 OverallQuality = Rates the overall material and finish of house
  ## Order matters here - Ordinal data... add ordered = TRUE param
  data_raw$OverallQual = factor(data$OverallQual, ordered = TRUE,
                                levels = c('1',
                                           '2',
                                           '3',
                                           '4',
                                           '5',
                                           '6',
                                           '7',
                                           '8',
                                           '9',
                                           '10'
                                ),
                                labels = c( "Very Poor"
                                            ,"Poor"
                                            ,"Fair"
                                            ,"Below Average"
                                            ,"Average"
                                            ,"Above Average"
                                            ,"Good"
                                            ,"Very Good"
                                            ,"Excellent"
                                            ,"Very Excellent")
  )
  
  ##Example of ordinal data plotted against the target
  ggplot(data_raw[!is.na(data_raw$SalePrice),], aes(x=as.factor(OverallQual), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  ## FEATURE 19 OverallCond = Rates the overall condition of the house
  ## Order matters here - Ordinal data... add ordered = true param
  data_raw$OverallCond = factor(data$OverallCond, ordered = TRUE,
                            levels = c('1',
                                       '2',
                                       '3',
                                       '4',
                                       '5',
                                       '6',
                                       '7',
                                       '8',
                                       '9',
                                       '10'
                            ),
                            labels = c( "Very Poor"
                                       ,"Poor"
                                       ,"Fair"
                                       ,"Below Average"
                                       ,"Average"
                                       ,"Above Average"
                                       ,"Good"
                                       ,"Very Good"
                                       ,"Excellent"
                                       ,"Very Excellent")
  )
  
  ##Example of ordinal data plotted against the target
  ggplot(data_raw[!is.na(data_raw$SalePrice),], aes(x=as.factor(OverallCond), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  ## FEATURE 20 YearBuilt: Original construction date
  
  ## FEATURE 21 YearRemodAdd: Remodel data (same as construction data if 
  ## if no remodeling or additons)
  ## Duplicate info... maybe turn into boolean
  data_raw$Remodeled <- +(data$YearRemodAdd != data$YearBuilt)
  data_raw$YearRemodAdd <- NULL  
  
  ## FEATURE 22: ROOFSTYLE:
  data_raw$RoofStyle = factor(data$RoofStyle,
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
  
  summary(data_raw$RoofStyle)
  
  ## FEATURE 22: Foot Material:
  data_raw$RoofMatl = factor(data$RoofMatl,
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
  
  ##barplot(tail(sort(table(data_raw$Exterior1st)),5))
  
  ##summary(data$Exterior1st)
  ##barplot(tail(sort(table(data$Exterior2nd)),5))
  ##summary(data$Exterior2nd)
  
  data_raw$Exterior = ifelse(
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
  
  data_raw$Exterior <- factor(data_raw$Exterior,
                          levels = c('Vinyl',
                                     'Hard Board',
                                     'Metal',
                                     'Wood Siding',
                                     'Plywood',
                                     'Other'
                            )
                        )
  
  #summary(data_raw$Exterior)
  data_raw$Exterior1st <- NULL
  data_raw$Exterior2nd <- NULL

  
  ## FEATURE 25 MsnVnrType: Mason Veneer Type
  ## What is this? On the exterior of the home to add curb
  ## appeal - (example, half brink on the exterior of a home)
  ## I would assume if a house has one or not plays more into the 
  ## values of the home versus what it's made of/sq ft.

  data_raw$HasMasonVeneer = +(data$MasVnrType != "None" | is.na(data$MasVnrArea))

  data_raw$MasVnrType <- NULL
  data_raw$MasVnrArea <- NULL
  
  summary(data_raw$MasVnrType)

  ## FEATURE 26: Exterior Quality  
  data_raw$ExterQual <- factor(data$ExterQual, ordered=TRUE,
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
  ## FEATURE 26: Exterior Quality  
  data_raw$ExterCond <- factor(data$ExterCond, ordered=TRUE,
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
  
  ## FEATURE 27: Foundation: Type of Foundation - OK, will
  ## revisit the factors/labels in feature engineering
    data_raw$Foundation <- factor(data$Foundation,
                               levels = c("BrkTil",
                                          "CBlock",
                                          "PConc",
                                          "Slab",
                                          'Stone',
                                          "Wood"
                               ),
                               labels = c("Brick & Tile",
                                          "Cinder Block",
                                          "Poured Concrete",
                                          "Slab",
                                          'Stone',
                                          "Wood"
                               )
  )


  ############################################################
  
  ## BASEMENT FEATURES - 11
  ## A BASEMENT.
  ##data$HasBasement <- +(data$BsmtQual != "NA")
  ##data$HasBasement <- +(!is.na(data$HasBasement))
  
  basement_variables <- c("BsmtHeight","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath")

  summary(data_raw$BsmtHeight)  
  ## 37 NAs
  
  ## Feature 28: BsmtQual -renamed to BsmtHeight - This is weird b/c its the height of basement.. not sure if it should be ordinal
  data_raw$BsmtQual <- NULL
  data_raw$BsmtHeight <- factor(data$BsmtQual, order=TRUE,
                                levels = c("NA",
                                           "Po",
                                           "Fa",
                                           "TA",
                                           'Gd',
                                           "Ex"
                                ),
                                labels = c("No Basement",
                                           "< 70in",
                                           "70-79in",
                                           "80-89in",
                                           '90-99in',
                                           "100+in"
                                )
  )
  
  invalid_basements <- which(is.na(data_raw$BsmtHeight))
  basement_list <- data_raw[invalid_basements,basement_variables]
  
  data_raw$BsmtHeight[invalid_basements] <- "No Basement"
  
  
  ## Feature 29: BsmtCond Evaluates the general condition of the
  ## basement
  
  data_raw$BsmtCond <- factor(data$BsmtCond, order=TRUE,
                                levels = c("NA",
                                           "Po",
                                           "Fa",
                                           "TA",
                                           'Gd',
                                           "Ex"
                                ),
                                labels = c("No Basement",
                                           "Poor",
                                           "Fair",
                                           "Average",
                                           'Good',
                                           "Excellent"
                                )
  )
  
  invalid_basements <- which(is.na(data$BsmtCond))
  basement_list <- data_raw[invalid_basements,basement_variables]
  
  data_raw$BsmtCond[invalid_basements] <- "No Basement"

  
  ## Feature 30: BsmtExposure: Refers to walkout or garden level walls
  
  data_raw$BsmtExposure <- factor(data$BsmtExposure, order=TRUE,
                              levels = c("NA",
                                         "No",
                                         "Mn",
                                         "Av",
                                         'Gd'
                              ),
                              labels = c("No Basement",
                                         "No Exposure",
                                         "Minimum Exposure",
                                         "Average Exposure",
                                         'Good Exposure'
                              )
  )
  
  data_raw$BsmtExposure[949] <- names(sort(-table(data_raw$BsmtExposure)))[1] ## IMPUTE MODE
  invalid_basements <- which(is.na(data_raw$BsmtExposure))
  basement_list <- data_raw[invalid_basements,basement_variables]
  data_raw$BsmtExposure[invalid_basements] <- "No Basement"
  
  ## Feature 31: BsmtFinType1: Rating of basement Finished Area
  data_raw$BsmtFinType1 <- factor(data$BsmtFinType1, order=TRUE,
                                  levels = c("NA",
                                             "Unf",
                                             "LwQ",
                                             "Rec",
                                             'BLQ',
                                             "ALQ",
                                             'GLQ'
                                  ),
                                  labels = c("No Basement",
                                             "Unfinished",
                                             "Low Quality",
                                             "Average Rec Room",
                                             'Below Average Living Quarters',
                                             "Average Living Quarters",
                                             "Good Living Quarters"
                                  )
              )
  
  summary(data_raw$BsmtFinType1)
  invalid_basements <- which(is.na(data_raw$BsmtFinType1))
  basement_list <- data_raw[invalid_basements,basement_variables]
  data_raw$BsmtFinType1[invalid_basements] <- "No Basement"
  
  ## Feature 32: BsmtFinSF1: Type 1 finished square feet
  data_raw$BsmtFinSF1[is.na(data$BsmtFinSF1)] <-0
  
  which(data_raw$BsmtFinSF1 > 0 & data_raw$BsmtCond == "No Basement")
  ## 0 - Good
  
  ## Feature 33: BsmtFinType2: Rating of basement finished area 
  ##  if multiple types
  data_raw$BsmtFinType2 <- factor(data$BsmtFinType2, order=TRUE,
                                  levels = c("NA",
                                             "Unf",
                                             "LwQ",
                                             "Rec",
                                             'BLQ',
                                             "ALQ",
                                             'GLQ'
                                  ),
                                  labels = c("No Basement",
                                             "Unfinished",
                                             "Low Quality",
                                             "Average Rec Room",
                                             'Below Average Living Quarters',
                                             "Average Living Quarters",
                                             "Good Living Quarters"
                                  )
                          )
  
  invalid_basements <- which(is.na(data_raw$BsmtFinType2))
  basement_list <- data_raw[invalid_basements,basement_variables]
  data_raw$BsmtFinType2[invalid_basements] <- "No Basement"  
  
  ## Feature 34: BsmtFinSF2: Type 2 Finished square feet
  data_raw$BsmtFinSF2[is.na(data$BsmtFinSF2)] <-0
  
  ## Feature 35: BsmtUnfSF: Unfinished Square Feet of Basement Area - Remove
  data_raw$BsmtUnfSF[is.na(data$BsmtUnfSF)] <-0
  
  ## Feature 36: TotalBsmtSF - Total sq ft of basement
  data_raw$TotalBsmtSF[is.na(data$TotalBsmtSF)] <-0
  
  ## Feature 45: BsmtFullBath
  data_raw$BsmtFullBath[is.na(data$BsmtFullBath)] <-0
  summary(data$BsmtFullBath)
  
  ## Feature 46: BsmtHalfBath
  data_raw$BsmtHalfBath[is.na(data$BsmtHalfBath)] <-0

  ########################################################
  
  ## FEATURE 37: Heating
  data_raw$Heating <- factor(data$Heating, 
                                  levels = c("Floor",
                                             "GasA",
                                             "GasW",
                                             "Grav",
                                             'OthW',
                                             "Wall"
                                  ),
                                  labels = c("Floor Furnace",
                                             "Gas w/ warm air furnace",
                                             "Gas w/ hot water/steam",
                                             "Gravity Furnace",
                                             "Not Gas",
                                             "Wall Furnace"
                                  )
  )
  
  ## Feature 38: HeatingQC
  data_raw$HeatingQC <- factor(data$HeatingQC, order=TRUE,
                              levels = c(
                                         "Po",
                                         "Fa",
                                         "TA",
                                         'Gd',
                                         "Ex"
                              ),
                              labels = c(
                                         "Poor",
                                         "Fair",
                                         "Average",
                                         'Good',
                                         "Excellent"
                              )
  )
  
  ## Feature 39: CentralAir
  data_raw$CentralAir <- +(data$CentralAir == "Y")

  ## Feature 40: Electrical - mix of ordinal maybe? Few instances other than SBrkr 
  ## Removing feature.
  data_raw$Electrical <- factor(data$Electrical,
                               levels = c(
                                 "SBrkr",
                                 "FuseA",
                                 "FuseF",
                                 'FuseP',
                                 "Mix"
                               ),
                               labels = c(
                                 "Standard Circuit",
                                 "Fuse Box - Average",
                                 "Fuse Box - Fair",
                                 'Good',
                                 "Excellent"
                               )
  )
  
  ggplot(data_raw[!is.na(data_raw$SalePrice),], aes(x=as.factor(Electrical), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  
  data_raw$Electrical <- NULL
  
  ## Feature 41: 1stFlrSF
  data_raw$X1stFlrSF[is.na(data$X1stFlrSF)] <-0
  
  ## Feature 42: 2ndFlrSF
  data_raw$X2ndFlrSF[is.na(data$X2ndFlrSF)] <-0
  
  ## Feature 43: LowQualFinSF 
  data_raw$LowQualFinSF[is.na(data$LowQualFinSF)] <-0
  
  ## Feature 44: GrLivArea
  data_raw$GrLivArea[is.na(data$GrLivArea)] <-0

  ## Feature 47: Full Bath - above grade
  data_raw$FullBath[is.na(data$FullBath)] <-0

  ## Feature 48: Half Bath - above grade
  data_raw$HalfBath[is.na(data$HalfBath)] <- 0
  
  ## Feature 49: Bedroom - above grade 
  data_raw$BedroomAbvGr[is.na(data$BedroomAbvGr)] <- 0
  
  ## Feature 50: Kitchens above grade
  data_raw$KitchenAbvGr[is.na(data$KitchenAbvGr)] <- 0
  
  ## Feature 51: KitchenQual 
  data_raw$KitchenQual <- factor(data$KitchenQual, order=TRUE,
                                levels = c(
                                           "Po",
                                           "Fa",
                                           "TA",
                                           'Gd',
                                           "Ex"
                                ),
                                labels = c(
                                           "Poor",
                                           "Fair",
                                           "Average",
                                           'Good',
                                           "Excellent"
                                )
  )
  
  ## Feature 52: TotRmsAbvGrd
  data_raw$TotRmsAbvGrd[is.na(data$TotRmsAbvGrd)] <- 0
  
  ## Feature 53: Functional
  data_raw$Functional <- factor(data$Functional, order=TRUE,
                                 levels = c(
                                   "Sal",
                                   "Sev",
                                   "Maj2",
                                   "Maj1",
                                   "Mod",
                                   "Min2",
                                   "Min1",
                                   "Typ"
                                 ),
                                 labels = c(
                                   "Salvage Only",
                                   "Severely Damanged",
                                   "Major Deductions 2",
                                   "Major Deductions 1",
                                   "Moderate Deductions",
                                   "Minor Deductions 2",
                                   "Minor Deductions 1",
                                   "Typical Functionality"
                                 )
  )
  
  ## Feature 54: Fireplaces - # of fireplaces
  data_raw$Fireplaces[is.na(data$Fireplaces)] <- 0
  
  ## Feature 55: FirplaceQu - Fireplace quality
  data_raw$FireplaceQu <- factor(data$FireplaceQu, order=TRUE,
                                levels = c("NA",
                                           "Po",
                                           "Fa",
                                           "TA",
                                           'Gd',
                                           "Ex"
                                ),
                                labels = c("No Fireplace",
                                           "Poor",
                                           "Fair",
                                           "Average",
                                           'Good',
                                           "Excellent"
                                )
  )
  data_raw$FireplaceQu[which(is.na(data_raw$FireplaceQu))] <- "No Fireplace"
  
  ## GARAGE FEATURES
  ## Feature 56: GarageType - Type of Garage 
  data_raw$GarageType <- factor(data$GarageType, order=TRUE,
                                  levels = c("NA",
                                             "Detchd",
                                             "CarPort",
                                             "BuiltIn",
                                             "Basment",
                                             "Attchd",
                                             "2Types"
                                  ),
                                  labels = c("No Garage",
                                             "Detached",
                                             "Car Port",
                                             "Built In",
                                             "Basement Garage",
                                             "Attached",
                                             "2+ Garages"
                                  )
  )
  
  data_raw$GarageType[which(is.na(data_raw$GarageType))] <- "No Garage"
  
  ## Feature 56: GarageYrBlt - Year garage was built 
  ## replace missing values w year the house was built
  data_raw$GarageYrBlt[is.na(data$GarageYrBlt)] <- data$YearBuilt[is.na(data$GarageYrBlt)]
  
  ## Feature 57: GarageFinish: Interior finish of garage 
  data_raw$GarageFinish <- factor(data$GarageFinish, order=TRUE,
                                levels = c("NA",
                                           "RFn",
                                           "Unf",
                                           "Fin"
                                ),
                                labels = c("No Garage",
                                           "Rough Finished",
                                           "Unfinished",
                                           "Finished"
                                )
  )
  
  summary(data_raw$GarageFinish)
  data_raw$GarageFinish[which(is.na(data_raw$GarageFinish))] <- "No Garage"
  ## Feature 58: GarageCars: Size of garage in car captacity
  data_raw$GarageCars[is.na(data$GarageCars)] <- 0
  
  ## Feautre 59: GarageArea
  data_raw$GarageArea[is.na(data$GarageArea)] <- 0
  
  ## Feature 60: GarageQual
  data_raw$GarageQual <- factor(data$GarageQual, order=TRUE,
                                levels = c("NA",
                                           "Po",
                                           "Fa",
                                           "TA",
                                           'Gd',
                                           "Ex"
                                ),
                                labels = c("No Garage",
                                           "Poor",
                                           "Fair",
                                           "Average",
                                           'Good',
                                           "Excellent"
                                )
  )
  
  data_raw$GarageQual[which(is.na(data_raw$GarageQual))] <- "No Garage"
  
  ## Feature 61: GarageCond
  data_raw$GarageCond <- factor(data$GarageCond, order=TRUE,
                                  levels = c("NA",
                                             "Po",
                                             "Fa",
                                             "TA",
                                             'Gd',
                                             "Ex"
                                  ),
                                  labels = c("No Garage",
                                             "Poor",
                                             "Fair",
                                             "Average",
                                             'Good',
                                             "Excellent"
                                  )
  )
  data_raw$GarageCond[which(is.na(data_raw$GarageCond))] <- "No Garage"
  summary(data_raw$GarageCond) ## 81 NAs are houses w/o garages
  
  ###################################################################
  
  ## Feature 62: PavedDrive
  data_raw$PavedDrive <- factor(data$PavedDrive, ordered=TRUE,
                           levels = c(
                             "N",
                             "P",
                             "Y"
                           ),
                           labels = c(
                             "Dirt/Gravel",
                             "Partial Pavement",
                             "Paved"
                           )
  )
  summary(data_raw$PavedDrive)
  ## Feature 63: WoodDeckSF
  data_raw$WoodDeckSF[is.na(data$WoodDeckSF)] <- 0
  
  ## Feature 64: OpenPorchSF   
  data_raw$OpenPorchSF[is.na(data$OpenPorchSF)] <- 0
  
  ## Feature 65: EnclosedPorch
  data_raw$EnclosedPorch[is.na(data$EnclosedPorch)] <- 0
  
  ## Feature 66: 3SsnPorch
  data_raw$X3SsnPorch[is.na(data$X3SsnPorch)] <- 0
  
  ## Feature 67: ScreenPorch
  data_raw$ScreenPorch[is.na(data$ScreenPorch)] <- 0
  
  ## POOL: I think just indicating if the house has a pool should be enough
  data_raw$HasPool <- +(data$PoolArea > 0)

  ## Feature 68: PoolArea
  data_raw$PoolArea <- NULL
  
  ## Feature 69: PoolQC
  data_raw$PoolQC <- NULL
  
  ## Feature 70: Fence
  data_raw$Fence <- factor(data$Fence, ordered=TRUE,
                                   levels = c(
                                     "NA",
                                     "MnWw",
                                     "GdWo",
                                     "MnPrv",
                                     'GdPrv'
                                   ),
                                   labels = c(
                                     "No Fence",
                                     "Minimum Fence",
                                     "Good Wood Fence",
                                     'Minimum Privacy',
                                     "Good Privacy"
                                   )
  )
  data_raw$Fence[which(is.na(data_raw$Fence))] <- "No Fence"

  ## Feature 71: MiscFeature  - I don't think this is necessary
  data_raw$MiscFeature <- NULL
  
  ## Feature 72: MiscVal - I don't think this is necessary
  data_raw$MiscVal <- NULL
  
  ## Feature 73: MoSold - do nothing
  
  ## Feature 74: YrSold - do nothing

  ## Feature 75: SaleType
  data_raw$SaleType <- factor(data$SaleType,
                                   levels = c(
                                     "WD"
                                     ,"CWD"
                                     ,"VWD"
                                     ,"New"
                                     ,'COD'
                                     ,"Con"
                                     ,"ConLw"
                                     ,"ConLI"
                                     ,"ConLD"
                                     ,"Oth"
                                   ),
                                   labels = c(
                                     "Warranty Deed - Conventional"
                                     ,"Warranty Deed - Cash"
                                     ,"Warranty Deed - VA Loan"
                                     ,'Home just constructed and sold'
                                     ,"Court Officer Deed/Estate"
                                     ,"Contract 15% Down payment regular terms"
                                     ,"Contract Low Down payment and low interest"
                                     ,"Contract Low Interest"
                                     ,"Contract Low Down"
                                     ,"Other"
                                   )
  )
  
  ## Feature 76: SaleCondition 
  data_raw$SaleCondition <- factor(data$SaleCondition,
                                levels = c(
                                  "Normal",
                                  "Abnorml",
                                  "AdjLand",
                                  "Alloca",
                                  'Family',
                                  "Partial"
                                ),
                                labels = c(
                                  "Normal",
                                  "Abnormal",
                                  "AdjLand",
                                  'Allocation',
                                  "Sale btwn Family",
                                  "Home wasnt complete when assessed"
                                )
  )


  
    return (data_raw)
  
    graphit <- function(target_var, independent_var){
    ggplot(data_raw[!is.na(data_raw$target_var),], aes(x=as.factor(independent_var), y=target_var)) +
      geom_bar(stat='summary', fun.y = "median", fill='blue')+
      scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
      geom_label(stat = "count", aes(label = ..count.., y = ..count..))
      }
  
  graphit(data_raw$SalePrice,data_raw$SaleCondition)

}