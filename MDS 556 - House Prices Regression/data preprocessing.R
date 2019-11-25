rm(list = ls())
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
dataPreprocessing <- function(){
  setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')
  options(scipen = 999)
  data <- read.csv(file='train.csv', header=TRUE)
  
  ## 1 - FEATURE: ID = REMOVE

  ## 2 - FEATURE: MSSUBCLASS
  ## Decided not to turn to factor, exclude instead
  ## these characteristics seem to be applied elsewhere in House Style and Building Type
  ##  data$MSSubClass = factor(data$MSSubClass,
  ##                         levels = c('20','30','40','45','50','60','70','75','80','85','90','120','150','160','180','190'),
  ##                         labels = c('1-STORY 1946 & NEWER ALL STYLES',
  ##                                    '1-STORY 1945 & OLDER',
  ##                                    '1-STORY W/FINISHED ATTIC ALL AGES',
  ##                                    '1-1/2 STORY - UNFINISHED ALL AGES',
  ##                                    '1-1/2 STORY FINISHED ALL AGES',
  ##                                    '2-STORY 1946 & NEWER',
  ##                                    '2-STORY 1945 & OLDER',
  ##                                    '2-1/2 STORY ALL AGES',
  ##                                    'SPLIT OR MULTI-LEVEL',
  ##                                    'SPLIT FOYER',
  ##                                    'DUPLEX - ALL STYLES AND AGES',
  ##                                    '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
  ##                                    '1-1/2 STORY PUD - ALL AGES',
  ##                                    '2-STORY PUD - 1946 & NEWER',
  ##                                    'UD - MULTILEVEL - INCL SPLIT LEV/FOYER',
  ##                                    '2 FAMILY CONVERSION - ALL STYLES AND AGES'))
  
  ## 3 - FEATURE: MSZONING, turn to factor
  
  ## DATA ISSUE 1: THERE ARE 10 ROWS THAT USE C (all) 
  ## WHICH ISN'T IN THE DATA DICTIONARY.
  ## For now lets assume that C (all) is Commercial
  
  data$MSZoning = factor(data$MSZoning,
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
  
  
  
  ## 4 - FEATURE: LotFrontage, for now replace missing values w/ mean
  data$LotFrontage = ifelse(is.na(data$LotFrontage),
                            ave(data$LotFrontage, FUN = function(x) mean(x,na.rm=TRUE)),
                            data$LotFrontage)
  
  ## 5 - FEATURE: LotArea, for now replace missing values w/ mean if any
  data$LotArea = ifelse(is.na(data$LotArea),
                        ave(data$LotArea, FUN = function(x) mean(x,na.rm=TRUE)),
                        data$LotArea)
  
  ## 6 - FEATURE: STREET, turn to factor
  data$Street = factor(data$Street,
                       levels = c(  'Grvl'
                                    ,'Pave'
                       ),
                       labels = c("Gravel","Paved"))
  
  ## 7 - FEATURE: Alley - Lets change this just to indicate if there is an alley or not
  
  data$Alley = +(!is.na(data$Alley))
##  data$Alley = factor(data$Alley,
##                      levels = c(   'Grvl'
##                                    ,'Pave'
##                                    ,'Not Applicable'
##                      ),
##                      labels = c("Gravel","Paved","Not Applicable"))
  
##  data$Alley[is.na(data$Alley)] <- "Not Applicable"  
  
  ## 8 - FEATURE: LotShape, turn to factor
  data$LotShape = factor(data$LotShape,
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
  data$LandContour = factor(data$LandContour,
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
  data$Utilities = factor(data$Utilities,
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
  data$LotConfig = factor(data$LotConfig,
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
  data$LandSlope = factor(data$LandSlope,
                              levels = c('Gtl',
                                         'Mod',
                                         'Sev'
                              ),
                              labels = c( "Gentle Slope"
                                         ,"Moderate Slope"
                                         ,"Sever Slope")
                              )

  ## 13 - FEATURE: Neighborhood, turn to factor
  data$Neighborhood = factor(data$Neighborhood,
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
  data$Neighborhood[is.na(data$Neighborhood)] <- "Other"
  
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
  data$ByRR <- +(data$Condition1 %in% RR_List | data$Condition2 %in% RR_List)
  rm("RR_List")
  data$ByHWY = +(data$Condition1 == "Artery" | data$Condition2 == "Artery")
  
  data$ByFeeder = +(data$Condition1 == "Feedr" | data$Condition2 == "Feedr")
  
  data$ByPositiveFeature = +(data$Condition1 == "PosN" | data$Condition2 == "PosN")  
  
  ##data$Condition1 = factor(data$Condition1,
  ##                           levels = c('Artery',
  ##                                      'Feedr',
  ##                                      'Norm',
  ##                                      'RRNn',
  ##                                      'RRAn',
  ##                                      'PosN',
  ##                                      'PosA',
  ##                                      'RRNe',
  ##                                      'RRAe'
  ##                           ),
  ##                           labels = c(  "Adjacent to Arterial Street"
  ##                                       ,"Adjacent to Feeder Street"
  ##                                       ,"Normal"
  ##                                       ,"Within 200' of North-South RR"
  ##                                       ,"Adjacent to North-South RR"
  ##                                       ,"Near Positive Off-Site Feature"
  ##                                       ,"Adjacent to Positive Off-Site Feature"
  ##                                       ,"Within 200' of East-West RR"
  ##                                       ,"Adjacent to East-West RR"
  ##                                      )
  ##                    )                         
  ## Remove Condition1 and Condition2
  ##data = subset(data, select= -c(Condition1,Condition2))
  
  ## FEATURE 16 BldgType = Type of Dwelling
  ## Maybe combine townhouse factor
  data$BldgType = factor(data$BldgType,
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
  ## FEATURE 17 HouseStyle = Style of dwelling
  ## Thinking about doing story as numeric
  ## Finished as boolean
  ## Split Foyer as boolean
  ## Split Level as boolen 
  ## If Split Foyer/Level - what is the # of stories?
  ## Factor for now
  ## What to do with unknown values?
  data$HouseStyle = factor(data$HouseStyle,
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
  ## Order matters here - Ordinal data... how to handle?

  ## FEATURE 19 OverallCond = Rates the overall condition of the house
  ## Order matters here - Ordinal data... how to handle?
  
  ## FEATURE 20 YearBuilt: Original construction date
  
  ## FEATURE 21 YearRemodAdd: Remodel data (same as construction data if 
  ## if no remodeling or additons)
  ## Duplicate info... maybe turn into boolean
  
  data$Remodeled <- +(data$YearRemodAdd != data$YearBuilt)
  
  ##data<- data[,c(2:13,80:85)]
  data <- data[c(##"MSSubClass",
                 "MSZoning"
                ,"LotFrontage"
                ,"LotArea"
                ,"Street"
                ,"Alley"
                ,"LotShape"
                ,"LandContour"
                ,"Utilities"
                ,"LotConfig"
                ,"LandSlope"
                ,"Neighborhood"
                ,"ByPositiveFeature"
                ,"ByFeeder"
                ,"ByHWY"
                ,"ByRR"
  ##              ,"BldgType"
  ##              ,"HouseStyle"
                ,"SalePrice"
                ,"OverallQual"
                ,"OverallCond"
                ,"YearBuilt"
                ,"Remodeled")]
  return (data)
}
