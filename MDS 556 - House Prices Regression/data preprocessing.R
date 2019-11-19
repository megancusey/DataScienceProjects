rm(list = ls())
dataPreprocessing <- function(){
  setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')
  options(scipen = 999)
  data <- read.csv(file='train.csv', header=TRUE)
  
  ## 1 - FEATURE: ID = REMOVE
  data <- data[,2:81]
  
  ##TMP
  data <- data[,c(1:12,80)]
  
  ## 2 - FEATURE: MSSUBCLASS, turn to factor
  data$MSSubClass = factor(data$MSSubClass,
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
  
  ## 7 - FEATURE: Alley, turn to factor but later will
  ## need to do something b/c I think no alley access
  ## may need to be separate. 
  
  data$Alley = factor(data$Alley,
                      levels = c(   'Grvl'
                                    ,'Pave'
                                    ,'Not Applicable'
                      ),
                      labels = c("Gravel","Paved","Not Applicable"))
  
  data$Alley[is.na(data$Alley)] <- "Not Applicable"  
  
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

  ## 12 - FEATURE: Neighborhood, turn to factor
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
  
  return (data)
}
