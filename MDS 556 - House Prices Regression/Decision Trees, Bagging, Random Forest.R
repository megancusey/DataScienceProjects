rm(list = ls())
setwd('C:/Users/cusey/source/repos/DataScienceProjects/MDS 556 - House Prices Regression')

## data preprocessing
source("data preprocessing.R", local = TRUE)
data <- dataPreprocessing()

summary(data$SalePrice)
## 214000 represents the 3rd quartile/75%
## if the sale price is projected to be top dollar, then it's above 214000

data$HighDollar <- ifelse(data$SalePrice >= 214000,"yes","no")
data$HighDollar <- factor(data$HighDollar)

split <- .8 
train <- splitData("train", split ,data)
test <- splitData("test", split,data)

data <- subset(data, select =-c(SalePrice))
test <- subset(test, select =-c(SalePrice))
train <- subset(train, select =-c(SalePrice))

houseVars <- setdiff(colnames(data), list("rgroup","HighDollar"))
houseVars

## USE ALL THE FEATURES AND DO BINARY CLASSIFICATION WHERE TRUE = SPAM DOCUMENTS
houseFormula <- as.formula(paste('HighDollar=="yes"',
                                paste(houseVars,collapse=" + "),sep=" ~ "))

library(rpart)

## Fit decision tree model using rpart library
treemodel <- rpart(houseFormula, train)

accuracyMeasures(predict(treemodel, newdata=train), 
                 train$HighDollar=="yes",
                 name="tree, training")
##           model  accuracy        f1 dev.norm
## 1 tree, training 0.9085851 0.6656575 0.476685

accuracyMeasures(predict(treemodel, newdata=test), 
                 test$HighDollar=="yes",
                 name="tree, test")

##        model  accuracy        f1 dev.norm
## 1 tree, test 0.8861386 0.3043478 0.634403


## Use bootstrap samples that are the same size as the training set, with 100 trees.
ntrain <- dim(train)[1] ## returns # of rows in train (1258)
n <- ntrain
ntree <- 100


## BUILD THE BOOTSTRAP SAMPLES BY SAMPLING THE ROW INDICES OF TRAIN WITH REPLACEMENT
## EACH COLUMN OF THE MATRIX SAMPLES REPRESENT THE ROW INDICES INTO TRAIN THAT COMPRISE
## THE BOOTSTRAP SAMPLE.
samples <- sapply(1:ntree, ## loop 1-100
                  FUN = function(iter){
                    ## obtain 100 samples, with replacement
                    sample(1:ntrain, 
                           size=n, 
                           replace=T)
                  })

## TRAIN THE INDIVIDUAL TREES FOR THE SAMPLES CREATED ABOVE AND RETURN THEM IN A LIST
treelist <- lapply(1:ntree,
                   FUN=function(iter){
                     samp <- samples[,iter];
                     rpart(houseFormula, train[samp,])
                   })

## Use predict.bag that assumes the underlying classifier to return decision probabilities
## instead of decisions
predict.bag <- function(treelist, newdata){
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)
                  })
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracyMeasures(predict.bag(treelist, newdata=train),
                 train$HighDollar=="yes",
                 name="baggin, training")

accuracyMeasures(predict.bag(treelist, newdata=test),
                 test$HighDollar=="yes",
                 name="bagging, test")


library(randomForest)

set.seed(5123512)

fmodel <- randomForest(x=train[,houseVars], ## independent variables
                       y=train$HighDollar, ## dependent variable
                       ntree = 100, ## default = 500 but want 100 to compare to bagging example
                       nodesize=7, ## specifies that each node of a tree must have a min of 7 features
                       importance=T) ## tells function to save information for calculating variable importance

predict(fmodel,
        newdata=train[,houseVars], type='prob')[,'HighDollar']

accuracyMeasures(predict(fmodel,
                         newdata=train[,houseVars], type='prob')[,'HighDollar'],
                 train$HighDollar=="yes", name="random forest, train")

##Error in predict(fmodel, newdata = train[, houseVars], type = "prob")[,  : 
##   subscript out of bounds

accuracyMeasures(predict(fmodel,
                 newdata=test[,houseVars], type='prob')[,'HighDollar'],
                 test$HighDollar=="yes", name="random forest, test")

##Error in predict(fmodel, newdata = train[, houseVars], type = "prob")[,  : 
##   subscript out of bounds
  
## VARIABLE IMPORTANCE IS CALCULATED DURING RANDOM FOREST TECHNIQUE
## RANDOM FOREST OBSERVES THE IMPACT A VARIABLE HAS ON THE DECISION
## TREE'S ACCURACY. IF THE IMPACT IS LARGE, THEN THE VARIABLE IS CONSIDERED 
## TO BE IMPORTANT. IN ADDITION, THE DECREASE IN NODE PURITY IS ALSO
## MEASURED WHEN A SPLIT IS PERFORMED ON THE VARIABLE.

varImp <- importance(fmodel)
## importance returns a matrix of importance measures.
## the larger the value, the more important the feature is
varImp[1:10,]

varImpPlot(fmodel, type=1)


##        ACCURACY   F1       DEV.NORM 
## DECISION TREE      
## TRAIN  .9085851  .6656575  .476685
## TEST   .8861386  .3043478  .634403
## BAGGING  
## TRAIN  .9340223  .7616366  .378156
## TEST   .8712871  .2692308  .5006863

