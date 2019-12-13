## USE BAGGING TO HELP REDUCE OVERFITTING, TRAINING VARIANCE (SAMPLES TAKEN FROM THE SAME 
## POPULATION CAN PRODUCE DECISION TREES WITH DIFFERENT STRUCTURES/PREDICTION ACCURACY),
## AND INCREASE PREDICTION ACCURACY.


###################################################################################
## THE FOLLOWING IS AN EXAMPLE OF A DECISION TREE W/O BAGGING
###################################################################################
rm(list = ls())
setwd("C:/Users/cusey/source/repos/DataScienceCoursework/MDS 556 - 2019 Fall B/")
spamD <- read.table("spamD.tsv.txt", header=T, sep="\t")

## SPLIT INTO TEST/TRAIN
spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)

spamVars <- setdiff(colnames(spamD), list("rgroup","spam"))
spamVars

## USE ALL THE FEATURES AND DO BINARY CLASSIFICATION WHERE TRUE = SPAM DOCUMENTS
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars,collapse=" + "),sep=" ~ "))

## CALCULATE THE LOG LIKELIHOOD
loglikelihood <- function(y,py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1,1-1e-12,py))
  
  sum(y*log(pysmooth) + (1-y)*log(1-pysmooth))
}

##  CALCULATE AND RETURN THE VARIOUS MEASURES ON THE MODEL
## 1. NORMALIZED THE DEVIANCE
## 2. PREDICTION ACCURACY
## 3. F1 (PRODUCT OF PRECISION AND RECALL)

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

library(rpart)
## Fit decision tree model using rpart library
treemodel <- rpart(spamFormula, spamTrain)

accuracyMeasures(predict(treemodel, newdata=spamTrain), 
                 spamTrain$spam=="spam",
                 name="tree, training")

##            model  accuracy        f1  dev.norm
## 1 tree, training 0.9104514 0.7809002 0.5618654

accuracyMeasures(predict(treemodel, newdata=spamTest), 
                 spamTest$spam=="spam",
                 name="tree, test")
##       model  accuracy        f1  dev.norm
## 1 tree, test 0.8799127 0.7091151 0.6702857

###################################################################################
## RESULT: THE ACCURACY ON THE TEST SET AND F1 SCORES DEGRADE WHEN APPLYING THE  ##
## DECISION TREE MODEL TO THE TEST SET (AS COMPARE TO THE TRAINING SET).         ##
###################################################################################

#####################################################################################
## NEXT, LET US SEE HOW BAGGING IMPACTS THE ACCURACY MEASURES OF THE DECISION TREE ##
#####################################################################################

## Use bootstrap samples that are the same size as the training set, with 100 trees.
ntrain <- dim(spamTrain)[1] ## returns # of rows in spamTrain (4143)
n <- ntrain
ntree <- 100

## BUILD THE BOOTSTRAP SAMPLES BY SAMPLING THE ROW INDICES OF spamTrain WITH REPLACEMENT
## EACH COLUMN OF THE MATRIX SAMPLES REPRESENT THE ROW INDICES INTO spamTrain THAT COMPRISE
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
                     rpart(spamFormula, spamTrain[samp,])
                   })

## Use predict.bag that assumes the underlying classifier to return decidion probabilities
## instead of decisions
predict.bag <- function(treelist, newdata){
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)
                  })
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

accuracyMeasures(predict.bag(treelist, newdata=spamTrain),
                 spamTrain$spam=="spam",
                 name="baggin, training")
##              model  accuracy        f1  dev.norm
## 1 baggin, training 0.9239681 0.8119212 0.4659072

accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test")
##          model  accuracy        f1  dev.norm
## 1 bagging, test 0.9039301 0.7598101 0.5261388

###################################################################################
## RESULT: THE ACCURACY ON THE TEST SET AND F1 SCORES IMPROVE DRAMATRICALLY WHEN ##
## COMPARED TO THE MODEL THAT PRODUCED ONLY ONE DECISION TREE. THE MODEL CAN BE  ##
## IMPROVED FURTHER BY APPLYING RANDOM FORESTS.                                  ##
###################################################################################

#####################################################################################
## NEXT, LET US SEE HOW RANDOM FOREST IMPACTS THE ACCURACY MEASURES                ##
#####################################################################################

## BAGGING USES RANDOM DATASETS, BUT SAME FEATURES.
## RESULT = INDIVIDUAL TREES ARE MORE LIKELY CORRELATED WITH EACH OTHER.
## IF ONE TREE TENDS TO MAKE MISTAKES, OTHERS ARE MORE LIKELY TO MAKE MISTAKES THERE ALSO
## RANDOM FOREST SEEKS TO DE-CORRELATE THE TREES BY RANDOMIZING THE SET OF VARIABLES
## ALLOWED TO USES.

## RANDOM FOREST APPLIES THE FOLLOWING STEPS:
## 1. GATHERS BOOTSTRAPPED SAMPLE FROM THE TRAINING DATA
## 2. FOR EACH SAMPLE, PRODUCE A DECISION TREE. AT EACH NODE OF THE TREE:
##   A. RANDOMLY DRAW A SUBSET OF VARIABLES FROM THE TOTAL FEATURES AVAILABLE.
##   B. PICK THE BEST VARIABLE AND SPLIT FROM THE SET OF VARIABLES.
##   C. CONTINUE UNTIL THE TREE IS GROWN

library(randomForest)

set.seed(5123512)

fmodel <- randomForest(x=spamTrain[,spamVars], ## independent variables
                       y=spamTrain$spam, ## dependent variable
                       netree = 100, ## default = 500 but want 100 to compare to bagging example
                       nodesize=7, ## specifies that each node of a tree must have a min of 7 features
                       importance=T) ## tells function to save information for calculating variable importance

accuracyMeasures(predict(fmodel,
                 newdata=spamTrain[,spamVars], type='prob')[,'spam'],
                 spamTrain$spam=="spam", name="random forest, train")

##                 model accuracy        f1  dev.norm
## 1 random forest, train 0.989621 0.9737141 0.1420866

accuracyMeasures(predict(fmodel,
                 newdata=spamTest[,spamVars], type='prob')[,'spam'],
                 spamTest$spam=="spam", name="random forest, test")

##                model  accuracy        f1  dev.norm
## 1 random forest, test 0.9563319 0.8897059 0.3019047

###################################################################################
## RESULT: THE ACCURACY OF THE RANDOM FOREST MODEL WAS MUCH BETTER THAN THE      ##
## SINGLE DECISION TREEY AND BAGGED MODELS. THOUGH IF YOU TAKE THE DIFFERENCE OF ##
## THE ACCURACY MEASURES FOR THE TEST AND TRAIN SET OF EACH MODELS, YOU'LL SEE   ##
## THAT THE BAGGED MODEL MINIMIZED THE GENERALIZED ERROR BETWEEN TEST/TRAIN      ##
## WHILE THE RANDOM FOREST MODEL WAS CLOSER TO THE SINGLE DECISION TREE.         ##
###################################################################################

#####################################################################################
## NEXT, LET US EXAMINE RANDOM FOREST, VARIABLE IMPORTANCE                         ##
#####################################################################################

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

## knowing the importance of the features can help in feature reduction.

## lets see what happens if we use only the 25 most important features instead
## of all 57.

## sort variables by their importance, measured by accuracy changed
selVars <- names(sort(varImp[,1], decreasing=T))[1:25]

fsel <- randomForest(x=spamTrain[,selVars],
                     y=spamTrain$spam,
                     ntree=100,
                     nodesize=7,
                     importance=T)

accuracyMeasures(predict(fsel,
                         newdata=spamTrain[,selVars],
                         type='prob')[,'spam'],
                         spamTrain$spam=="spam",
                         name="RF small, train")

##           model  accuracy        f1  dev.norm
##1 RF small, train 0.9862419 0.9651595 0.1500728

accuracyMeasures(predict(fsel,
                         newdata=spamTest[,selVars],
                         type='prob')[,'spam'],
                 spamTest$spam=="spam",
                 name="RF small, test")
##            model  accuracy        f1  dev.norm
## 1 RF small, test 0.9519651 0.8793605 0.3035443

###################################################################################
## RESULT: THE SMALLER RANDOM FOREST MODEL PERFORMED JUST AS WELL AS THE RANDOM  ##
## MODEL PRODUCED WITH 57 FEATURES.
###################################################################################
