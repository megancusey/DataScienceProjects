accuracyMeasures <- function(pred, truth, name="model", prob){
  library(caret)
  ## Normalizethe deviance by th number of data points so we can compare the deviance
  ## across the test and training sets
  dev.norm <- -2*loglikelihood(as.numeric(truth),pred)/length(pred)
  ## Convert the class probability estimator into a classifer 
  ## by labeling documents that score greater than .5 as spam.
  ctable <- table(truth=truth,
                  pred=(pred > prob))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- precision(ctable)
  recall <-recall(ctable)
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, precision=precision,recall=recall, dev.norm)
}
## CALCULATE THE LOG LIKELIHOOD
loglikelihood <- function(y,py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1,1-1e-12,py))
  
  sum(y*log(pysmooth) + (1-y)*log(1-pysmooth))
}

data <- data_FS
summary(data$Target)

## 214000 represents the 3rd quartile/75%
## if the sale price is projected to be top dollar, then it's above 214000

data$HighDollar <- ifelse(data$Target >= 214000,"yes","no")
data$HighDollar <- factor(data$HighDollar)

split <- .8 
train <- splitData("train", split ,data)
test <- splitData("test", split,data)

data <- subset(data, select =-c(Target))
test <- subset(test, select =-c(Target))
train <- subset(train, select =-c(Target))

houseVars <- setdiff(colnames(data), list("rgroup","HighDollar"))
houseVars

## USE ALL THE FEATURES AND DO BINARY CLASSIFICATION WHERE TRUE = HIGH DOLLAR HOME
houseFormula <- as.formula(paste('HighDollar=="yes"', paste(houseVars,collapse=" + "),sep=" ~ "))

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

## Fit decision tree model using rpart library
treemodel <- rpart(houseFormula, train)
rpart.plot(treemodel)
treemodel

pred_decision <- predict(treemodel, newdata=train)
y_pred_decision = ifelse(pred_decision > .75, "yes","no")
y_pred_decision

## Making the Confusion Matrix
cm_decision = table(y_pred_decision,train$HighDollar)
cm_decision

precision(cm_decision)
recall(cm_decision)
accuracyMeasures(predict(treemodel, newdata=train), train$HighDollar=="yes",  name="tree, training",.75)
## FROM PREVIOUS ASSIGNMENT w/o FE
##           model  accuracy        f1 dev.norm
## 1 tree, training 0.9085851 0.6656575 0.476685

## W/ Feature Engineering
## 1 tree, training 0.9203822 0.7096442 0.4059239
accuracyMeasures(predict(treemodel, newdata=test), 
                 test$HighDollar=="yes",
                 name="tree, test",.75)

##        model  accuracy        f1 dev.norm
## 1 tree, test 0.8861386 0.3043478 0.634403
## w/ fe
## 1 tree, test 0.9257426 0.5157143 0.3980521

pred_decision_test <- predict(treemodel, newdata=test)
y_pred_decision_test = ifelse(pred_decision_test > .75, "yes","no")
cm_decision_test = table(y_pred_decision_test,test$HighDollar)
cm_decision_test
precision(cm_decision_test)
recall(cm_decision_test)
length(which(test$HighDollar=="yes"))

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
treelist
pred_bag <- predict.bag(treelist, newdata=train)
y_pred_bag = ifelse(pred_bag > .75, "yes","no")
y_pred_bag


plot( treelist )
text( tree.carseats , pretty =0)

## Making the Confusion Matrix
cm_bag = table(y_pred_bag,train$HighDollar)
cm_bag
library(caret)
precision(cm_bag)
##[1] 0.9858233
recall(cm_bag)
##[1] 0.9159068
accuracyMeasures(predict.bag(treelist, newdata=train),
                 train$HighDollar=="yes",
                 name="baggin, training",.75)

accuracyMeasures(predict.bag(treelist, newdata=test),
                 test$HighDollar=="yes",
                 name="bagging, test",.75)
pred_bag_test <- predict.bag(treelist, newdata=test)
y_pred_bag_test = ifelse(pred_bag_test > .75, "yes","no")
y_pred_bag_test

cm_bag_test = table(y_pred_bag_test,test$HighDollar)
cm_bag_test
library(caret)
precision(cm_bag_test)
##[1] 0.9858233
recall(cm_bag_test)
##[1] 0.9159068

library(ipred)
library(MASS)
bagging(HighDollar~.,data=train, coob=TRUE)
library(randomForest)

set.seed(5123512)
f <- randomForest(x=data[1:1458,-9], y=data$HighDollar[1:1458], ntree=100,importance=TRUE)
fmodel <- randomForest(x=train[,houseVars], ## independent variables
                       y=train$HighDollar, ## dependent variable
                       ntree = 100, ## default = 500 but want 100 to compare to bagging example
                       nodesize=5, ## specifies that each node of a tree must have a min of 7 features
                       importance=T) ## tells function to save information for calculating variable importance

predict(f,newdata=train[,houseVars],type="prob")[,"HighDollar"]

accuracyMeasures(predict(f,newdata=train[,houseVars],type="prob")[,"HighDollar"],
                 train$HighDollar=="yes", name="random forest, train")
names(train)
houseVars
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
varImp

varImpPlot(fmodel, type=1)


##        ACCURACY   F1       DEV.NORM 
## DECISION TREE      
## TRAIN  .9085851  .6656575  .476685
## TEST   .8861386  .3043478  .634403
## BAGGING  
## TRAIN  .9340223  .7616366  .378156
## TEST   .8712871  .2692308  .5006863

## w/ FEATURE ENGINEERING
##        ACCURACY   F1       DEV.NORM 
## DECISION TREE      
## TRAIN  0.9203822 0.7096442 0.4059239
## TEST   0.9257426 0.5157143 0.3980521
## BAGGING  
## TRAIN  0.933121 0.7601384 0.3156886
## TEST   0.9306931 0.5494505 0.3063632
