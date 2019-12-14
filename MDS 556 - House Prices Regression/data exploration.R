library(purrr)
library(tidyr)
library(ggplot2)

##https://drsimonj.svbtle.com/quick-plot-of-all-variables
about_target <- function(data) {
## TARGET VARIABLE
summary(data$SalePrice)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  34900  129975  163000  180921  214000  755000 

ggplot(data=data[!is.na(data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(binwidth = 10000) +
  ylab("Frequency") + 
  ggtitle("Distribution of Sale Price")

}

numeric_correlation_plot <- function(data) {
  ## NUMERIC VARIABLES
  numeric_variables <- which(sapply(data, is.numeric)) #index vector numeric variables
  binary_variables <- c(grep("Alley",colnames(data)),  ## Alley
                        grep("CentralAir",colnames(data)),  ## CentralAir
                        grep("ByRR",colnames(data)), ## ByRR
                        grep("ByHWY",colnames(data)), ## ByHWY
                        grep("ByFeeder",colnames(data)), ## ByFeeder
                        grep("ByPositiveFeature",colnames(data)), ## ByPositiveFeature
                        grep("Remodeled",colnames(data)), ## Remodeled
                        grep("HasMasonVeneer",colnames(data)), ## HasMasonVeneer
                        grep("HasPool",colnames(data))  ## HasPool
                        )
  numeric_variables <- numeric_variables[-binary_variables]
  numeric_variables ## remove binary features
  data[numeric_variables] %>%
    keep(is.numeric) %>% 
    gather() %>%
    ggplot(aes(value)) +
      facet_wrap(~ key, scales="free") +
      geom_histogram() + 
      ylab("Frequency") + 
      xlab("Features") +
      ggtitle("Distribution")

  ## FACTORS
  factor_variables <- which(sapply(data, is.factor))
  
  cat('There are ',length(numeric_variables),' numeric variables, ',
      length(factor_variables),' categoric/ordinal variables, and ', 
      length(binary_variables),' binary variables.') 
  
  names(numeric_variables)
  cor_numVar <- cor(data[,numeric_variables], use="pairwise.complete.obs") #correlations of all numeric variables
  cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
  #select only high corelations
  CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
  cor_numVar <- cor_numVar[CorHigh, CorHigh]
  corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
  
}



## Random Forest
importantvariables_rf <- function(data) {
  
  target_index <- grep("SalePrice",colnames(data))
  numb_of_rows <- nrow(data)
  set.seed(2018)
  
  quick_RF <- randomForest(x=data[1:numb_of_rows,-target_index], y=data$SalePrice[1:numb_of_rows], ntree=100,importance=TRUE)
  imp_RF <- importance(quick_RF)
  imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
  imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
  
  ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
}
