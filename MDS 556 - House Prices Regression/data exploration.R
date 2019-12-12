library(purrr)
library(tidyr)
library(ggplot2)

##https://drsimonj.svbtle.com/quick-plot-of-all-variables

## TARGET VARIABLE
summary(data$SalePrice)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  34900  129975  163000  180921  214000  755000 

ggplot(data=data[!is.na(data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(binwidth = 10000) +
  ylab("Frequency") + 
  ggtitle("Distribution of Sale Price")
## NUMERIC VARIABLES
numeric_variables <- which(sapply(data, is.numeric)) #index vector numeric variables
numeric_variables <- numeric_variables[-c(3,  ## Alley
                                          9,  ## CentralAir
                                          33, ## ByRR
                                          34, ## ByHWY
                                          35, ## ByFeeder
                                          36, ## ByPositiveFeature
                                          37, ## Remodeled
                                          38, ## HasMasonVeneer
                                          39  ## HasPool
                                          )]
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


names(numeric_variables)
cor_numVar <- cor(data[,numeric_variables], use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
