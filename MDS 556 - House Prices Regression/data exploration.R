library(purrr)
library(tidyr)
library(ggplot2)

##https://drsimonj.svbtle.com/quick-plot-of-all-variables

## TARGET VARIABLE
summary(data$SalePrice)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  34900  129975  163000  180921  214000  755000 

ggplot(data=data[!is.na(data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(binwidth = 10000) 
## NUMERIC VARIABLES

data %>%
  keep(is.numeric) %>% 
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales="free") +
    geom_histogram()
