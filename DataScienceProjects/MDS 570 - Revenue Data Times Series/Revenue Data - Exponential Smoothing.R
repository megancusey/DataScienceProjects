library(fpp2)
## remove scientic notation
options(scipen = 999)
## import functions I want to use

## Sometimes my working directory gets reset to my OneDrive so
## need to set wd when getting started.
wd <- "C:/Users/cusey/source/repos/TimeSeriesProjectts/TimeSeriesProjects/TimeSeriesProjects"
setwd(wd);

## load in user defined functions.
source("functions.R", local = TRUE)

## Import CSV Data.
revenue.original <- read.csv("Revenue Data.csv", header = TRUE)

## Convert to Time Series Object
revenue.original.ts <- ts(data = revenue.original[, 'Revenue'], frequency = 12, start = c(2014, 7), end = c(2019, 8))

## Plot Original Data
autoplot(revenue.original.ts)

## 11/1/2017 is the outlier, taking the mean of all November #s. Including
## the outlier so the observation isn't completely ignored.
replace.outlier <- mean(c(14966599904, 13760762315, 11052122507, 10940462626, 33983939406))
revenue.smoothed.outlier.ts <- revenue.original.ts
revenue.smoothed.outlier.ts[41] <- replace.outlier

autoplot(revenue.smoothed.outlier.ts)

##STL Decomposition
revenue.original.ts.stl <- stl(revenue.original.ts, s.window = 13, robust = TRUE)
revenue.smoothed.outlier.ts.stl <- stl(revenue.smoothed.outlier.ts, s.window = 13, robust = TRUE)

revenue.original.ts.seasadj <- seasadj(revenue.original.ts.stl)
revenue.smoothed.outlier.ts.seasadj <- seasadj(revenue.smoothed.outlier.ts.stl)

autoplot(revenue.original.ts.stl)
autoplot(revenue.smoothed.outlier.ts.stl)


## Explore Data 

## Seasonal Plots
ggseasonplot(revenue.original.ts, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(revenue.original.ts, polar = TRUE, year.labels = TRUE, year.labels.left = TRUE)
ggsubseriesplot(revenue.original.ts)

qplot(revenue.original$`ï..Month.Year`,revenue.original$Revenue)

gglagplot(revenue.original.ts)

## Data Observations

## Always have more money in April... Why?? End of tax season is a thought.
## No clear upward trend per year as the order of the years as a function of revenue 
## is a bit sporatic. February is a consistent low point of revenue. There may be 
## some seasonality here.

## Split into test/train

## Original
revenue.train.ts <- ts(revenue.original[, 'Revenue'], frequency = 12, start = c(2014, 7), end = c(2018, 8))
revenue.test.ts <- ts(revenue.original[, 'Revenue'], frequency = 12, start = c(2018, 9), end = c(2019, 8))

## Smoothed Outlier
revenue.smoothed.outlier.train.ts <- window(revenue.smoothed.outlier.ts, start=c(2014,7), end=c(2018.8))
revenue.smoothed.outlier.test.ts <- window(revenue.smoothed.outlier.ts, start=c(2018,9), end=c(2019,8))

## STL Seasonally Adjusted Smoothed Outlier
revenue.smoothed.outlier.seasadj.train.ts <- window(revenue.smoothed.outlier.ts.seasadj, start=c(2014,7), end=c(2018.8))
revenue.smoothed.outlier.seasadj.test.ts <- window(revenue.smoothed.outlier.ts.seasadj, start=c(2018,9), end=c(2019,8))

## STL Seasonally Adjusted Smoothed Original
revenue.original.seasadj.train.ts <- window(revenue.original.ts.seasadj, start=c(2014,7), end=c(2018.8))
revenue.original.seasadj.test.ts <- window(revenue.original.ts.seasadj, start=c(2018,9), end=c(2019,8))


## Simple, Baseline approaches:
## Original Data
benchmark.methods.rmse <- findBenchmarkMethod(revenue.train.ts,revenue.test.ts, 12)
benchmark.methods.rmse

## according to the benchmark, Average Method is the best benchmark method
## though the RMSE is way high. I think I may need to replace the outlier
## towards the end of 2017 with an average for the month for the past 
## couple years (or whatever ends up working best).

## Smoothed the outlier
benchmark.methods.smoothed.outlier.rmse <- findBenchmarkMethod(revenue.smoothed.outlier.train.ts,revenue.smoothed.outlier.test.ts, 12)
benchmark.methods.smoothed.outlier.rmse

## seasonal method did much better after smoothing the effects of the outlier. May also try seasonally adjusted.

## Seasonal Method: 1364619541

## STL Seasonally Adjusted Smoothed Outlier
benchmark.methods.smoothed.outlier.seasadj.rmse <- findBenchmarkMethod(revenue.smoothed.outlier.seasadj.train.ts,revenue.smoothed.outlier.seasadj.test.ts, 12)
benchmark.methods.smoothed.outlier.seasadj.rmse

## DRIFT METHOD: 1222261139

## STL Seasonally Adjusted Original
benchmark.methods.seasadj.rmse <- findBenchmarkMethod(revenue.original.seasadj.train.ts,revenue.original.seasadj.test.ts, 12)
benchmark.methods.seasadj.rmse

## Out of All the Benchmark models and different data transformations, 
## the seasonally adjusted, smoothed outlier Drift Method has the best RMSE.
## I like smoothing the outlier because I think that there was probably
## some politcal one - time event that lead to a hike in the data, though
## I'll need to try to get some clarification on it from the business users.
## I like the smoothes seasonal method approach because I think it lends itself to
## the data more. I know currently, the business users look at the dollars available
## in revenue in the previous year (same time of year) in order to help make their
## judgement. Since the seasonal method essentially does this, I'd like to keep it around as
## a benchmark and see how well more advanced models do with the same data transformation & seasonal 
## component.

## Simple Exponential Smoothing

## Smoothed Outlier
exponential.smoothing.methods.smoothing.outlier <- ExponentialSmoothingMethods (revenue.smoothed.outlier.train.ts,revenue.smoothed.outlier.test.ts, 12)
exponential.smoothing.methods.smoothing.outlier

## STL Seasonally Adjusted Smoothed Outlier
exponential.smoothing.methods.smoothing.outlier.seasadj <- ExponentialSmoothingMethods (revenue.smoothed.outlier.seasadj.train.ts,revenue.smoothed.outlier.seasadj.test.ts, 12)
exponential.smoothing.methods.smoothing.outlier.seasadj

## Simple drift method did better than the more complex exponential smoothing methods.

rm(list = ls())