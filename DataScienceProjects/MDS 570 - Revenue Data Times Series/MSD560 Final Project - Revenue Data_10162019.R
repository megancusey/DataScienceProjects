## LOAD REQUIRED LIBRARIES
library(fpp2)
library(forecast)
library(urca)

##rm(list = ls())

## remove scientic notation
options(scipen = 999)

## Sometimes my working directory gets reset to my OneDrive so
## need to set wd when getting started.

wd <- "C:/Users/cusey/source/repos/TimeSeriesProjectts/TimeSeriesProjects/TimeSeriesProjects"
setwd(wd)

## Import CSV Data.
data <- read.csv("Revenue Data.csv", header = TRUE)

## Convert to Time Series Object
myts <- ts(data = data[, 'Revenue'], frequency = 12, start = c(2014, 7), end = c(2019, 8))

#########################################################
## DATA EXPLORATION:

## Plot Original Data
autoplot(myts) + xlab("Month/Year") + ylab("Monthly Revenue")+ggtitle("Time Plot of Revenue")

## Maybe apply transformation to remove variance in outlier before
## 2018?
BoxCox(myts,BoxCox.lambda(myts)) %>% autoplot()

myts.boxcox <- BoxCox(myts,BoxCox.lambda(myts)) 

log(myts) %>% autoplot()

## Plot Original Data
autoplot(myts) + xlab("Month/Year") + ylab("Monthly Revenue")+ggtitle("Time Plot of Revenue")

## Seasonal Plots
ggseasonplot(myts, year.labels = TRUE, year.labels.left = TRUE) 

ggseasonplot(myts, polar = TRUE, year.labels = TRUE, year.labels.left = TRUE)
ggsubseriesplot(myts)

gglagplot(myts)

## 11/1/2017 is the outlier, taking the mean of all November 2017 and November 2016
## I don't like that it's one of the last observations seen in the training data
## and is used to forecast the test data. I think it's more important that we smooth
## the value and get an entire year for the test data. The alternative is to only
## evaluate the models based on training data which I don't want to do either.
replace.outlier <- mean(c(13760762315,33983939406))
myts[41] <- replace.outlier


## Plot Original Data W/ Smoothed Outlier
autoplot(myts) + xlab("Month/Year") + ylab("Monthly Revenue")+ggtitle("Time Plot of Revenue")


######################################################
## Split into Test/Train Data Sets
myts.train <- window(myts, start=c(2014,7), end=c(2018,08))
myts.test <-  window(myts, start=c(2018,09), end=c(2019,8))

myts.h <- 12
#######################################################
## Simple Approaches

## Average Method
fit.average.method <- myts.train %>% meanf(h=myts.h)

accuracy(fit.average.method, myts.test)

## Naive Method 
fit.naive.method <- naive(myts.train, h = myts.h)
accuracy(fit.naive.method, myts.test)

## Seasonal Naive Method - w/o boxcox
fit.seasonal.naive.method <- snaive(myts.train, h = myts.h)
accuracy(fit.seasonal.naive.method, myts.test)

## Seasonal Naive Method - w boxcox
boxcox.fit.seasonal.naive.method <- myts.train %>% snaive(h=myts.h, lambda="auto", biasadj = TRUE)
accuracy(boxcox.fit.seasonal.naive.method, myts.test)

## Drift Method w/o boxcox
fit.drift.method <- rwf(myts.train, h = myts.h, drift = TRUE)
accuracy(fit.drift.method, myts.test)

## Drift Method w boxcox
boxcox.fit.drift.method <- rwf(myts.train, h = myts.h, drift = TRUE, lambda = "auto", biasadj=TRUE)
accuracy(boxcox.fit.drift.method, myts.test)


autoplot(myts) +
  ##autolayer(myts.test, series = "Observed Values") +
  autolayer(fit.average.method, series = "Average Method", PI = FALSE) +
  autolayer(fit.naive.method, series = "Naive Method", PI = FALSE) +
  autolayer(fit.seasonal.naive.method, series = "Seasonal Naive Method", PI = FALSE) +
  autolayer(fit.drift.method, series = "Drift Method", PI = FALSE) +
  autolayer(boxcox.fit.seasonal.naive.method, series = "BC Seasonal Naive Method", PI = FALSE) +
  autolayer(boxcox.fit.drift.method, series = "BC Drift Method", PI = FALSE) +
  xlab("Month/Year") + ylab("Revenue") + ggtitle("Simple Methods Forecast Results")

## SEASONAL NAIVE METHOD WITH BOXCOX TRANSFORMATION
## PRODUCES THE LOWEST RMSE.

## CHECK RESIDUALS
checkresiduals(residuals(fit.drift.method))

#############################################################
## Linear Regression

bc.fit.linear.regression <- tslm(myts.train~ trend+season, lambda="auto",biasadj = TRUE)

bc.fit.linear.regression.forecast <- fit.linear.regression %>% forecast(h=myts.h)
accuracy(bc.fit.linear.regression.forecast, myts.test)

fit.linear.regression <- tslm(myts.train~ trend+season)

fit.linear.regression.forecast <- fit.linear.regression %>% forecast(h=myts.h)
accuracy(fit.linear.regression.forecast, myts.test)

autoplot(myts) +
  autolayer(fitted(fit.linear.regression), series = "Fitted") +
  autolayer(bc.fit.linear.regression.forecast, series = "W/ Box Cox Forecasted", PI = FALSE) +
  autolayer(fit.linear.regression.forecast, series = "W/O Box Cox Forecasted", PI = FALSE) +
  xlab("Month/Year") + ylab("Revenue") + ggtitle("Linear Regression Model Results")

## check residuals
autoplot(residuals(bc.fit.linear.regression))

qplot(fitted(bc.fit.linear.regression), residuals(bc.fit.linear.regression))

checkresiduals(bc.fit.linear.regression)
## a small p value indicates there is significant autocorrelation
## remaining in the residuals
##	Breusch-Godfrey test for serial correlation of order up to 16

## data:  Residuals from Linear regression model
## LM test = 20.527, df = 16, p-value = 0.1974


#############################################################
## Exponential Smoothing

## Simple Exponential Smoothing - W/ BC
bc.fit.ses <- ses(myts.train,h=myts.h, lambda="auto", biasadj = TRUE)
accuracy(bc.fit.ses,myts.test)

bc.fit.ses[["model"]]

## Simple Exponential Smoothing - W/O BC
fit.ses <- ses(myts.train,h=myts.h)
accuracy(fit.ses,myts.test)

fit.ses[["model"]]

## Holt Linear Trend W/ BC
bc.fit.holt.linear.trend <- holt(myts.train,h=myts.h,lambda = "auto", biasadj=TRUE)
accuracy(bc.fit.holt.linear.trend,myts.test)

bc.fit.holt.linear.trend[["model"]]

## Holt Linear Trend Damped W/ BC
bc.fit.holt.linear.trend.damped <- holt(myts.train,h=myts.h,lambda = "auto", biasadj=TRUE, damped = TRUE)
accuracy(bc.fit.holt.linear.trend.damped,myts.test)

bc.fit.holt.linear.trend.damped[["model"]]

## Holt Linear Trend W/O BC
fit.holt.linear.trend <- holt(myts.train,h=myts.h)
accuracy(fit.holt.linear.trend,myts.test)

fit.holt.linear.trend[["model"]]

## Holt Linear Trend Damped W/O BC
fit.holt.linear.trend.damped <- holt(myts.train,h=myts.h, damped=TRUE)
accuracy(fit.holt.linear.trend.damped,myts.test)

fit.holt.linear.trend.damped[["model"]]

## Holt Winters Seasonal Method
## Additive
fit.hw.additive <- hw(myts.train, h=5, seasonal="additive",lambda = "auto", biasadj=TRUE)
accuracy(fit.hw.additive,myts.test)

fit.hw.additive[["model"]]

fit.hw.additive.damped <- hw(myts.train, h=5, seasonal="additive", damped=TRUE, lambda = "auto", biasadj=TRUE)
accuracy(fit.hw.additive.damped,myts.test)

fit.hw.additive.damped[["model"]]
## Multiplicative
fit.hw.multiplicative <- hw(myts.train, h=5, seasonal="multiplicative",lambda = "auto", biasadj=TRUE)
accuracy(fit.hw.multiplicative,myts.test)

fit.hw.multiplicative[["model"]]

fit.hw.multiplicative.damped <- hw(myts.train, h=5, seasonal="multiplicative", damped=TRUE,lambda = "auto", biasadj = TRUE)
accuracy(fit.hw.multiplicative.damped,myts.test)

fit.hw.multiplicative.damped[["model"]]


fit.ets <- ets(myts.train, lambda="auto",biasadj=TRUE)
summary(fit.ets,myts.test)

forecast.ets <- fit.ets %>% forecast(h=5)


autoplot(myts) +
  autolayer(fit.ses, series = "Simple E.S.", PI = FALSE) +
  autolayer(fit.holt.linear.trend, series = "Holt Linear W/O Damp", PI = FALSE) +
  autolayer(fit.holt.linear.trend.damped, series = "Holt Linear W/ Damp", PI = FALSE) +
  xlab("Month/Year") + ylab("Revenue") + ggtitle("Exponential Smoothing Results 1")

autoplot(myts) +
  autolayer(fit.hw.additive, series = "HW Additive", PI = FALSE) +
  autolayer(fit.hw.additive.damped, series = "HW Additive - Damped", PI = FALSE) +
  autolayer(fit.hw.multiplicative, series = "HW Multiplication", PI = FALSE) +
  autolayer(fit.hw.multiplicative.damped, series = "HW Multiplication - Damped", PI = FALSE) +
  autolayer(forecast.ets, series = "ETS", PI = FALSE) +
  xlab("Month/Year") + ylab("Revenue") + ggtitle("Exponential Smoothing Results 2")