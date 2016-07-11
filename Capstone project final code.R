# Read in the dataset in R
export <- read.csv("Crudeoil.csv", header = FALSE)
# install packages for time series analysis
install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)

# Converting the dataset into time series format
exportts <- ts(export, frequency = 12, start = c(2000,1), end = c(2014, 12))
exportts

# Checking for NA values
is.na(exportts)
class(exportts) # this shows that it is a time series data
cycle(exportts) # this prints the cycle across years

#Checking for seasonality
boxplot(exportts~cycle(exportts), xlab = "Months", ylab = "Mb/d")
summary(exportts)

# Plotting time series data
plot(exportts, type = "o", ylab = "Mb/d", main = "Crude oil Export between Canada and USA")
acf(exportts, main = "Exportts")
pacf(exportts, main = "Exportts")
acf(exportts, main = "Exportts", plot = FALSE) # display summary result of ACF without the plot
pacf(exportts, main = "Exportts", plot = FALSE)# display summary result of PACF without the plot

#Decomposing our dataset into its components

exporttscmpt <- decompose(exportts)
exporttscmpt$seasonal 
exporttscmpt$trend
plot(exporttscmpt, type = "o")
plot(exporttscmpt$seasonal, type = "o")

#Checking stationarity of the dataset

adf.test(exportts, alternative = "stationary", k = 0)
adf.test(exportts, alternative = "explosive", k = 0)
adf.test(exportts)

#Differencing our dataset to obtain stationarity

exporttsdiff1 <- diff(exportts, differences = 1)
exporttsdiff1
summary(exporttsdiff1)
plot.ts(exporttsdiff1, ylab = "exporttsdiff1")
exporttsdiff2 <- diff(exportts, differences = 2)
exporttsdiff2
summary(exporttsdiff2)
plot.ts(exporttsdiff2, ylab = "exporttsdiff2")
abline(h=0,col = "red")
hist(exporttsdiff2)
exporttsdiff3 <- diff(exportts, differences = 3)
exporttsdiff3
summary(exporttsdiff3)

#Stationarity check of the differenced data

acf(exporttsdiff2, lag.max = 24, main = "Exporttsdiff2", plot = FALSE)
pacf(exporttsdiff2, lag.max = 24, main = "Exporttsdiff2", plot = FALSE)
adf.test(exporttsdiff2, k = 0)
adf.test(exporttsdiff2)

# Getting an idea of p,q value of the ARIMA(p,d,q) method using autoarima

fit <- auto.arima(exporttsdiff2)
fit

# Several other p,q values were also used to check for best fit
exparima1 <- arima(exporttsdiff2, order = c(5, 2, 1), seasonal = c(1,0,0))
exparima1
exparima2 <- arima(exporttsdiff2, order = c(3, 2, 1), seasonal = c(1,0,0))
exparima2
exparima3 <- arima(exporttsdiff2, order = c(4, 2, 1), seasonal = c(1,0,0))
exparima3
exparima5 <- arima(exporttsdiff2, order = c(5, 0, 1), seasonal = c(1,0,0))
exparima5
exparima6 <- arima(exporttsdiff2, order = c(4, 1, 2),seasonal = c(1,0,0))
exparima6
exparima7 <- arima(exporttsdiff2, order = c(3, 1, 3), seasonal = c(1,0,0))
exparima7

# Prediction/Forecasting for 2015 using the best fit model and plotting it

expforecasts <- forecast.Arima(exparima5, h=12)
expforecasts
summary(expforecasts)
plot(expforecasts, main = "Forecast from ARIMA(5,0,1)(1,0,0)[12]")
exparima.pred1 <- predict(exparima5, n.ahead = 12)
exparima.pred1
exparima.pred1$pred
exparima.pred1$se
lines(exparima.pred1$pred, col = "blue", xlab = "Time", ylab = "exparima.pred1")
lines(exparima.pred1$pred+2*exparima.pred1$se, col = "red")
lines(exparima.pred1$pred-2*exparima.pred1$se, col = "red")
plot(exparima.pred1, main = "Forecast from ARIMA(5,0,1)(1,0,0)[12]")

# Checking the actual vs. fitted value

acf(expforecasts$residuals, lag.max = 24)# check for stationarity
# Check for autocorrelation of time series different from zero
Box.test(expforecasts$residuals, lag = 24, type = "Ljung-Box") 
plot.ts(expforecasts$residuals)
residual <- resid(expforecasts)
hist(residual, col = "lightblue", freq = FALSE)

# Read in the 2015 dataset for comparison with the forecasted values
checkoil <- read.csv("checkoil.csv", header = FALSE)
# Converting it into time series format and plotting the data
checkoilts <- ts(checkoil, frequency = 12, start = c(2000,1), end = c(2015, 12))
checkoilts
plot.ts(checkoilts)
# Differencing the data to make it stationary and plotting it
checkoiltsdiff2 <- diff(checkoilts, differences = 2)
checkoiltsdiff2
plot(checkoiltsdiff2, ylab = "checkoiltsdiff2")
