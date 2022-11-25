library(TSA)
library(forecast)
library(lmtest)
library(rugarch)
library (fGarch)
library (qrmtools)
cpi_data <- read.csv("DataSet/CPIUSSA.csv", header=TRUE) #class dataframe
colnames(cpi_data) <- c('date','cpi') #rename
cpi_data$date <- as.Date(cpi_data$date) #Set date as date time format
#rownames(cpi_data) <- cpi_data$date #Set date as index

cpi_data <- subset(cpi_data, date >= as.Date("1980-01-01")) #remove data before 1980

cpi_train <- subset(cpi_data, date <= as.Date("2022-05-01")) #for training the model

cpi_test <- subset(cpi_data, date > as.Date("2022-05-01")) #for testing the model

cpi_ts <- ts(cpi_train$cpi, frequency = 12, start=c(1980,1))

ggtsdisplay(cpi_ts, lag.max = 50)
adf.test(cpi_ts)

ggtsdisplay(log(cpi_ts), lag.max = 50)

# log d = 1
cpi_ts.rdiff = diff(log(cpi_ts), lag = 1, differences = 1)
ggtsdisplay(cpi_ts.rdiff, lag.max = 50)

# log sd = 1
cpi_ts.sdiff = diff(log(cpi_ts), lag = 12, differences = 1)
ggtsdisplay(cpi_ts.sdiff, lag.max = 50)

# log d = 1 sd = 1
cpi_ts.rdiff.sdiff = diff(diff(log(cpi_ts), lag = 12, differences = 1))
ggtsdisplay(cpi_ts.rdiff.sdiff, lag.max = 50)

# log d = 1 sd = 1
cpi_ts.rdiff.sdiff = diff(diff(log(cpi_ts), lag = 12, differences = 1), differences = 2)
ggtsdisplay(cpi_ts.rdiff.sdiff, lag.max = 50)
adf.test(cpi_ts.rdiff.sdiff)

model_fit = Arima(log(cpi_ts), order = c(2,1,1), seasonal = c(0,1,1), lambda = NULL, include.constant = TRUE)

autoplot(model_fit)
coeftest(model_fit)
ggtsdisplay(model_fit$residuals, lag.max = 50)
ggtsdisplay((model_fit$residuals)**2, lag.max = 50)
plot(resid(model_fit)**2, ylab='Squared residue')

#https://www.rdocumentation.org/packages/TSA/versions/1.3/topics/McLeod.Li.test
#The null hypothesis: No (ARCH) among the lags considered. if p ≤ α=0.05, reject H0
McLeod.Li.test(model_fit, y=log(cpi_ts))


