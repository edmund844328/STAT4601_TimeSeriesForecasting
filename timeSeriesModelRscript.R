install.packages('TSA')
install.packages('forecast')
install.packages('lmtest')
install.packages('rugarch')
install.packages('fGarch')
install.packages('qrmtools')
install.packages('fpp2')
#library(aTSA)
library(lmtest)
library(rugarch)
library (fGarch)
library (forecast)
library (qrmtools)
library(TSA)
library(xts)
library(tseries)
library(fpp2)
#from https://fred.stlouisfed.org/series/CPILFESL
cpi_data <- read.csv("DataSet/CPIUSSA.csv", header=TRUE) #class dataframe
colnames(cpi_data) <- c('date','cpi') #rename
cpi_data$date <- as.Date(cpi_data$date) #Set date as date time format
#rownames(cpi_data) <- cpi_data$date #Set date as index

cpi_data <- subset(cpi_data, date >= as.Date("2000-01-01")) #remove data before 1980

cpi_train <- subset(cpi_data, date <= as.Date("2019-05-01")) #for training the model

cpi_test <- subset(cpi_data, date > as.Date("2019-05-01")) #for testing the 

cpi_test2 <- subset(cpi_test, date <= as.Date("2019-10-01"))


cpi.test <- ts(cpi_test2$cpi, frequency = 12, start=c(2019,6))
cpi.test

cpi_ts <- ts(cpi_train$cpi, frequency = 12, start=c(2000,1))

ggtsdisplay(cpi_ts, lag.max = 50, main='Orignal')

seaonal_effect <- decompose(cpi_ts)
plot(seaonal_effect)

ggseasonplot(cpi_ts) + ggtitle("Seasonal Plot: Change in CPI") +ylab('CPI')

# d = 1
cpi_ts.rdiff = diff(cpi_ts, lag = 1, differences = 1)
ggtsdisplay(cpi_ts.rdiff, lag.max = 50, main='∇(1)')
adf.test(cpi_ts.rdiff)
cpi_ts.rdiff

# d = 2
cpi_ts.r2diff = diff(cpi_ts, lag = 1, differences = 2)
ggtsdisplay(cpi_ts.sdiff, lag.max = 50, main='∇(2)')
adf.test(cpi_ts.r2diff)

# d = 1 sd = 1
cpi_ts.rdiff.sdiff = diff(diff(cpi_ts, lag = 12, differences = 1))
ggtsdisplay(cpi_ts.rdiff.sdiff, lag.max = 50, main='∇(1) ∇(s = 12)')
adf.test(cpi_ts.rdiff.sdiff)

model_auto <-auto.arima(cpi_ts)
model_auto
ggtsdisplay(model_auto$residuals, lag.max = 50, main='Auto model residual')

#base model
model_fit = Arima(cpi_ts, order = c(1,1,1), seasonal = c(0,0,0), method = 'ML')

autoplot(model_fit)
coeftest(model_fit)
model_fit

ggtsdisplay(model_fit$residuals, lag.max = 50, main='SARIMA(1,1,1)x(0,0,0)12 Residuals')

ggtsdisplay((model_fit$residuals)**2, lag.max = 30, main='Squared residue')

#H0: no correlation for the error terms reject Ho if a < 0.05
Box.test(model_fit$residuals, lag = 50, type = c("Ljung-Box"), fitdf = 2)

acf(model_fit$residuals, lag= 20, main='Residuals Sample ACF')

tsdiag(model_fit,gof.lag = 20)

#normality
resid <- as.numeric(residuals(model_fit))
write.csv(resid, "resid1.csv")
resid <- as.vector(model_fit$residuals)
shapiro.test(resid) #H0: the variable is normally distributed
qqnorm(model_fit$residuals); qqline(resid)
hist(resid, probability = T)
kurtosis(model_fit$residuals)
skewness(model_fit$residuals)
#https://www.rdocumentation.org/packages/TSA/versions/1.3/topics/McLeod.Li.test
#The null hypothesis: No (ARCH) among the lags considered. if p ≤ α=0.05, reject H0
McLeod.Li.test(model_fit)
McLeod.Li.test
testing <- garch_fit_mod@residuals


garch_fit_mod <- garchFit(~ 1 + garch(1,1), data=as.numeric(residuals(model_fit)), 
                                  cond.dist="std", trace=FALSE)
summary(garch_fit_mod)
plot(garch_fit_mod, which = 13)
library(ggplot2)
p1 <- predict(garch_fit_mod, n.ahead=5, plot = T)
print(p1)
plot(volatility(garch_fit_mod))

