# clear the memory space

rm(list=ls())

#read the data file
strikes0<- read.csv(file.choose(), header=T)
str(strikes0)

library(psych)
describe(strikes0$Strikes)

#define the time series settings
strikes<- ts(strikes0$Strikes, frequency=1, start=1947)
str(strikes)

# plot the time series plot
plot.ts(strikes, ylab="Number of strikes during 1947 - 2020")

# log of strikes and its plot
logstrikes<- log(strikes)
plot.ts(logstrikes, ylab="Logarithm of strikes during 1947 - 2020")


library(forecast)

# differenccing the time series data and checking for stationarity
logstrikes_diff<- diff(logstrikes, differences=1)
plot.ts(logstrikes_diff)
plot.ts(logstrikes_diff, ylab="First differenced log strikes during 1947 - 2020")

# ACF and PACF of the first differenced logarithm of strikes
acf(logstrikes_diff, main="", ylab="Auto correlation of first differenced log strikes")
pacf(logstrikes_diff, main="", ylab="Partial auto correlation of first differenced log strikes")

# fitting the intial ARIMA candidate model - ARIMA (2,1,2)
arima0<- arima(logstrikes, order=c(2, 1, 2))
summary(arima0)

# fitting and checking alternate candidate models
arima1<- arima(logstrikes, order=c(1, 1, 1))
summary(arima1)

arima2<- arima(logstrikes, order=c(2, 1, 1))
summary(arima2)

arima3<- arima(logstrikes, order=c(1, 1, 2))
summary(arima3)

model=c("arima212", "arima111", "arima211", "arima112")
aic=c(arima0$aic, arima1$aic, arima2$aic, arima3$aic)
modelcompare<- cbind(model, aic)
modelcompare

arima2_forecast<- forecast(arima2, h=2, level=95)
plot(arima2_forecast, ylab="Forecast of the log strikes for 2021 and 2022", main="")
plot.ts(arima2$residuals, ylab="Residuals of ARIMA(2, 1, 1) model")


Box.test(arima2$residuals, lag=12, type="Ljung-Box")

hist(arima2$residuals, xlab=" Residuals of ARIMA(2, 1, 1)", col="gold", main="")

plot(exp(arima2_forecast$x), ylab="Number of strikes", col="blue")
lines(exp(arima2_forecast$fitted), col="red")
legend("topright", legend=c("Observed", "Forecast"), col=c("blue", "red"), lty=1, cex=0.8)
 
