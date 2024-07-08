library(readxl)
project_mid_CAN <- read_excel("C:/Users/linhh/OneDrive/Desktop/Time series/project mid CAN/project_mid_CAN.xlsx", 
                              sheet = "Daily data")
View(project_mid_CAN)

#forecastproccess
attach(project_mid_CAN)
cpi <- project_mid_CAN$price

can <- project_mid_CAN$price
plot.ts(can)


# Create a histogram
hist(can, freq = FALSE, main = "Histogram and density")

# Calculate density
dcan <- density(can)

# Add density
lines(dcan, lwd = 2, col = "red")

plot.ts(diff(can))


# graph
plot.ts(cpi)
plot.ts(diff(cpi))

time <-seq_along(can)
can.detrend <- resid(lm(can~time))
plot.ts(can.detrend)

#Unit root test
library(urca)
summary(ur.df(cpi, type = 'trend'))

#unit root test for differnce series
summary(ur.df(diff(cpi), type = 'drift'))

m <- ar(diff(cpi), method = c('mle'))
m$aic
##ACF & PACF to determine for ARMA of Difference series

acf(diff(can))
pacf(diff(can))

## Estimate ARIMA model
library(forecast)
reg.cpi.arima113 <- Arima(cpi, order = c(12,1,12), include.constant = TRUE)
summary(reg.cpi.arima113)		


reg.cpi.arima112 <- Arima(cpi, order = c(12,1,1), include.constant = TRUE)
summary(reg.cpi.arima112)

# Post hoc test

autoplot(reg.cpi.arima112)

# test for white noise residual

checkresiduals(reg.cpi.arima112)


## forecast
cpif.arima112<-forecast(reg.cpi.arima112,h=10)
cpif.arima112


autoplot(cpif.arima112)

x<-as.data.frame(cpif.arima112)
x
f<-x$`Point Forecast`
fd <- diff(f)
# rmse, mape
library(Metrics)
rmse(f, fitted(cpif.arima112))
mape(f, fitted(cpif.arima112))

abs