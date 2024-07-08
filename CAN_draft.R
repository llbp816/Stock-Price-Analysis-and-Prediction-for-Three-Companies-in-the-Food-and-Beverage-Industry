### Import data

## Save data
#save(quartly, file = "quarterly.rda")
attach(project_mid_CAN)
View(project_mid_CAN)

## Time series formatting

ts(project_mid_CAN, start = c(2009, 1), frequency = 4)
sale <- ts(project_mid_CAN$"sale", start = c(2009, 1), frequency = 4)
# line chart
plot.ts(sale)

### Time trend & seasonality

## Time trend

# Create time trend

time <- seq_along(sale)
summary(time)

## Linear-linear

#a

plot(time, sale)
abline(lm(sale~time))
reg412 <- lm(sale~time)
summary(reg412)

#b

library(Metrics)

rmse(sale[1:55], fitted(reg412))
mape(sale[1:55], fitted(reg412))


#c

salef1 <- ts(fitted(reg412), start = c(2009, 1), frequency = 4)
plot.ts(sale)
lines(salef1, col = 'red')

## Linear log 

reg413 <- lm(sale~log(time))
summary(reg413)
rmse(sale[1:55], fitted(reg413))
mape(sale[1:55], fitted(reg413))
salef2 <- ts(fitted(reg413), start = c(2009, 1), frequency = 4)
plot.ts(sale)
lines(salef2, col = "blue")

## Log-linear

reg414 <- lm(log(sale) ~ time)
summary(reg414)
rmse(sale[1:55], exp(fitted(reg414)))
mape(sale[1:55], exp(fitted(reg414)))
salef3 <- ts(exp(fitted(reg414)), start = c(2009, 1), frequency = 4)
plot.ts(sale)
lines(salef3, col = "purple")

## Log-Log

reg415 <- lm(log(sale)~log(time))
summary(reg415)
rmse(sale[1:55], exp(fitted(reg415)))
mape(sale[1:55],exp(fitted(reg415)))
salef4 <- ts(exp(fitted(reg415)), start = c(2009, 1), frequency = 4)
plot.ts(sale)
lines(salef4, col = "orange")
# so sanh cac mo hinh

salef3 <- ts(exp(fitted(reg414)), start = c(2009, 1), frequency = 4)
salef4 <- ts(exp(fitted(reg415)), start = c(2009, 1), frequency = 4)

## Seasonal Dummies
# create quarterly dummies
library(tsutils)
s1 <- c(rep(c(1,0,0,0), 14))
s2 <- c(rep(c(0,1,0,0), 14))  
s3 <- c(rep(c(0,0,1,0), 14))
s4 <- c(rep(c(0,0,0,1), 14))

library(tsutils)
seas <- seasdummy(56, 4)

# Regression with seasonal dummies

reg42b <- lm(sale ~ s2+s3+s4)
summary(reg42b)

# comparison with auto created dummies

summary(lm(sale ~ seas))

# trend + seasonal with linear

# addictive form
# s1 is base
summary(lm(sale ~ time + s2+s3+s4))

#s4 is base
summary(lm(sale ~ time + seas))


# mape

mape(sale[1:55], fitted(lm(sale ~ time + s2+s3+s4)))
mape(sale[53:55], fitted(lm(sale ~ time + s2+s3+s4)))

#multiplicative form

summary(lm(sale ~ time + time*s2 + time*s3 + time*s4))

# mape

mape(sale[1:55], fitted(lm(sale ~ time + time*s2 + time*s3 + time*s4)))
mape(sale[53:55], fitted(lm(sale ~ time + time*s2 + time*s3 + time*s4)))
# non - linear trend + seasonality

# additive form
summary(lm(log(sale) ~ time + s2 + s3 + s4))          

# multiplicative form
summary(lm(log(sale) ~ time + time*s2 + time*s3 + time*s4))


## Holt - winter with seasonality (additive form)
sale <- project_mid_CAN$`sale`[1:55]
#sale <- ts(quartly$`gross sale`, start = c(2009, 1), frequency = 4)
sale <- ts(sale, start = c(2009, 1), frequency = 4)
hw.sale.a <-HoltWinters(sale, seasonal = "a")
hw.sale.a

plot.ts(sale)
lines(fitted(hw.sale.a)[,1], col = 'red')

mape(sale[1:55], fitted(hw.sale.a))
mape(sale[52:55],fitted(hw.sale.a))


## Holt_Winters with seasonality, Multiplicative form

hw.sale.m <- HoltWinters(sale, seasonal = "m")
hw.sale.m

plot.ts(sale)
lines(fitted(hw.sale.m)[,1], col = 'blue')

mape(sale[1:55], fitted(hw.sale.m))
mape(sale[52:55],fitted(hw.sale.m))