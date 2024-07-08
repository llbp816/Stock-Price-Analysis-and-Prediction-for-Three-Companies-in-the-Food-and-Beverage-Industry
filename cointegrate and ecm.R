library(readxl)
data_daily <- read_excel("D:/data-daily.xlsx")
View(data_daily)

scd <- data_daily$SCD
can <- data_daily$CAN
vcf <- data_daily$VCF


library(forecast)
library(urca)

## johansen test using 'trace' criteria

summary(ca.jo(data.frame(scd, can, vcf), type = 'trace'))

# when r <= 1 not reject Ho --> 1 quan he

## johansen test using 'eigen value' criteria

summary(ca.jo(data.frame(scd, can, vcf), type = 'eigen'))

# when r <= 1 not reject Ho --> 1quan he

## check unit root for non stationary series:

summary(ur.df(diff(scd), type = 'none'))
summary(ur.df(diff(can), type = 'none'))
summary(ur.df(diff(vcf), type = 'none'))

# all series are no root series when diff

## test for cointegration 
# estimate model
summary(lm(can ~ scd + vcf))

# stationary residual test

resid.can <- resid(lm(can ~ scd + vcf))
summary(ur.df(resid.can, type = 'none'))

# reject Ho --> stationary

### error correction model:
resid1 <- resid(lm(can ~ scd + vcf))
summary(lm(diff(can) ~ diff(scd) + diff(vcf) + resid1[1:498]))

# after each period, can adjust 0.03717
