#https://towardsdatascience.com/a-deep-dive-on-vector-autoregression-in-r-58767ebb3f06

# Load pacman
library(pacman)

# Use p_load to install (if necessary) and load the required packages
p_load(vars, mFilter, tseries, TSstudio, forecast, tidyverse, xts)

# Load the dataset

mp <- read_csv("data/SampleVAR.csv") 

head(mp)
tail(mp)

# Read date from chr to date


mp$date <- as.Date(mp$date, format = "%m/%d/%y")

colnames(mp)


colnames(mp) <- c("date","M1","lnIP","RRP", "CPI")

## Convert the dataset to a time series object

ts_data <- ts(mp[,2], frequency = 12, start = c(2003, 1))



# Convert the dataset to xts 

mp_xts <- xts(mp[,-1], order.by = mp$date)

names(mp_xts)

# Plot the time series data mp_xts

autoplot(mp_xts$M1) + theme_minimal() + labs(title = "M1", x = "Year", y = "M1")

autoplot(mp_xts$lnIP) + theme_minimal() + labs(title = "lnIP", x = "Year", y = "lnIP")

autoplot(mp_xts$RRP) + theme_minimal() + labs(title = "RRP", x = "Year", y = "RRP")

autoplot(mp_xts$CPI) + theme_minimal() + labs(title = "CPI", x = "Year", y = "CPI")

## Philips Perron Test

pp.test(mp_xts$M1)

pp.test(mp_xts$CPI)

pp.test(mp_xts$RRP)

pp.test(mp_xts$lnIP)


# Split the data into training and testing sets

train_data <- window(ts_data, start = c(2003, 1), end = c(2018, 12))

test_data <- window(ts_data, start = c(2019, 1))


lagselect <- VARselect(mp_xts, lag.max = 8, type = "const") 


lagselect$selection


Model1 <- VAR(mp_xts, p = 2, type = "const", season = NULL, exog = NULL) 
summary(Model1)


## Model Diagnostic 


Serial1 <- serial.test(Model1, lags.pt = 4, type = "PT.asymptotic")

Serial1


Arch1 <- arch.test(Model1, lags.multi = 12, multivariate.only = TRUE)
Arch1

Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1

Stability1 <- stability(Model1, type = "OLS-CUSUM")

## Plot with fixing margin of plots

plot(Stability1, mar = c(3, 3, 3, 3))

## Granger Causality Test

GrangerRRP<- causality(Model1, cause = "RRP")
GrangerRRP
GrangerM1 <- causality(Model1, cause = "M1")
GrangerM1
GrangerCPI <- causality(Model1, cause = "CPI")
GrangerCPI
GrangerlnIP <- causality(Model1, cause = "lnIP")
GrangerlnIP


## IRF 

RRPirf <- irf(Model1, impulse = "RRP", response = "RRP", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "RRP", main = "RRP's shock to RRP")
M1irf <- irf(Model1, impulse = "RRP", response = "M1", n.ahead = 20, boot = TRUE)
plot(M1irf, ylab = "M1", main = "RRP's shock to M1")
CPIirf <- irf(Model1, impulse = "RRP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "RRP's shock to CPI")
lnIPirf <- irf(Model1, impulse = "RRP", response = "lnIP", n.ahead = 20, boot = TRUE)
plot(lnIPirf, ylab = "lnIP", main = "RRP's shock to lnIP")

FEVD1 <- fevd(Model1, n.ahead = 10)
FEVD1

## Plot by having small plot margins

plot(FEVD1, mar = c(3, 3, 3, 3))
plot(FEVD1)


## Forecasting

forecast <- predict(Model1, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "RRP", main = "Fanchart for RRP", xlab = "Horizon", ylab = "RRP")
fanchart(forecast, names = "M1", main = "Fanchart for M1", xlab = "Horizon", ylab = "M1")
fanchart(forecast, names = "CPI", main = "Fanchart for CPI", xlab = "Horizon", ylab = "CPI")
fanchart(forecast, names = "lnIP", main = "Fanchart for lnIP", xlab = "Horizon", ylab = "lnIP")
forecast

