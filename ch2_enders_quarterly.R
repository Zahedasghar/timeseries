library(tidyverse)
library("zoo")
library(xts)
library(forecast)
library(readxl)
library(tseries)
### PAGE 88
data = read_excel("data/quarterly.xlsx")

data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill
# 
 spread_xts <- xts(data$spread,order.by=data$DATE)
colnames(spread_xts) <- c("spread")
## Make an autoplot and mean line of spread

autoplot(spread_xts)
# 
 autoplot(diff(spread_xts)) 

library(ggplot2)
library(dplyr)

# Assuming 'data' is your dataframe with columns 'DATE' and 'spread'
data <- data %>% mutate(diff_spread = c(NA, diff(spread)))

# Plot 1: The Interest Rate Spread
p1 <- ggplot(data, aes(x = DATE, y = spread)) +
  geom_line(color = "steelblue4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "The Interest Rate Spread", x = "", y = "") +
  theme_minimal()

# Plot 2: First Difference of The Spread
p2 <- ggplot(data %>% filter(!is.na(diff_spread)), aes(x = DATE, y = diff_spread)) +
  geom_line(color = "steelblue4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "First-Difference of The Spread", x = "", y = "") +
  theme_minimal()


p1
p2
# Displaying the plots side-by-side
library(patchwork)
p1 / p2



# Plotting ACF using ggAcf
p3 <- ggAcf(data$spread, lag.max = 12) + 
  labs(title = "ACF of The Spread") +
  theme_minimal()

# Plotting PACF using ggPacf
p4 <- ggPacf(data$spread, lag.max = 12) + 
  labs(title = "PACF of The Spread") +
  theme_minimal()

# Displaying the plots side-by-side
p3 / p4

### PAGE 91

# Fit AR model on xts
library(xts)
library(lmtest)
library(modelsummary)
library(dynlm)
# Assuming xts object is named 'my_xts'
ar7 <- Arima(data$spread,order = c(7,0,0)) 
ar6 <- Arima(data$spread,order=c(6,0,0))
ar2 <- Arima(data$spread,order=c(2,0,0))

# Fit ARIMA with 1, 2, and 7 AR lags
# Create an order object with multiple AR lags

# Fit ARIMA model with partial AR lags
ar127 <- arima(data$spread, 
                     xreg = data.frame(ar1 = lag(data$spread, 1), ar2 = lag(data$spread, 2), ar7 = lag(data$spread, 7)))

## Have msummary with asterics

msummary(ar127, stars = TRUE, gof_map = "AIC|BIC", coef_map = c("ar1" = "AR(1)", "ar2" = "AR(2)", "ar7" = "AR(7)"))

arma11 <- Arima(data$spread,order=c(1,0,1))


arma21 <- Arima(data$spread,order=c(2,0,1))

# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Assuming your time series data is in the vector 'spread_data'
# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Fit ARIMA model with AR(1), MA(1), and MA(7)
Arima(data$spread, order = c(1, 0, 1))


# Fit ARIMA model with AR(1), MA(1), and MA(7)


# Fitting multiple ARIMA models
ar7 <- Arima(data$spread, order = c(1, 0, 7))
ar6 <- Arima(data$spread, order = c(1, 0, 6))
ar2 <- Arima(data$spread, order = c(1, 0, 2))
ar127 <- Arima(data$spread, order = c(1, 0, 0), 
               seasonal = list(order = c(0, 0, 0), period = NA),
               xreg = cbind(lag(data$spread, 1), lag(data$spread, 7)))
arma11 <- Arima(data$spread, order = c(1, 0, 1))
arma21 <- Arima(data$spread, order = c(2, 0, 1))
arima1_17 <- Arima(data$spread, order = c(1, 0, 7))

# Store models in a named list
models <- list("AR(7)" = ar7,
               "AR(6)" = ar6,
               "AR(2)" = ar2,
               "AR(1,7)" = ar127,
               "ARMA(1,1)" = arma11,
               "ARMA(2,1)" = arma21,
               "ARIMA(1,0,7)" = arima1_17)

# Create a summary table
summary_table <- modelsummary(
  models,
  stars = TRUE,  # Display stars for significance
  gof_omit = "AIC|BIC",  # Omit AIC and BIC using a regex pattern
  coef_map = c("ar1" = "AR(1)", 
               "ma1" = "MA(1)", 
               "ma7" = "MA(7)")
)

summary_table


# Create a list to store the ARIMA models
arima_models <- models

# Fit 7 ARIMA models and store them in the list
for (i in 1:7) {
  arima_models[[i]] <- Arima(data$spread, order = c(i, 0, 0))
}

# Perform Ljung-Box test for each model at lags 4, 8, and 12
lags <- c(4, 8, 12)
for (lag in lags) {
  cat("Ljung-Box test at lag", lag, ":\n")
  for (i in 1:7) {
    cat("Model", i, ":")
    print(Box.test(arima_models[[i]]$residuals, lag = lag, type = "Ljung-Box"))
  }
}
library(broom)

# Create a list to store the test results
test_results <- list()

# Perform Ljung-Box test for each model at lags 4, 8, and 12
lags <- c(4, 8, 12)
for (lag in lags) {
  for (i in 1:7) {
    test <- tidy(Box.test(arima_models[[i]]$residuals, lag = lag, type = "Ljung-Box"))
    test$model <- paste("Model", i)
    test$lag <- lag
    test_results[[length(test_results) + 1]] <- test
  }
}

# Combine the test results into a data frame
test_df <- do.call(rbind, test_results)

# Print the test results
test_df 
as_tibble(test_df) |> select(lag, everything())

# Print the summary table
print(summary_table)


library(broom)



# Print the model summary
modelsummary(model_summary)
# Print the summary of the model
library(tseries)
arma(data$spread, lag=list(ar=1,ma=c(1,7))) 
### TABLE 2.4
# Install and load the fracdiff package
#install.packages("fracdiff")
library(fracdiff)
#install.packages("rugarch")
library(rugarch)

# Now you should be able to use the arfimaspec function
spec.ar7 = arfimaspec(mean.model = list(armaOrder = c(7, 0), include.mean = TRUE))

spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE))
fit.ar7 = arfimafit(spec=spec.ar7,data=data$spread)
fit.ar7
res.ar7 = fit.ar7@fit$residuals
Box.test(res.ar7,lag=4,type="Ljung-Box")
Box.test(res.ar7,lag=8,type="Ljung-Box")
Box.test(res.ar7,lag=12,type="Ljung-Box")


spec.ar6 = arfimaspec(mean.model=list(armaOrder=c(6,0),include.mean=TRUE))
fit.ar6 = arfimafit(spec=spec.ar6,data=data$spread)
fit.ar6
res.ar6 = fit.ar6@fit$residuals
Box.test(res.ar6,lag=4,type="Ljung-Box")
Box.test(res.ar6,lag=8,type="Ljung-Box")
Box.test(res.ar6,lag=12,type="Ljung-Box")


spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
fit.ar2 = arfimafit(spec=spec.ar2,data=data$spread)
fit.ar2
res.ar2 = fit.ar2@fit$residuals
Box.test(res.ar2,lag=4,type="Ljung-Box")
Box.test(res.ar2,lag=8,type="Ljung-Box")
Box.test(res.ar2,lag=12,type="Ljung-Box")


spec.ar27 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE),
                       fixed.pars=list(ar3=0,ar4=0,ar5=0,ar6=0))
fit.ar27 = arfimafit(spec=spec.ar27,data=data$spread)
fit.ar27
res.ar27 = fit.ar27@fit$residuals
Box.test(res.ar27,lag=4,type="Ljung-Box")
Box.test(res.ar27,lag=8,type="Ljung-Box")
Box.test(res.ar27,lag=12,type="Ljung-Box")


spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE))
fit.arma11 = arfimafit(spec=spec.arma11,data=data$spread)
fit.arma11
res.arma11 = fit.arma11@fit$residuals
Box.test(res.arma11,lag=4,type="Ljung-Box")
Box.test(res.arma11,lag=8,type="Ljung-Box")
Box.test(res.arma11,lag=12,type="Ljung-Box")


spec.arma21 = arfimaspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE))
fit.arma21 = arfimafit(spec=spec.arma21,data=data$spread)
fit.arma21
res.arma21 = fit.arma21@fit$residuals
Box.test(res.arma21,lag=4,type="Ljung-Box")
Box.test(res.arma21,lag=8,type="Ljung-Box")
Box.test(res.arma21,lag=12,type="Ljung-Box")


# Define your ARFIMA model specification
spec.arma27 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 7), include.mean = TRUE, arfima = TRUE),
  distribution.model = "norm"
)

# Fit the model
fit.arma27 <- tryCatch(
  ugarchfit(spec = spec.arma27, data = data$spread),
  error = function(e) e
)

# Check if the model converged properly
if (inherits(fit.arma27, "try-error") || fit.arma27@fit$convergence != 0) {
  print("Model did not converge properly. Try simplifying your model.")
} else {
  # Extract residuals only if the model converged
  res.arma27 <- fit.arma27@fit$residuals
  
  # Perform Ljung-Box test on residuals
  print(Box.test(res.arma27, lag = 4, type = "Ljung-Box"))
  print(Box.test(res.arma27, lag = 8, type = "Ljung-Box"))
  print(Box.test(res.arma27, lag = 12, type = "Ljung-Box"))
}

### PAGE 94
data$DATE[163]
data$spread[163] # Typo in the book. Instead of 0.04 it is written 0.4

which(data$DATE=="2000 Q2")
data$DATE[162]
actual = data$spread[-c(1:162)]

### 1-STEP AHEAD ROLLING WINDOW FORECAST
fore.arma27 = fore.ar7 = NULL
for (i in 1:50){
  fit.ar7=arfimafit(spec=spec.ar7,data=data$spread[1:(162+i-1)],solver="nlminb")
  fore.ar7[i]=arfimaforecast(fit.ar7,n.ahead=1)@forecast$seriesFor
  fit.arma27=arfimafit(spec=spec.arma27,data=data$spread[1:(162+i-1)],solver="gosolnp")
  fore.arma27[i]=arfimaforecast(fit.arma27,n.ahead=1)@forecast$seriesFor
}
mean(fore.ar7)
mean(fore.arma27)
var(fore.ar7)
var(fore.arma27)

### FORECAST ERROR and FORECAST ERROR VARIANCE
fore.error.ar7 = fore.ar7-actual
fore.error.arma27 = fore.arma27-actual
var(fore.error.ar7)
var(fore.error.arma27)

### PAGE 95
summary(lm(actual~fore.ar7))
summary(lm(actual~fore.arma27))


### GRANGER-NEWBOLD TEST
x = fore.error.ar7+fore.error.arma27
z = fore.error.ar7-fore.error.arma27
corxz = cor(z,x)
corxz/( sqrt( (1-corxz^2)/(length(fore.error.ar7)-1)))

### DIEBOLD-MARIANO TEST
dm.test(fore.error.ar7,fore.error.arma27, h=1, power=4)

d = (fore.error.ar7)^4-(fore.error.arma27)^4
DM = mean(d)/(var(d)/(length(d)-1))^0.5
DM
acf.d = acf(d)
acf.d


### SEASONALITY
### PAGE 98
library(forecast)

par(mfrow=c(2,1))

# Plotting M1NSA
plot(data$DATE, data$M1NSA, type="l", las=1, xaxs="i", yaxs="i", xlab="", ylab="", 
     main="M1NSA Over Time", tck=0.02, col="steelblue4", ylim=c(0, 2500))

# Plotting Growth Rate of M1NSA
mg <- 100 * diff(log(data$M1NSA))
plot(data$DATE[-1], mg, type="l", las=1, xaxs="i", yaxs="i", xlab="", ylab="", 
     main="Growth Rate of M1NSA", tck=0.02, col="steelblue4", ylim=c(-4, 8))
abline(h=0)

# ACF and PACF Analysis (PANEL A)
par(mfrow=c(2,1))
acf(mg, lag.max=25, main="ACF of Growth Rate of M1NSA")
pacf(mg, lag.max=25, main="PACF of Growth Rate of M1NSA")

# Seasonal Differencing
par(mfrow=c(1,1))
smg <- diff(diff(log(data$M1NSA), 4))  # Seasonal differencing
plot(smg, type="l", las=1, xaxs="i", yaxs="i", xlab="", ylab="", 
     main="Seasonally Differenced M1NSA", tck=0.02, col="steelblue4")
abline(h=0)

# ACF and PACF Analysis (PANEL B)
par(mfrow=c(2,1))
acf(smg, lag.max=25, main="ACF of Seasonally Differenced M1NSA")
pacf(smg, lag.max=25, main="PACF of Seasonally Differenced M1NSA")

# Fit ARIMA model (Equivalent to ARMA(4,4) with some terms set to zero)
fit.arma14 <- Arima(smg, order = c(4, 0, 4), fixed = c(NA, 0, 0, NA, 0, 0, 0, NA))
summary(fit.arma14)

# Extract residuals
res.arma14 <- residuals(fit.arma14)

# Perform Ljung-Box tests
Box.test(res.arma14, lag = 4, type = "Ljung-Box")
Box.test(res.arma14, lag = 8, type = "Ljung-Box")
Box.test(res.arma14, lag = 12, type = "Ljung-Box")


### PARAMETER INSTABILITY
data = read_excel("data/QUARTERLY.xls")
library("zoo")
data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

par(mfrow=c(2,1))
plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.pre = arfimafit(spec=spec.arma27,data=data$spread[1:88])
fit.arma27.pre
fit.arma27.post = arfimafit(spec=spec.arma27,data=data$spread[89:193],solver="gosolnp")
fit.arma27.post
sum(fit.arma27.pre@fit$residuals^2)
sum(fit.arma27.post@fit$residuals^2)

data$Indicator = 0
data$Indicator[which(data$DATE=="1982 Q1"):nrow(data)]=1
data$Indicator

spec.arma27.ex = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE,external.regressors=matrix(data$Indicator)),
                            fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.ex = arfimafit(spec=spec.arma27.ex,data=data$spread)
fit.arma27.ex


### ENDOGENOUS BREAKS
### PAGE 104


# Load the data
Break <- read_excel("data/y_break.xlsx")
br <- Break %>% select(-1)  # Remove the first column if it is not part of the data
colnames(br) <- c("Value")
br$Index <- 1:nrow(br)

# Plot the data using ggplot2
ggplot(br, aes(x = Index, y = Value)) +
  geom_line(color = "steelblue4") +
  labs(title = "Break Data Plot", x = "Index", y = "Value") +
  theme_minimal()

# Convert the data to a numeric vector
br_numeric <- as.numeric(unlist(br$Value))

# Fit an AR(1) model using forecast::Arima()
fit.ar1 <- Arima(br_numeric, order = c(1, 0, 0))
summary(fit.ar1)

# Extract coefficients
intercept <- coef(fit.ar1)["intercept"]
ar1_coef <- coef(fit.ar1)["ar1"]
unconditional_mean <- intercept / (1 - ar1_coef)

cat("Intercept:", intercept, "\n")
cat("AR(1) Coefficient:", ar1_coef, "\n")
cat("Unconditional Mean:", unconditional_mean, "\n")

# Forecasting next 10 periods
forecast_ar1 <- forecast(fit.ar1, h = 10)

# Plotting forecast with original data using ggplot2
forecast_df <- data.frame(
  Index = (length(br_numeric) + 1):(length(br_numeric) + 10),
  Forecast = as.numeric(forecast_ar1$mean)
)

ggplot() +
  geom_line(data = br, aes(x = Index, y = Value), color = "steelblue4") +
  geom_line(data = forecast_df, aes(x = Index, y = Forecast), color = "red", linetype = "dashed") +
  labs(title = "Original Data and Forecast", x = "Index", y = "Value") +
  theme_minimal()

# AR(1) Model Residual Analysis and CUSUM Calculation
forecasts <- NULL
for (i in 1:(length(br_numeric)-2)){
  model <- Arima(br_numeric[1:(2+i-1)], order = c(1, 0, 0))
  forecasts[i] <- forecast(model, h = 1)$mean
}
res <- br_numeric[-c(1:2)] - forecasts

# CUSUM Calculation
CUMSUM <- cumsum(res / sd(res))

ggplot(data.frame(Index = 1:length(CUMSUM), CUMSUM = CUMSUM), aes(x = Index, y = CUMSUM)) +
  geom_line(color = "steelblue4") +
  labs(title = "CUSUM Test", x = "Index", y = "CUSUM") +
  theme_minimal()

# Calculate Forecast Errors and Variances
fore_error <- res
forecast_error_variance <- var(fore_error)

cat("Forecast Error Variance:", forecast_error_variance, "\n")

# Calculate Optimal Weighted Forecast (Assuming you have different models to compare)
# Example: Using two different AR models
fit.ar2 <- Arima(br_numeric, order = c(2, 0, 0))
fit.ar3 <- Arima(br_numeric, order = c(3, 0, 0))

forecasts_combined <- cbind(
  forecast(fit.ar1, h = 10)$mean,
  forecast(fit.ar2, h = 10)$mean,
  forecast(fit.ar3, h = 10)$mean
)

# Equally Weighted Forecast
equally_weighted_forecast <- rowMeans(forecasts_combined)

# Optimal Weights Calculation
forecast_errors <- apply(forecasts_combined, 2, function(f) br_numeric[(length(br_numeric)-9):length(br_numeric)] - f)
forecast_variances <- apply(forecast_errors, 2, var)
optimal_weights <- 1 / forecast_variances
optimal_weights <- optimal_weights / sum(optimal_weights)

# Optimal Weighted Forecast
optimal_forecast <- forecasts_combined %*% optimal_weights

# Plotting Combined Forecasts
forecast_combined_df <- data.frame(
  Index = (length(br_numeric) + 1):(length(br_numeric) + 10),
  EquallyWeighted = equally_weighted_forecast,
  OptimalWeighted = as.vector(optimal_forecast)
)

ggplot() +
  geom_line(data = br, aes(x = Index, y = Value), color = "steelblue4") +
  geom_line(data = forecast_combined_df, aes(x = Index, y = EquallyWeighted), color = "orange", linetype = "dashed") +
  geom_line(data = forecast_combined_df, aes(x = Index, y = OptimalWeighted), color = "purple", linetype = "dashed") +
  labs(title = "Combined Forecasts (Equally vs. Optimally Weighted)", x = "Index", y = "Forecasted Value") +
  theme_minimal()
### END
