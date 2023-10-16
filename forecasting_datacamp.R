library(fpp2)
library(fpp3)
# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Find the outlier in the gold series
goldoutlier <- which.max(gold)
goldoutlier
# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)


# Load the fpp2 package
library(fpp2)

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)

# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer,start= 1992)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)


# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil,lag=1:9)

# Create an ACF plot of the oil data
ggAcf(oil)

pacf(oil)

auto.arima(oil)
# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- lag(1)

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- lag(7)


# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")


# Use naive() to forecast the goog series
fcgoog <- naive(goog,h=20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer,h=16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
fcbeer<-ausbeer %>% snaive()%>% checkresiduals

# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE


# install.packages("remotes")
#remotes::install_github("robjhyndman/forecast")
library(forecast)

# Create the training data as train
train <- subset(gold, end = 1000)

# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 108)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
#accuracy(naive_fc, gold)
#accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc

## # Create three training series omitting the last 1, 2, and 3 years
## train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
## train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
## train3 <- window(vn[, "Melbourne"], end = c(2012, 4))
## 
## # Produce forecasts using snaive()
## fc1 <- snaive(train1, h = 4)
## fc2 <- snaive(train2, h = 4)
## fc3 <- snaive(train3, h = 4)
## 
## # Use accuracy() to compare the MAPE of each series
## #accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
## #accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
## accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]
## 

# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(goog, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


# Create a training set using subset()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy( fcses,marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa,h=10)

# Look at fitted model using summary()
summary(fcholt)
# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)


## # Plot the data
## ___
## 
## # Produce 3 year forecasts
## fc <- hw(___, seasonal = ___, h = ___)
## 
## # Check if residuals look like white noise
## ___
## whitenoise <- ___
## 
## # Plot forecasts
## ___
## 

# Plot the data
autoplot(a10)
# Produce 3 year forecasts
fc <- hw(a10, seasonal ="multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)


# Create training data with subset()
#train <- subset(___, end = ___)

# Holt-Winters additive forecasts as fchw
#fchw <- hw(___, seasonal = ___, h = ___)

# Seasonal naive forecasts as fcsn
#fcsn <- ___

# Find better forecasts with accuracy()
#accuracy(___, ___)
#accuracy(___, ___)

# Plot the better forecasts
#autoplot(___)


# Create training data with subset()
train <- subset(hyndsight, end = length(hyndsight) - 28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)
