library(fpp2)
library(fpp3)
library(tidyverse)
library(forecast)
library(zoo)
library(ggfortify)
library(xts)
## Data source Karandaz Load to SME Value Ration


sme <- read_csv("docs/data/loan_to_sme.csv", skip = 1)


sme |> 
  mutate(date_parsed=parse_date_time(
    Monthly,
    order='mdy'
  ))

sme$date <- as.Date(paste0("01 ", sme$Monthly), format = "%d %b %Y")



sme <- sme |> select(-1)

# # Extract the month, day, and year components in the desired order
# sme$date<- format(sme$date, "%m-%d-%Y")
# sme$date <- as.Date(sme$date)
sme |> arrange(date) ->sme
View(sme)
library(janitor)

#sme$date <- as.Date(format(sme$date, "%Y-%m-01"))

sme <- sme |> clean_names() 



sme_ts <- start_date <- as.Date("2015-12-01")

# Create a monthly time series with December 2015 as the start date
tssme <- ts(sme$pkr_millions, start = c(year(start_date), month(start_date)), frequency = 12)



tssme |> glimpse()
# Create a time series object
sme_ts <- zoo(sme$pkr_millions, sme$date)

frequency(tssme)

# Plot the time series
autoplot(tssme)



# Find the outlier in the gold series
which.max(sme$pkr_millions)


autoplot(tssme)
seasonplot(tssme)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(tssme)
ggseasonplot(tssme, polar = TRUE)
ggsubseriesplot(tssme)




# Create a lag plot of the oil data
gglagplot(tssme,lag=1:3)

# Create an ACF plot of the oil data
ggAcf(tssme)
ggPacf(tssme)


# Plot the differenced series
autoplot(goog)
autoplot(diff(goog))
ggAcf(goog)
ggPacf(goog)
# ACF of the differenced series
ggAcf(diff(tssme))
ggPacf(diff(tssme))
# Ljung-Box test of the differenced series
Box.test(diff(tssme), lag = 6, type = "Ljung")


# Use naive() to forecast the sme series
fcnaive <- naive(tssme,h=6)

# Plot and summarize the forecasts
autoplot(fcnaive)
summary(fcnaive)

# Use snaive() to forecast the sme series
fcsme <- snaive(tssme,h=6)

# Plot and summarize the forecasts
autoplot(fcsme)
summary(fcsme)

# Check the residuals from the naive forecasts applied to the goog series
tssme %>% naive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)


# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
fcsme<-tssme %>% snaive()%>% checkresiduals

# install.packages("remotes")
#remotes::install_github("robjhyndman/forecast")
library(forecast)
tssme |> glimpse()
# Create the training data as train
# Subset the time series object

train <- subset(tssme, end = length(tssme)-20)






# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 20)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 20)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, train)
accuracy(mean_fc, train)

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
e <- tsCV(tssme, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


# Use ses() to forecast the next 10 years of winning times
fc <- ses(tssme, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))



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
