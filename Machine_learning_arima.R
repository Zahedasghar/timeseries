
## https://www.geeksforgeeks.org/machine-learning-for-time-series-data-in-r/



# Load the "AirPassengers" dataset
data("AirPassengers")

# Convert the dataset to a time series object
ts_data <- ts(AirPassengers, frequency = 12, start = c(1949, 1))

# Split the data into training and testing sets


train_data <- window(ts_data, start = c(1949, 1), end = c(1958, 12))



test_data <- window(ts_data, start = c(1959, 1))

# Train an ARIMA model
arima_model <- forecast::auto.arima(train_data)
# Forecast 12 months ahead
forecast_result <- forecast::forecast(arima_model, h = 12) 

summary(arima_model)

# Plot the forecasted values

plot(forecast_result, main = "Forecasted AirPassengers", xlab = "Year", ylab = "Passengers")
lines(test_data, col = "red")
legend("topleft", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)


# Load the package
library(forecast)
# Load the dataset

data(lynx)  ## Cats data 

# Convert data to a time series object
ts_data <- ts(lynx, frequency = 1, start = c(1821))

# Fit an ARIMA model to the data
arima_model <- auto.arima(ts_data)

# Split data into training and testing sets
train_data <- window(ts_data, start = c(1821), end = c(1900))

test_data <- window(ts_data, start = c(1901))

# Fit ARIMA model on training data
arima_model <- arima(train_data, order = arima_model$arma[c(1, 6, 2)])



arima_model$residuals

## Your question is to find residuals for test data. 
## Actually this is not the right question

#arima_model on test data and then we use forecast and then we compare the forecasted values with the 
##actual values of the test data

## difference forecast values and test data values is forecast error, so I dont know that anything like residuals 

## on test data is needed. 


# Forecast using the ARIMA model
forecast_values <- forecast(arima_model, h = length(test_data))



# Calculate RMSE to evaluate model performance
rmse <- sqrt(mean((forecast_values$mean - test_data)^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

# Plot the forecasted values
plot(forecast_values, main = "Annual Lynx Trappings Forecast")
lines(test_data, col = "red")



# Load the dataset   "نهر النيل"  Naher Al-Nil
data(Nile)

# Convert data to a time series object
ts_data <- ts(Nile, frequency = 1, start = c(1871))

# Fit an ARIMA model to the data
arima_model <- auto.arima(ts_data)

# Split data into training and testing sets
train_data <- window(ts_data, start = c(1871), end = c(1950))
test_data <- window(ts_data, start = c(1951))

# Fit ARIMA model on training data
arima_model <- arima(train_data, order = arima_model$arma[c(1, 6, 2)])

# Forecast using the ARIMA model
forecast_values <- forecast(arima_model, h = length(test_data))

# Calculate RMSE to evaluate model performance
rmse <- sqrt(mean((forecast_values$mean - test_data)^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

# Plot the forecasted values
plot(forecast_values, main = "Annual Nile River Flow Forecast")
lines(test_data, col = "blue")




# Load the necessary libraries
library(readxl)
library(dplyr)

# Load the data
sa <- read_excel("data12.xlsx")

names(sa)

# Only keep the Date and Close columns
sa <- sa %>% select(Date, Close)   ## Pipe operator

names(sa)

## Inspect your data


sa |> head() 

# Ensure Date is in Date format
sa$Date <- as.Date(sa$Date)


dim(sa)  

# Split the data into training (first 230 rows) and test sets (remaining rows)
train_data <- sa[1:200, ] 

dim(train_data)


test_data <- sa[201:nrow(sa), ]

test_data |> head()



# Check the lengths of the training and test sets
length(train_ts)  # Should be 230
length(test_ts)  # Should be the remaining days

# Fit an ARIMA model to the training data

arima_model <- auto.arima(train_ts)

# Forecast the next 18 days 

forecast_values <- forecast(arima_model, h = length(test_ts))

# Plot the forecasted values  

plot(forecast_values, main = "Forecasted Stock Prices", xlab = "Date", ylab = "Close Price")



# Ensure the forecast and test data are aligned
forecast_ts <- ts(forecast_values$mean, frequency = 365, start = start(test_ts))

# Calculate RMSE
rmse <- sqrt(mean((forecast_ts - test_ts)^2))
print(rmse)





