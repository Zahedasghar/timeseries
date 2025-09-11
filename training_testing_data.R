
## https://www.geeksforgeeks.org/machine-learning-for-time-series-data-in-r/

# Load the package
library(forecast)

## Load the data
data("AirPassengers")

# Inspect the data

head(AirPassengers)
tail(AirPassengers)

# Convert the dataset to a time series object
ts_data <- ts(AirPassengers, frequency = 12, start = c(1949, 1))

# Split the data into training and testing sets
train_data <- window(ts_data, start = c(1949, 1), end = c(1958, 12))
test_data <- window(ts_data, start = c(1959, 1))

# Train an ARIMA model
arima_model <- forecast::auto.arima(train_data)

# Forecast 12 months ahead
forecast_result <- forecast::forecast(arima_model, h = test_data) 

summary(arima_model)

# Plot the forecasted values

plot(forecast_result, main = "Forecasted AirPassengers", xlab = "Year", ylab = "Passengers")
lines(test_data, col = "red")
legend("topleft", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)


# Load the dataset
data(lynx)

# Inspect the data

head(lynx)
tail(lynx)
# Convert data to a time series object
ts_data <- ts(lynx, frequency = 1, start = c(1821))

# Fit an ARIMA model to the data
arima_model <- auto.arima(ts_data)

# Split data into training and testing sets
train_data <- window(ts_data, start = c(1821), end = c(1900))
test_data <- window(ts_data, start = c(1901))

## Identify the best ARIMA model

auto.arima(train_data)

# Fit ARIMA model on training data
arima_model <- arima(train_data, order = arima_model$arma[c(1, 6, 2)])

# Forecast using the ARIMA model
forecast_values <- forecast(arima_model, h = length(test_data))

# Calculate RMSE to evaluate model performance
rmse <- sqrt(mean((forecast_values$mean - test_data)^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

# Plot the forecasted values
plot(forecast_values, main = "Annual Lynx Trappings Forecast")
lines(test_data, col = "red")



# Load the dataset
data(Nile)

# Convert data to a time series object
ts_data <- ts(Nile, frequency = 1, start = c(1871))

# Fit an ARIMA model to the data
arima_model <- auto.arima(ts_data)

# Split data into training and testing sets
train_data <- window(ts_data, start = c(1871), end = c(1950))
test_data <- window(ts_data, start = c(1951))

auto.arima(train_data)
# Fit ARIMA model on training data
arima_model <- arima(train_data, order = arima_model$arma[c(1, 6, 2)])

# Forecast using the ARIMA model
forecast_values <- forecast(arima_model, h = length(test_data))

# Calculate RMSE to evaluate model performance
rmse <- sqrt(mean((forecast_values$mean - test_data)^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

# Plot the forecasted values
plot(forecast_values, main = "Annual Nile River Flow Forecast")
lines(test_data, col = "red")
