
library(tidyverse)
library(forecast)
 library(zoo)
library(ggfortify)
library(readxl)
library(xts)
library(prophet)
f_data <- read_csv("data/time_series_data.csv")

f_data |> dim()

f_data |>  glimpse()

# Convert data to time series
ts_data <- ts(f_data$RCHRG_SUM, frequency = 7, start = c(2023, 6, 1))
f_data$ds <- as.Date(f_data$RCHRG_DT_KEY,format = "%m/%d/%Y") 

autoplot(ts_data)
f_data <- f_data |> select(ds,RCHRG_SUM)
View(summary(f_data))
plot(RCHRG_SUM~ds , f_data, type = "l")

colnames(f_data) <- c("ds", "y")
m <- prophet(f_data)
future <- make_future_dataframe(m, periods = 30)
forecast <- predict(m, future)
plot(m, forecast)


head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

prophet_plot_components(m, forecast)



ts_data

# Fit a forecasting model (e.g., auto.arima)
model <- auto.arima(ts_data)
model
# Forecast the next 90 days
forecast_result <- forecast(model, h = 90)

# View the forecasted values
print(forecast_result)
plot(forecast_result)












f_data$date <- as.Date(f_data$RCHRG_DT_KEY,format = "%m/%d/%Y")

f_data$day <- weekdays(f_data$date)

f_data |> glimpse()

f_data |> head()

# Reorder the levels of DayOfWeek from Monday to Sunday
f_data$day <- factor(f_data$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Calculate the mean value for each day of the week
daily_means <- f_data %>%
  group_by(day) %>%
  summarize(MeanRCHRG_SUM = mean(RCHRG_SUM, na.rm = TRUE))

daily_means 


# Visualize the daily effects
ggplot(daily_means, aes(x = day, y = MeanRCHRG_SUM)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Mean RCHRG_SUM by Day of the Week",
       x = "Day of the Week", y = "Mean RCHRG_SUM") +
  theme_minimal()

# Calculate the total mean RCHRG_SUM
total_mean <- sum(daily_means$MeanRCHRG_SUM)

# Calculate the weight of each day
daily_means_weight <- transform(daily_means, Weight = MeanRCHRG_SUM / total_mean)

# View the result
daily_means_weight


# Create dummy variables for each day of the week
dummy_vars <- model.matrix(~ day - 1, data = f_data)

# Combine the dummy variables with the original data
df_with_dummies <- cbind(f_data, dummy_vars)

# Fit a Holt forecasting model
model <- holt(ts(df_with_dummies$RCHRG_SUM), h = 90)

# Forecast the next 90 days
forecast_result <- forecast(model)

# Plot the data and forecast
autoplot(forecast_result) + xlab("Date") + ylab("RCHRG_SUM") + ggtitle("Holt Forecasting for Next 90 Days")


df_with_dummies <- df_with_dummies |> as.factor(day)

lm_model <- lm(RCHRG_SUM ~ day, data = df_with_dummies)

# Extract the coefficients
coefficients <- coef(lm_model)

# Print the coefficients
print(coefficients)

# Create a data frame for the forecast period with the days of the week
forecast_dates <- seq.Date(max(df_with_dummies$date) + 1, length.out = 90, by = "day")
forecast_df <- data.frame(day = weekdays(forecast_dates))


# Create dummy variables for the days of the week
forecast_df <- cbind(forecast_df, model.matrix(~ day - 1, data = forecast_df))

# Predict the values using the coefficients
forecast_values <- predict(lm_model, newdata = forecast_df)

# Print the forecasted values
print(forecast_values)

autoplot(forecast_result) +
  xlab("Date") +
  ylab("RCHRG_SUM") +
  ggtitle("Forecasted RCHRG_SUM for Next 90 Days")







# Create an xts time series object
f_xts <- xts(f_data[, c("RCHRG_SUM")], order.by = f_data$date)


min(f_xts)
max(f_xts)

decomp <- decompose(f_xts, "multiplicative")

# Extract the seasonal component
seasonal_component <- decomp$seasonal

# Plot the seasonal component
plot(seasonal_component, main = "Daily Seasonal Effect")




ses(ts_data, alpha = .2, h = 90) |> autoplot()

ets(ts_data, model = "AAA") |> autoplot()




f_fcasts_models <- ts_data %>%
  model(arima = ARIMA(RCHRG_SUM),
        ets = ETS(RCHRG_SUM))
  





ggplot(f_data)+aes(x=date,y=RCHRG_SUM)+
  geom_line() +geom_smooth(method="lm",se=FALSE)

holt(ts_data, h = 90) |> autoplot()


fd.hw <- ets(ts_data, model = "ZZZ")
autoplot(forecast(fd.hw),h=90)


autoplot(forecast(ets(ts_data, model = "AZZ")),h=30)

forecast(ets(ts_data, model = "AZZ"),h=30)
fd.ann <- ets(f_data$RCHRG_SUM, model = "ANN")
autoplot(forecast(fd.ann),h=30)







ts_weekly <- split(f_xts, f = "weeks")
ts_weekly |> head(15)

ts_weekly |> head()





# Create a list of weekly means, temps_avg, and print this list
ts_avg <- lapply(X =ts_weekly, FUN = mean)
ts_avg






# Convert kse to weekly and assign to kse_weekly
f_weekly <- to.period(f_xts, period = "weeks",OHLC = FALSE)

autoplot(f_weekly)

ndays(f_xts)

# Count the weeks
nweeks(f_xts)

# Count the months

nmonths(f_xts)
# Count the quarters
nquarters(f_xts)

# Count the years

nyears(f_xts)


## Time zone

tzone(f_xts)


# Explore underlying units of f_xts in two commands: .index() and .indexwday()
.index(f_xts)
.indexwday(f_xts)










# Second part -------------------------------------------------------------



# Create an index of weekend days using which()
index <- which(.indexwday(f_xts) == 0 | .indexwday(f_xts) == 6)

# Select the index
f_xts[index]





f_data |> select(RCHRG_SUM, date) |> glimpse()

# Create an xts time series
daily_f_data <- xts(f_data)




library(dygraphs)


f_data |> 
  select(date, RCHRG_SUM) |> 
  dygraph(main="Totoal Recharge") |> 
  dyAxis("y", label = "RCHRG") |> 
  dyAxis("x", label = "date") |> 
  dyOptions(drawPoints = TRUE) |> 

  dyShading(from = "2023-7-1", to = "2024-1-1", color = "#FFE6E6") %>%
    dyShading(from = "2024-1-1", to = "2024-3-12", color = "#CCEBD6") |> 
  dyRangeSelector() 
