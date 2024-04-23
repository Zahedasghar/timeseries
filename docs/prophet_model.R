
# Facebook’s time series forecasting package Prophet~ is designed for analyzing
# time series that display patterns on different time scales such as yearly,
# weekly, and daily. It also has advanced capabilities for modeling the effects
# of holidays on a time series and implementing custom changepoints. Prophet is
# robust to missing data and shifts in the trend, and typically handles outliers
# well. It is used in many applications across Facebook for producing reliable 
# forecasts for planning and goal setting.~ 
~
# Load libraries

#install.packages("prophet") # for forecasting if not already installed

library(prophet) # for forecasting
library(dplyr) # for data manipulation~


# Load data

visits <- read_csv("D:/RepTemplates/timeseries/docs/data/viewers.csv")

# Inspect the data

glimpse(visits)

dim(visits)

head(visits)
tail(visits)
# Convert Date  to daily from Jan 2, 2018 to 27 March 2024

dates <- seq(as.Date("2018-01-02"), as.Date("2024-03-27"), by = "1 day")

visits$Date <- dates


# Convert to ds and y

Visits <- visits |> 
  rename(ds = Date, y = Views)

# Fit the model

m <- prophet(Visits, daily.seasonality = TRUE, weekly.seasonality = TRUE, seasonality.mode = 'multiplicative')

# Forecast

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

# Plot the forecast

plot(m, forecast) 


head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

prophet_plot_components(m, forecast)
``
