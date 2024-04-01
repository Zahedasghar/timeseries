library(prophet)
library(tidyverse)
library(janitor)


# Load data

visits <- read_csv("D:/RepTemplates/timeseries/docs/data/viewers.csv")

# Inspect the data

glimpse(visits)

dim(visits)

# Convert Date  to daily from Jan 2, 2018 to 27 March 2024

dates <- seq(as.Date("2018-01-02"), as.Date("2024-03-27"), by = "1 day")

visits$Date <- dates


# Convert to ds and y

Visits <- visits |> 
  rename(ds = Date, y = Views)

# Fit the model

m <- prophet(Visits)

# Forecast

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

# Plot the forecast

plot(m, forecast)


head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

prophet_plot_components(m, forecast)

