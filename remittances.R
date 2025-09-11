library(fable) # for forecasting

library(fpp3) # for forecasting

library(tsibble) # for time series data manipulation

library(dplyr) # for data manipulation

library(ggplot2) # for data visualization

library(xts) # for time series data manipulation

library(lubridate) # for date manipulation

library(tidyr) # for data manipulation

library(forecast) # for forecasting
# Load data

#rmt <- read.csv("data/remittances_karandaaz.csv") # Upto February 2024

rmt <- read.csv("docs/data/worker_remittances.csv") # Upto February 2024

# Inspect the data

rmt |> head(10) |> View()

# Inspect data 

rmt |> glimpse() 

rmt <- rmt[-1,] # remove the first row



# Convert the remaining columns to numeric
rmt <-  rmt |> mutate_at(vars(-1), as.numeric)

rmt |> head() |> View()

dates <- seq(as.Date("2024-04-01"), as.Date("2010-07-01"), by = "-1 month")

rmt$date <- dates

rmt |> glimpse()


rmt <- rmt %>% select(-Period) |> select(date, everything())

# Convert wide to long 

rmt_long <- rmt %>% pivot_longer(-date, names_to = "country", values_to = "remittances")

rmt_long |> distinct(country) 




rmt_long |> filter(country!="Total" ) |> 
ggplot()+ aes(x = date, y = remittances, color = country) +
  geom_line() +
  labs(title = "Remittances by Country",
       x = "Date",
       y = "Remittances") +
  theme_minimal()

## Calculate %share by dividing col 2 to 13 by column 14

rmt_share <- rmt |> mutate_at(vars(-date), ~./rmt$Total) |> select(date, Total, everything())


rmt_share_long <- rmt_share %>% pivot_longer(-date, names_to = "country", values_to = "share")

rmt_share_long |> filter(country!="Total" , share>0.08) |>
  ggplot()+ aes(x = date, y = share, color = country) +
  geom_line() +
  labs(title = "Remittances by Country",
       x = "Date",
       y = "Remittances") +
  theme_minimal()



library(plotly)
p <- rmt_long |> filter(country!="Total" ) |>
  ggplot()+ aes(x = date, y = remittances, color = country) +
  geom_line() +
  labs(title = "Remittances by Country",
       x = "Date",
       y = "Remittances") +
  theme_minimal() +theme(legend.position = "none")

ggplotly(p)

# Reverse the order of observations
rmt_reversed <- rev(rmt$Total)

# Fit the snaive model with the reversed data
fit_SN <- snaive(rmt_reversed, h = 12)

# Plot the forecast
autoplot(forecast::snaive(rmt_reversed, h = 24))

## tsiibble

rmt_long <- rmt_long |> mutate(month_year = yearmonth(date))

ts_data <- as_tsibble(rmt_long, key = country, index = month_year) 

total_rem <- ts_data %>% filter(country == "Total") |> select(-country) |> 
  select(-date) 


library(fabletools)

ts_data |> filter(country == "Total") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "Saudi.Arabia") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "United.Arab.Emirates") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "USA") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "United.Kingdom") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "EU.Countries") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)



saudi_rem <- ts_data %>% filter(country == "Saudi.Arabia") |> select(-country) |> 
  select(-date) 

autoplot(decompose(saudi_rem))+ ggtitle("Decomposition of Total Remittances")+
  theme_minimal()


saudi_rem <- ts(saudi_rem$remittances, start = c(year(min(total_rem$month_year)), month(min(total_rem$month_year))),
                end = c(year(max(total_rem$month_year)), month(max(total_rem$month_year))), frequency = 12)

autoplot(saudi_rem)


## Simple exponential smoothing

ses(saudi_rem, h = 12) |> forecast() |> autoplot()


## Holt's method

holt(saudi_rem, h = 12) |> forecast() |> autoplot()


## Holt-Winters method

hw(saudi_rem, h = 12) |> forecast() |> autoplot()

ets(saudi_rem, model = "AAA") |> forecast(h = 12) |> autoplot()






fit_ARIMA <- auto.arima(saudi_rem, stepwise = FALSE, approximation = FALSE)
summary(fit_ARIMA)

fit_ARIMA %>% forecast(h=24) %>% autoplot()

fit_ARIMA %>% forecast(h=24)

fit_ARIMA_total <- auto.arima(total_rem_ts, stepwise = FALSE, approximation = FALSE)

summary(fit_ARIMA_total)

fit_ARIMA_total %>% forecast(h=24) %>% autoplot()

fit_ARIMA_total %>% forecast(h=12)


# Create a ts object
total_rem_ts <- ts(total_rem$remittances, start = c(year(min(total_rem$month_year)), month(min(total_rem$month_year))),
                   end = c(year(max(total_rem$month_year)), month(max(total_rem$month_year))), frequency = 12)

# Check the structure of the new ts object
glimpse(total_rem_ts)

autoplot(total_rem_ts)

ets(total_rem_ts, model = "AAA") |> forecast(h = 12) |> autoplot()


ets(total_rem_ts, model = "AZZ") |> forecast(h = 12) |> autoplot()


ets(total_rem_ts, model = "ZZZ") |> forecast(h = 12) |> autoplot()


