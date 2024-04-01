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

rmt <- read.csv("data/remittances_karandaaz.csv")

# Inspect the data

rmt |> head()

rmt <- rmt[-1,] # remove the first row
rmt |> glimpse()
# Convert the remaining columns to numeric
rmt[, -1] <- lapply(rmt[, -1], as.numeric)

dates <- seq(as.Date("2024-02-01"), as.Date("2010-07-01"), by = "-1 month")

rmt$date <- dates

rmt |> glimpse()


rmt <- rmt %>% select(-Period) |> select(date, everything())

# Convert wide to long 

rmt_long <- rmt %>% pivot_longer(-date, names_to = "country", values_to = "remittances")





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

rmt_share_long |> filter(country!="Total", country=="Malaysia") |>
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
total_rem |> head()


library(fabletools)

ts_data |> filter(country == "Total") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "Saudi.Arabia") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "United.Arab.Emirates") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "USA") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "United.Kingdom") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)

ts_data |> filter(country == "EU.Countries") |> model(SNAIVE(remittances ~ lag(12))) |> forecast(h = 12) |> autoplot(ts_data)
table(ts_data$country)


# Create a ts object
total_rem_ts <- ts(total_rem$remittances, start = c(year(min(total_rem$month_year)), month(min(total_rem$month_year))),
                   end = c(year(max(total_rem$month_year)), month(max(total_rem$month_year))), frequency = 12)

# Check the structure of the new ts object
glimpse(total_rem_ts)

autoplot(total_rem_ts)

ets(total_rem_ts, model = "AAA") |> forecast(h = 12) |> autoplot()


ets(total_rem_ts, model = "AZZ") |> forecast(h = 12) |> autoplot()


ets(total_rem_ts, model = "ZZZ") |> forecast(h = 12) |> autoplot()


autoplot(decompose(total_rem_ts))+ ggtitle("Decomposition of Total Remittances")+
  theme_minimal()

saudi_rem <- ts_data %>% filter(country == "Saudi.Arabia") |> select(-country) |> 
  select(-date) 

saudi_rem <- ts(saudi_rem$remittances, start = c(year(min(total_rem$month_year)), month(min(total_rem$month_year))),
                   end = c(year(max(total_rem$month_year)), month(max(total_rem$month_year))), frequency = 12)

autoplot(saudi_rem)

ets(saudi_rem, model = "AAA") |> forecast(h = 12) |> autoplot()



holt.saudi <- holt(saudi_rem, h = 12)
autoplot(holt.saudi)

holt.saudi <- holt(saudi_rem, h = 12, damped = TRUE)
autoplot(holt.saudi)



fit_ARIMA <- auto.arima(rmt_reversed, stepwise = FALSE, approximation = FALSE)
summary(fit_ARIMA)
fit_ARIMA %>% forecast(h=24) %>% autoplot()
auto.arima(rev(rmt$Saudi.Arabia)) %>% forecast(h=24) %>% autoplot()

rmt_long |> distinct(country) 
