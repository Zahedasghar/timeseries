~
## Simple Exponential Smoothing
## Holt's method
## Holt-Winters method~
## ARIMA method
## ETS method (Error, Trend, Seasonality)


# Load libraries

library(tidyverse) # for data m~`~anipulation


library(tsibble) # for time series data manipulation



library(tidyr) # for data manipulation

library(forecast) # for forecasting
# Load data``


total_rem <- read.csv("data/total.csv") 


# Inspect the data

total_rem |> head(10) |> View()

# Inspect data 

total_rem |> glimpse() 

total_rem <- total_rem[-1,-1] # remove the first row and first column



# Convert the remaining columns to numeric
rem <-  total_rem |> mutate_at(vars(-1), as.numeric)

rem |> head() |> View()

rem |> glimpse()

dates <- seq(as.Date("2024-02-01"), as.Date("2010-07-01"), by = "-1 month")

rem$date <- dates

rem |> glimpse()

rem |> head(10) |> View()


rem <- rem %>% select(-Period) |> select(date, everything())

colnames(rem) <- c("date", "remittances")


 
  ggplot(rem)+ aes(x = date, y = remittances) +
  geom_line() +
  labs(title = "Total remittances in millions of USD",
       x = "Date",
       y = "Remittances") +
  theme_minimal()

# Fit the snaive model with the reversed data
fit_SN <- snaive(rev(rem$remittances), h = 12)
autoplot(fit_SN)
# Plot the forecast
autoplot(forecast::snaive(rev(rem$remittances), h = 24))

## tsibble

rem1 <- rem |> mutate(month_year = yearmonth(date))

ts_data <- as_tsibble(rem1, index = month_year) 

## Forecasting Practice and Principles

total_rem <- ts_data  |> 
  select(-date) |> select(month_year, remittances)

total_rem <- ts(total_rem$remittances, start = c(year(min(total_rem$month_year)), month(min(total_rem$month_year))),
                end = c(year(max(total_rem$month_year)), month(max(total_rem$month_year))), frequency = 12)






autoplot(decompose(total_rem))+ ggtitle("Decomposition of Total Remittances")+
  theme_minimal()



autoplot(total_rem)

class(total_rem)


## Simple exponential smoothing

ses(total_rem, h = 12) |> forecast() |> autoplot()


## Holt's method

holt(total_rem, h = 12) |> forecast() |> autoplot()


## Holt-Winters method

hw(total_rem, h = 12) |> forecast() |> autoplot()

ets(total_rem, model = "ZZZ") |> forecast(h = 12) |> autoplot()



fit_ARIMA <- auto.arima(total_rem, stepwise = FALSE, approximation = FALSE)
summary(fit_ARIMA)

fit_ARIMA %>% forecast(h=24) %>% autoplot()

fit_ARIMA %>% forecast(h=24)


fit_ARIMA_total %>% forecast(h=12)


ets(total_rem, model = "AAA") |> forecast(h = 12) |> autoplot()


ets(total_rem, model = "AZZ") |> forecast(h = 12) |> autoplot()


ets(total_rem, model = "ZZZ") |> forecast(h = 12) |> autoplot()


