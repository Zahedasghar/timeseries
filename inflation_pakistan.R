#https://stackoverflow.com/questions/63488699/how-to-simulate-distribution-values-from-an-arima-in-order-to-plot-as-a-fanchart/63488700#63488700

library(tidyverse)
library(tsibble)
library(fable)
library(janitor)
library(xts)
library(forecast)
library(tseries)
library(urca)
cpi <- 
  read_csv("data/cpi.csv") #Pakistan cpi by PBS

cpi <- cpi |> clean_names() 

# Convert period to Date
cpi <- cpi %>%
  mutate(Date = parse_date(paste("01", period, sep = "-"), format = "%d-%b-%y"))


cpi <- cpi |> select(Date, year_on_year, month_over_month)

ggAcf(cpi$year_on_year)
ggPacf(cpi$year_on_year)

adf.test(cpi$year_on_year)
pp.test(cpi$year_on_year)
ggAcf(cpi$month_over_month)

ggPacf(cpi$month_over_month)



cpi |> model(arima=ARIMA(month_over_month))

auto.arima(cpi$year_on_year)
#cpi <- cpi |> mutate(Date=dmy(period)) 


# Create tsibble object
cpi <- cpi %>% 
  mutate(Date = yearmonth(Date)) %>%
  as_tsibble(index=Date)

# Fit ARIMA model to log data
# Fit ARIMA model to log data
fit <- cpi %>%
  model(arima = ARIMA(year_on_year))


# Simulated future sample paths
fit %>%
  generate(times=20, h="1 year") %>%
  autoplot(.sim) + autolayer(cpi, year_on_year) +
  ylab("CPI") +
  theme(legend.position="none")

#Fan plot
fit %>%
  forecast(h=6) %>%
  autoplot(cpi, level=seq(10,90,by=10)) +
  theme(legend.position="none")+theme_minimal()

# Convert cpi$Date to Date class
cpi$Date <- as.Date(cpi$Date, format = "%b-%y")

# Create xts object
cpi_yoy <- xts(cpi$year_on_year, order.by = cpi$Date)

autoplot(cpi_yoy)


arima_model <- Arima(cpi$year_on_year, order=c(1,1,0))

# Generate 6-month forecast
forecast_result <- forecast(arima_model, h=6)

# Evaluate forecast accuracy
accuracy_result <- accuracy(forecast_result)

print(accuracy_result)




# Load required libraries
library(forecast)

# Assuming your monthly inflation data is stored in 'inflation_data'
# Replace 'inflation_data' with your actual data

# Convert the data to a time series object
inflation_ts <- ts(cpi$year_on_year, start=c(2019, 10), frequency=12)

# Split the data into training and test sets (e.g., 80% training, 20% test)
train_size <- floor(length(inflation_ts) * 0.8)
train_data <- window(inflation_ts, end=c(2023, 4))
test_data <- window(inflation_ts, start=c(2023, 5))

# Train the ARIMA model
arima_model <- Arima(train_data, order=c(1, 1, 0))  # Example order, replace with appropriate values

arima_mod1 <- ARIMA(train_data)
# Make forecasts
forecast_result <- forecast(arima_model, h=length(test_data))

# Print the forecast results
print(forecast_result)

forecast(arima_mod1,h=length(test_data))
arima_mod1

library(dygraphs)
cpi %>% 
  select(Date, year_on_year) %>%
  dygraph(main = "Consumer price index") %>%
  dyAxis("y", label = "Price") %>%
  dyAxis("x", label = "date")  |> 
  dyRangeSelector()


cpi %>% mutate(d_inf=year_on_year-lag(year_on_year)) |> 
  select(Date, d_inf) %>%
  dygraph(main = "Consumer price index") %>%
  dyAxis("y", label = "Price") %>%
  dyAxis("x", label = "date")  |> 
  dyRangeSelector()
