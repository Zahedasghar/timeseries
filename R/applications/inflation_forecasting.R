#https://stackoverflow.com/questions/63488699/how-to-simulate-distribution-values-from-an-arima-in-order-to-plot-as-a-fanchart/63488700#63488700

library(tidyverse)
library(tsibble)
library(fable)

cpi <- 
  read_csv("D:/RepTemplates/timeseries/docs/004_module_4_Forecast_uncertainty_and_Model_evaluation/Thailand_M.csv")



cpi <- my_df_read|> mutate(Date=dmy(dateid01)) |> select(3,4)


# Create tsibble object
cpi <- cpi %>% 
  mutate(Date = yearmonth(Date)) %>%
  as_tsibble(index=Date)

# Fit ARIMA model to log data
# Fit ARIMA model to log data
fit <- cpi %>%
  model(arima = ARIMA(p))

# Simulated future sample paths
fit %>%
  generate(times=20, h="1 year") %>%
  autoplot(.sim) + autolayer(cpi, p) +
  ylab("CPI") +
  theme(legend.position="none")

#Fan plot
fit %>%
  forecast(h=6) %>%
  autoplot(cpi, level=seq(10,90,by=10)) +
  theme(legend.position="none")
