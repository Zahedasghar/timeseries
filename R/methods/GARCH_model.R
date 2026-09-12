library(tidyverse)

library(xts)

library(zoo)

library(janitor)

library(Spillover)

library(vars)

library(parallel)
library(rugarch)

dyn_df <- read_csv('data/indo_us.csv') 


dyn_df |> glimpse()


dyn_df$date <- dmy(dyn_df$Date)

dyndf <- dyn_df |> dplyr::select(date, NIFTY50,SP500) 

dynam <- dyndf


dynam1 <- dynam |> mutate(log_nifty = log(NIFTY50),log_sp=log(SP500))
dynam1 <- dynam1 |> mutate(nifty_return = log_nifty-lag(log_nifty),
                           sp_return=log_sp-lag(log_nifty))
dynam2 <- dynam1 |> dplyr::select(date,log_nifty, log_sp)



# Setting the date ranges
pre_covid_start_date <- as.Date("2018-01-08")
covid_start_date <- as.Date("2019-12-30")
post_covid_start_date <- as.Date("2021-05-03")
end_date <- as.Date("2023-08-31")  # Your dataset's end date

# Subsetting the data for crisis analysis
pre_covid_data <- dynam2[dynam2$date >= pre_covid_start_date & dynam2$date < covid_start_date, ]

# Subsetting the data for crisis analysis
pre_covid_data <- dynam2[dynam2$date >= pre_covid_start_date & dynam2$date < covid_start_date, ]
covid_data <- dynam2[dynam2$date >= covid_start_date & dynam2$date < post_covid_start_date, ]
post_covid_data <- dynam2[dynam2$date >= post_covid_start_date & dynam2$date <= end_date, ]

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))



fit1 <- ugarchfit(spec1, data = dynam2$log_nifty)
fit2 <- ugarchfit(spec2, data = dynam2$log_sp)


garch_model_pre_covid <- fit_garch_model(pre_covid_data, pre_covid_data$log_nifty)


garch_model_covid <- fit_garch_model(covid_data, covid_data$log_nifty)
garch_model_post_covid <- fit_garch_model(post_covid_data, post_covid_data$log_nifty)


summary(fit1)
summary(fit2)

coef(fit1)
coef(fit2)

plot(fit1)
plot(fit2)
