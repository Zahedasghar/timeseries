library(tidyverse)

library(xts)

library(zoo)

library(janitor)

library(Spillover)

library(vars)
dyn_df <- read_csv('data/indo_us.csv') 


dyn_df |> glimpse()


dyn_df$date <- dmy(dyn_df$Date)

dyndf <- dyn_df |> dplyr::select(date, NIFTY50,SP500) 

dynam <- dyndf

dyndf |> glimpse()

## Time plots to see idea how indices move 

dyndf |> 
  pivot_longer(cols = c(nifty50, sp500), names_to = "Variable", values_to = "Value") |> 
ggplot()+ aes(x = date, y = log(Value), color = Variable) +
  geom_line() +
  labs(title = "Multiple Line Graph", x = "Date", y = "Y-Axis Label", color = "Variable") +
  theme_minimal()




dyndf %>% 
  dplyr::select(-date) %>% 
  VAR(p=2) %>% 
  G.spillover(standardized = FALSE) %>% 
  round(2)







## Now, let’s move on, and get the dynamic total spillover index:

total_dynamic <- total.dynamic.spillover(as.zoo(dyndf[,-1]),
                                         width = 20, 
                                         index="generalized",
                                         p=2) 


require(ggplot2)

tibble(Date= dyndf$date[-c(1:19)],  index = total_dynamic) %>% 
   ggplot(aes(x=Date, y=index)) +
  geom_line()+
  labs(caption = "Fig 2. Total volatility spillovers for both indices")+
  theme(plot.caption = element_text(hjust = 0))

dy_results <- dynamic.spillover(data=dyndf, 
                                width=20, 
                                p=2) 

# Directional volatility spillovers, FROM four asset classes
# In order to get Figure 3 from Diebold and Yilmaz (2012), we proceed as follows:

pp_from <- plotdy(data=dy_results, direction = "from") 


pp_from +
  labs(caption = "Fig 3. Directional volatility spillovers, FROM four asset classes.")+
  theme(plot.caption = element_text(hjust = 0.5))  
pp_to <- plotdy(dy_results, direction = "to")  


# Just for customization 
pp_from +
  labs(caption = "Fig. 4. Directional volatility spillovers, TO four asset classes.")+
  theme(plot.caption = element_text(hjust = 0.5))  

pp_net <- plotdy(dy_results, direction = "net")  

pp_net +
  labs(caption = "Fig. 5. Net volatility spillovers, four asset classes.")+
  theme(plot.caption = element_text(hjust = 0.5))

#Net pairwise volatility spillovers.
pp_netpairwise <- plotdy(dy_results, direction = "net_pairwise")  

pp_netpairwise +
  labs(caption = "Fig. 6. Net pairwise volatility spillovers.")+
  theme(plot.caption = element_text(hjust = 0.5))  

pp_from_to_pairwise <- plotdy(dy_results, direction = "from_to_pairwise")





# Calculate statistics for nifty50
mean_nifty50 <- mean(dyndf$nifty50)
sd_nifty50 <- sd(dyndf$nifty50)
var_nifty50 <- var(dyndf$nifty50)
max_nifty50 <- max(dyndf$nifty50)
min_nifty50 <- min(dyndf$nifty50)

# Calculate statistics for sp500
mean_sp500 <- mean(dyndf$sp500)
sd_sp500 <- sd(dyndf$sp500)
var_sp500 <- var(dyndf$sp500)
max_sp500 <- max(dyndf$sp500)
min_sp500 <- min(dyndf$sp500)

# Print the results
cat("Statistics for Time Series 1:\n")
cat("Mean:", mean_nifty50, "\n")
cat("Standard Deviation:", sd_nifty50, "\n")
cat("Variance:", var_nifty50, "\n")
cat("Maximum:", max_nifty50, "\n")
cat("Minimum:", min_nifty50, "\n")

cat("\nStatistics for Time Series 2:\n")
cat("Mean:", mean_sp500, "\n")
cat("Standard Deviation:", sd_sp500, "\n")
cat("Variance:", var_sp500, "\n")

cat("Maximum:", max_sp500, "\n")
cat("Minimum:", min_sp500, "\n")


# Perform Jarque-Bera test for nifty50
jb_test_nifty50 <- jarque.bera.test(dyndf$nifty50)
cat("\nJarque-Bera Test for Time Series 1:\n")
cat("JB Statistic:", jb_test_nifty50$jb1, "\n")
cat("P-Value:", jb_test_nifty50$p.value, "\n")

# Perform Jarque-Bera test for sp500
jb_test_sp500 <- jarque.bera.test(dyndf$sp500)
cat("\nJarque-Bera Test for Time Series 2:\n")
cat("JB Statistic:", jb_test_sp500$jb1, "\n")
cat("P-Value:", jb_test_sp500$p.value, "\n")


library(urca)

# Perform ADF test for nfity50
adf_test_nfity50 <- ur.df(dyndf$nifty50, lags = 1)
adf_summary_nfity50 <- summary(adf_test_nfity50)
adf_p_value_nfity50 <- adf_summary_nfity50@test$p.value[1]

# Perform ADF test for ts2
adf_test_ts2 <- ur.df(ts2, lags = 1)
adf_summary_ts2 <- summary(adf_test_ts2)
adf_p_value_ts2 <- adf_summary_ts2@test$p.value[1]

# Create a data frame for the results
results <- data.frame(
  Variable = c("nifty50", "sp500"),
  Mean = c(mean_nifty50, mean_sp500),
  SD = c(sd_nifty50, sd_sp500),
  Variance = c(var_nifty50, var_sp500),
  Max = c(max_nifty50, max_sp500),
  Min = c(min_nifty50, min_sp500),
  JB_P_Value = c(2.43975e-07, 2.129131e-06 ),
  ADF_Statistic = c(adf_stat_nifty50, adf_stat_sp500)
)

results

library(tidyr)
library(gt)

results %>%
  gather(Statistic, Value, -Variable) |> gt() |> fmt_number(decimals = 3)



estimate_var <- function(dyndf, p) {
  # Estimate VAR model with lag order p
  var_model <- VAR(dyndf, p = 2)
  
  # Extract the index (e.g., first variable) from the model
  index <- var_model$nifty50[[1]]$Y[, 1]
  
  return(index)
}

estimate_var

estimate_var_sensitivity <- function(xts_data, max_lag = 10) {
  results <- data.frame(Lag_Order = numeric(max_lag),
                        Fit_Criterion = numeric(max_lag))
  
  for (lag in 1:max_lag) {
    var_model <- VAR(xts_data, p = lag)
    fit_stats <- summary(var_model)$criteria
    fit_criterion <- fit_stats[1]  # Choose a fit criterion (e.g., AIC, BIC, HQIC)
    
    results$Lag_Order[lag] <- lag
    results$Fit_Criterion[lag] <- fit_criterion
  }
  
  return(results)
}

xts_data
# Replace 'your_data' with your actual dataset
lag_sensitivity_results <- estimate_var_sensitivity(xts_data)



library(ggplot2)

ggplot(lag_sensitivity_results, aes(x = Lag_Order, y = Fit_Criterion)) +
  geom_line() +
  labs(title = "Sensitivity of VAR Model to Lag Order",
       x = "Lag Order",
       y = "Fit Criterion") +
  theme_minimal()









library(parallel)
library(rugarch)
# Creating a function to fit GARCH(1,1) model
fit_garch_model <- function(dynam, NIFTY50) {
  # Extract the returns
  returns <- diff(log(dynam[["NIFTY50"]]))
  
  # Fit a GARCH(1,1) model
  garch_spec <- ugarchspec(
    mean.model = list(armaOrder = c(0, 0)),
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    distribution.model = "std"
  )
  garch_fit <- ugarchfit(spec = garch_spec, data = dynam)
  
  # Display summary of the GARCH model
  cat("Summary of GARCH(1,1) Model for", NIFTY50, ":\n")
  cat("---------------------------------------------------\n")
  print(garch_fit)
  
  return(garch_fit)
}

# Setting the date ranges
pre_covid_start_date <- as.Date("2018-01-08")
covid_start_date <- as.Date("2019-12-30")
post_covid_start_date <- as.Date("2021-05-03")
end_date <- as.Date("2023-08-31")  # Your dataset's end date

# Subsetting the data for crisis analysis
pre_covid_data <- dynam[dynam$date >= pre_covid_start_date & dynam$date < covid_start_date, ]
# Subsetting the data for crisis analysis
pre_covid_data <- dynam[dynam$date >= pre_covid_start_date & dynam$date < covid_start_date, ]
covid_data <- dynam[dynam$date >= covid_start_date & dynam$date < post_covid_start_date, ]
post_covid_data <- dynam[dynam$date >= post_covid_start_date & dynam$date <= end_date, ]

# Fit GARCH models for each period
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))


fit1 <- ugarchfit(spec1, data = xts_data$lnifty)
fit2 <- ugarchfit(spec2, data = xts_data$lsp)



garch_model_pre_covid <- fit_garch_model(pre_covid_data, "NIFTY50")
garch_model_covid <- fit_garch_model(covid_data, "NIFTY50")
garch_model_post_covid <- fit_garch_model(post_covid_data, "NIFTY50")


summary(fit1)
summary(fit2)

coef(fit1)
coef(fit2)

plot(fit1)
plot(fit2)


garch_model_pre_covid <- fit_garch_model(pre_covid_data, "SP500")
garch_model_covid <- fit_garch_model(covid_data, "SP500")
garch_model_post_covid <- fit_garch_model(post_covid_data,"SP500")
