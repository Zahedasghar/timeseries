options(repos = list(CRAN="http://cran.rstudio.com/"))
library(Spillover, quietly = TRUE)
data(dy2012)

library(tidyverse)

dy2012 |> glimpse()
dy2012 %>% 
  dplyr::select(-Date) %>% 
  VAR(p=4) %>% 
  G.spillover(standardized = FALSE) %>% 
  round(2)


## Now, let’s move on, and get the dynamic total spillover index:
  
  total_dynamic <- total.dynamic.spillover(as.zoo(dy2012[,-1]),
                                           width = 200, 
                                           index="generalized",
                                           p=4) 

require(ggplot2)

tibble(Date= dy2012$Date[-c(1:199)],  index = total_dynamic) %>% 
  mutate(Date=as.Date(as.character(Date))) %>% 
  ggplot(aes(x=Date, y=index)) +
  geom_line()+
  labs(caption = "Fig 2. Total volatility spillovers, four asset classes.")+
  theme(plot.caption = element_text(hjust = 0))


dy_results <- dynamic.spillover(data=dy2012, 
                                width=200, 
                                p=4) 


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

library(vars)

library(xts)

dyndf <- dyndf |> mutate(lnifty=log(nifty50),lsp=log(sp500))

# Assuming 'df' is your data frame
xts_data <- xts(dyndf[, c("lnifty", "lsp")], order.by = dyndf$date)

xts_data 

estimate_var_sensitivity <- function(xts_data, max_lag = 10) {
  results <- data.frame(Lag_Order = numeric(max_lag),
                        Fit_Criterion = numeric(max_lag))
  
  for (lag in 1:max_lag) {
    var_model <- VAR(xts_data, p = lag)
    fit_stats <- summary(var_model)$criteria
    
    if (length(fit_stats) > 0) {
      fit_criterion <- fit_stats[1]  # Choose a fit criterion (e.g., AIC, BIC, HQIC)
      
      results$Lag_Order[lag] <- lag
      results$Fit_Criterion[lag] <- fit_criterion
    }
  }
  
  results <- na.omit(results)  # Remove rows with missing fit criteria
  return(results)
}


estimate_var_model
# Replace 'your_data' with your actual dataset
lag_sensitivity_results <- estimate_var_sensitivity(xts_data)

lag_sensitivity_results
# Replace 'your_data' with your actual dataset





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

library(vars)

# Assuming 'data' is your time series data
max_lag <- 10  # Set the maximum lag order to consider
aic_values <- numeric(max_lag)

for (lag in 1:max_lag) {
  var_model <- VAR(xts_data, p = lag)
  aic_values[lag] <- AIC(var_model)
}

library(ggplot2)

lag_orders <- 1:max_lag
aic_df <- data.frame(Lag_Order = lag_orders, AIC = aic_values)

aic_df

ggplot(aic_df, aes(x = Lag_Order, y = AIC)) +
  geom_line() +
  labs(title = "Sensitivity of VAR Model to Lag Order",
       x = "Lag Order",
       y = "AIC") +
  theme_minimal()

xts_data |> head()
