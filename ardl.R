library(tidyverse)
library(readxl)
library(xts)
library(ARDL)
library(tidyr)
library(janitor) # to clean names

ardlf <- read_excel("data/ardl.xlsx", skip=3)

ardlf |> clean_names() -> ardlf

ardlf |> glimpse()
ardlf |> endpoints(on='months')

ardlf$date <- format(ardlf$date, format = "%m-%d-%Y")

ardlf$date <- mdy(ardlf$date)


# amihuid, illiddity, turn over, ratio, bid ask, sprea, market cap, annualized volatility
# oen, beta month, dy
# tri
# Annualized volatility
# Beta weekly leveraged
# dy
# market cap
# pe
# liquidity

# rs
# _
# metrics %>%
#   group_
# by(.metric) %>%
#   summarize(min = min(.estimate),
#             median = median(.estimate),
#             max = max(.estimate),
#             mean = mean(.estimate),
#             sd = sd(.estimate)

  
 ardlf |>  
summarise(
  across(
    .cols = c(
      tri, annualised_volatility_open,beta_monthly_leveraged,dy, market_cap,pe, liqudity
    ),
    .fns = c(
      n = ~sum(!is.na(.)),
      mean = \(x) mean(x, na.rm = TRUE),
      variance =\(x) var(x, na.rm = TRUE),
      min = \(x) min(x, na.rm = TRUE),
      max = \(x) max(x, na.rm = TRUE)
    ),
    .names = '{.col}----{.fn}'
  )
) |> 
  pivot_longer(
    cols = everything(),
    names_sep = '----',
    names_to = c('variable', 'stat')
  )  -> ardf


ardf |> pivot_wider(names_from = stat, values_from = value) ->wide_data
wide_data

# Optional: Rename columns
colnames(wide_data) <- c("Variable", "Observations","Mean", "Variance", "Min", "Max")

# Reset row names to NULL
row.names(wide_data) <- NULL

wide_data |> View()

# Assuming 'wide_data' is your data frame
rounded_data <- wide_data

# Identify numeric columns to round
numeric_columns <- sapply(rounded_data, is.numeric)

# Round numeric columns to 2 decimal places
rounded_data[numeric_columns] <- lapply(rounded_data[numeric_columns], function(x) round(x, 2))

library(gt)
library(gtExtras)
rounded_data |> gt() |> fmt_number(
  decimals = 2
) |> gt_theme_pff()



correlation_matrix <- ardlf |> select(
  tri, annualised_volatility_open,beta_monthly_leveraged,dy, market_cap,pe, liqudity) |> cor()

# Create a data frame from the correlation matrix
correlation_df <- as.data.frame(correlation_matrix)

# Create a gt table
correlation_table <- correlation_df  |> 
  gt() |> fmt_number(
    columns = everything(),
    decimals = 2
  )  
library(data.table)
# Convert the correlation matrix to a data table with variable names
correlation_table <- data.table(data.frame(Var = colnames(correlation_matrix), correlation_matrix))

# Print the correlation table
correlation_table |> gt() |>  fmt_number(
  decimals = 2
)


# transforming variables into log form

ardlf <- ardlf |> mutate(ln_annualised_vol_open=log(annualised_volatility_open),ln_mkt_cap=log(market_cap),
                        lntri=log(tri))

library(tseries)
library(purrr)


## ADF test

ardlf %>%
  select(ln_annualised_vol_open,lntri,ln_mkt_cap,pe,dy,beta_monthly_leveraged,liqudity) %>%
  map(~adf.test(.)) |> 
  map_df(broom::tidy, .id = "Variable") ->adf_test
## PP test

ardlf %>%
  select(ln_annualised_vol_open, lntri,ln_mkt_cap,pe,dy,beta_monthly_leveraged,liqudity) %>%
  map(~pp.test(.)) |> 
  map_df(broom::tidy, .id = "Variable") ->pp_test

adf_test <- as_tibble(adf_test)

pp_test <- as_tibble(pp_test) 


 bind_cols(
   adf_test,
   pp_test
)  |> select(1:3,8,9) -> adf_pp
 
 colnames(adf_pp) <- c("Variable", "adf_stat","p_val", "pp_stat", "p_val")
 
 adf_pp |> View()

# Load the urca package
library(urca)

library(ARDL)
ardl_mod <- auto_ardl(lntri~ln_annualised_vol_open+ln_mkt_cap+pe+dy+beta_monthly_leveraged+liqudity, data = ardlf, max_order = 4)

# tri amihud_illiqudity turn_over_ratio bid_ask_spread 
# 2                 4               0              0
# beta_monthly_leveraged dy annualised_volatility_open market_cap
#               0         4                          4   1
# (2,2,2,1,2,0,0)
              
              
# The top 20 models according to the AIC
ardl_mod$top_orders 
              
# The best model was found to be the ARDL(3,1,3,2)
ardl_22212000 <- ardl_mod$best_model
ardl_22212000 |> tidy()

uecm_22212000 <- uecm(ardl_22212000)
summary(uecm_22212000)


# And also the RECM (Restricted Error Correction Model) of the underlying
# ARDL(2,3,0,0,0,4,4), allowing the constant to join the long-run relationship (case
# 2), instead of the short-run (case 3).

recm_2221200 <- recm(uecm_22212000, case = 2) 

summary(recm_2221200)
              




#Let’s test if there is a long-run levels relationship (cointegration) using the bounds test from Pesaran et al. (2001).

# The bounds F-test (under the case 2) rejects the NULL hypothesis (let's say, assuming alpha = 0.01) with p-value = 0.004418.
bounds_f_test(recm_2221200, case = 2)

tbounds <- bounds_t_test(uecm_22212000, case = 3, alpha = 0.01)
tbounds

# So t=-2.855 lies below lower limit and indicates there is no cointegration and p-value is also 0.5683

#Here we have the short-run and the long-run multipliers (with standard errors, t-statistics and p-values).

multipliers(ardl_22212000, type = "sr")

multipliers(ardl_22212000, type = "lr")


coef(ardl_22212000, type = "long")


#We can also estimate and visualize the delay multipliers along with their standard errors.

mult15 <- multipliers(ardl_22212000, type = 15, se = TRUE)
plot_delay(mult15, interval = 0.95)


#Now let’s graphically check the estimated long-run relationship (cointegrating equation) against the dependent variable LRM.

ce <- coint_eq(ardl_22212000, case = 2)

ce
plot_lr(ardl_22212000, coint_eq = ce, show.legend = TRUE)





#Estimating and ARDL using dynardl
ardl_mod1 <- dynardl(lntri~ln_annualised_vol_open+ln_mkt_cap+pe+dy+beta_monthly_leveraged+liqudity, data = ardlf,
                lags = list("tri"=2,"amihud_illiqudity"=4, "dy"=4, "annualised_volatility_open"=4, "market_cap"=1 ),
                ec = TRUE, simulate = FALSE)

ardl_mod1 <- dynardl(tri ~ L(tri, 1) + amihud_illiquidity + turn_over_ratio + bid_ask_spread + beta_monthly_leveraged + dy + annualised_volatility_open + market_cap, data = ardlf,
                     lags = list("amihud_illiquidity" = 4, "turn_over_ratio" = 0, "bid_ask_spread" = 0,
                                 "beta_monthly_leveraged" = 0, "dy" = 4, "annualised_volatility_open" = 4, "market_cap" = 1),
                     ec = TRUE, simulate = FALSE)




#Declaring the Time Series Variables
tri <- ts(ardlf$tri, start = c(2013,1,1), frequency = 12)
amihud_illiqudity <- ts(ardlf$amihud_illiqudity, start = c(2013,1,1), frequency = 12)
turn_over_ratio<- ts(ardlf$turn_over_ratio, start = c(2013,1,1), frequency = 12)
bid_ask_spread <- ts(ardlf$bid_ask_spread, start = c(2013,1,1), frequency = 12)

beta_monthly_leveraged <- ts(ardlf$beta_monthly_leveraged, start = c(2013,1,1), frequency = 12)
annualised_volatility_open <- ts(ardlf$annualised_volatility_open, start = c(2013,1,1), frequency = 12)

dy <- ts(ardlf$dy, start = c(2013,1,1), frequency = 12)
market_cap <- ts(ardlf$market_cap, start = c(2013,1,1), frequency = 12)

ts_plot(tri)

ts_plot(amihud_illiqudity)
ts_plot(turn_over_ratio)
ts_plot(bid_ask_spread)
ts_plot(beta_monthly_leveraged)
ts_plot(dy)
ts_plot(market_cap)

ts_plot(annualised_volatility_open)

library(forecast)


adf.test(tri)
adf.test(amihud_illiqudity)
adf.test(turn_over_ratio)
adf.test(bid_ask_spread)
adf.test(beta_monthly_leveraged)
adf.test(dy)
adf.test(market_cap)

adf.test(annualised_volatility_open)

ardl_mod1 <- dynardl(tri ~ lag(tri,1)+lag(tri,2)+ amihud_illiquidity + turn_over_ratio + bid_ask_spread + 
                       beta_monthly_leveraged + dy + annualised_volatility_open + market_cap, data = ardlf,
                     lags = list( "amihud_illiquidity" = 4, "turn_over_ratio" = 0, "bid_ask_spread" = 0,
                                 "beta_monthly_leveraged" = 0, "dy" = 4, "annualised_volatility_open" = 4, "market_cap" = 1),
                     ec = TRUE, simulate = FALSE)






