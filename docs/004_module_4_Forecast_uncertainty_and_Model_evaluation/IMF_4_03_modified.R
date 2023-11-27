library(forecast)
library(stringr)
library(xts)
library(plyr)
library(tidyverse)
library(fanplot)
#### Reading data ####
# my data
my_df_read <-
  read_csv("D:/RepTemplates/timeseries/docs/004_module_4_Forecast_uncertainty_and_Model_evaluation/Thailand_M.csv")



my_df <- my_df_read|> mutate(date=dmy(dateid01))

cpi_xts <- xts(my_df$p,order.by=my_df$date) 

cpi_xts |> colnames()

ggplot(my_df)+ aes(x=date,y=p)+geom_line()

autoplot(cpi_xts)
## Make a line graph for p from cpi_xts

my_df |> tail()

acf(cpi_xts)

ggAcf(cpi_xts)

ggPacf(cpi_xts)

ts_data <- ts(my_df$p, start=2003:1, end=2014:1, frequency=12)

# Assuming your time series data is stored in 'ts_data'
# Replace 'ts_data' with your actual time series data
# Fit ARIMA(1,1,0) model
arima_model <- Arima(ts_data, order=c(1,1,0))

# Generate 6-month forecast
forecast_result <- forecast(arima_model, h=6)

# Evaluate forecast accuracy
accuracy_result <- accuracy(forecast_result)


# Calculate Thiel's U statistic manually
thiel_u <- sqrt(mean((forecast_result$mean - ts_data)^2)) / 
  (sqrt(mean(ts_data^2)) + sqrt(mean(forecast_result$mean^2)))

# View forecast result
print(forecast_result)

# View accuracy result
print(accuracy_result)

# View Thiel's U statistic
print(thiel_u)

# Load required libraries
library(forecast)
library(fanplot)

# Assuming your time series data is stored in 'ts_data'
# Replace 'ts_data' with your actual time series data
# Fit ARIMA(1,1,0) model
arima_model <- Arima(ts_data, order=c(1,1,0))

# Generate 6-month forecast
forecast_result <- forecast(arima_model, h=6)

# Create fan chart
fan_chart <- fan.plot(forecast_result, PI=TRUE, FUN=lines, lty=1, col=1, lwd=2, main="Fan Chart")

# Plot the fan chart
plot(fan_chart)












# arima specification
my_arima_order <- c(1, 1, 0)
my_seasonal_order <- c(0, 0, 0)

# forecasting parameters
my_w <- 55 # training time window if last date not used
my_h <- 6  # forecasting horizon  

# last date of training subsample
is_last_date_used <- FALSE
last_date <- "2008-12-01" 

# toggle between expanding and rolling forecasting strategy
is_expanding <- TRUE

# horizon column name in accuracy data.frame
my_id <- "my_fcast_h"

# stringr parameters for padding string (leave as is)
my_pad <- "0"
my_side <- "left"
my_prefix <- "fcast_"

#### Data wrangling ####
my_df <- 
  my_df_read %>% 
  mutate(dateid01 = as.Date(dateid01)) 

# getting first and last date
my_min_max_date <-
  my_df %>% 
  summarise(my_min_year = min(year(dateid01)),
            my_max_year = max(year(dateid01)))

# filter data according to last date of training sample
my_df_train <-
  my_df %>% 
  filter(dateid01 <= last_date)

# setting training subsample window length:
# if last date of training dataset is used, 
# than window size is calculated, if not,
# window size as set by the user is used.
if(last_date == TRUE){
  my_w <- my_df_train %>% count %>% unlist %>% unname
}

# setting time series object
my_ts <- 
  ts(my_x, start = my_min_max_date[, 1], frequency = 12)

# plot ts for visual inspection of a time series
tsdisplay(my_ts, las = 1)
abline(h = 0)

# my ts data.frame
my_ts_df <-
  data.frame(my_labels = cpi_xts %>% as.xts %>% index %>% as.character,
             my_obs_ts = cpi_xts %>% as.xts %>% coredata)

# length, start, end and frequency of time series
my_t <- length(cpi_xts)
my_start <- tsp(cpi_xts)[1]
my_end <- tsp(my_ts)[2]
my_freq <- tsp(my_ts)[3]

# parameter that tells how many times one can extend the training set 
# to produce forecasts for assumed horizon.
my_myst_param <- my_t - my_w - my_h

# start of validation set
my_valid <- time(my_ts)[my_w]

# stringr parameters (width)
my_width <- my_myst_param %>% nchar

# data
y <- ts(my_ts, start = my_start, frequency = my_freq)

fcasts <- list()

# expanding forecasts
for (i in 1:my_myst_param) {
  
  if(is_expanding){
    
    win.y <- window(y, end = my_valid + i / my_freq)
    
    fit <- Arima(win.y, order = my_arima_order,
                 seasonal = my_seasonal_order,
                 include.constant = TRUE)  
  } else {
    
    win.y <- window(y, 
                    start = my_start + i / my_freq, 
                    end = my_valid + i / my_freq)
    
    fit <- Arima(win.y, order = my_arima_order,
                 seasonal = my_seasonal_order,
                 include.constant = TRUE)
    
  }
  
  fcasts[[i]] <- forecast(fit, h = my_h)
}

names(fcasts) <- 
  str_c(my_prefix,                       
        str_pad(1:my_myst_param, width = my_width, 
                pad = my_pad, side = my_side))

# getting point forecasts and corresponding dates
my_means <- list()

for(k in seq_along(fcasts)){
  
  my_mean <- fcasts[[k]]$mean %>% as.xts
  my_labels <- my_mean %>% index %>% as.character
  
  my_df <- 
    data.frame(my_labels, my_mean) %>% 
    mutate(my_h_fcasts = 1:n())
  
  my_means[[k]] <- my_df
  
}

# joining forecasts and original data
my_fcasts_df <- 
  ldply(my_means, data.frame) %>%
  left_join(., my_ts_df)

# computing accuracy statistics
my_fcasting_acc_df <-
  my_fcasts_df %>% 
  dlply(., .(my_h_fcasts)) %>% 
  lapply(.,function(x) accuracy(ts(x$my_mean), 
                                ts(x$my_obs_ts))) %>% 
  ldply(., data.frame, .id = my_id)

# a look at accuracy statistics
my_fcasting_acc_df

