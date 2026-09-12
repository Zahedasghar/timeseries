#https://rpubs.com/nomarpicasso/1151428
library(tidyverse)
library(modeltime)
library(xgboost)
library(tidymodels)
library(timetk)

visits_daily <- read_csv("D:/RepTemplates/timeseries/docs/data/viewers.csv")

dates <- seq(as.Date("2018-01-02"), as.Date("2024-03-27"), by = "1 day")

visits_daily$date <- dates


glimpse(visits_daily)
visits_daily <- visits_daily[,-1]
colnames(visits_daily) <- c( "value","date")

visits_tbl <- visits_daily 

glimpse(visits_tbl)

# Plot data set 

visits_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Bike Sharing Transactions",
       x = "Date",
       y = "Transactions") +
  theme_minimal()

visits_tbl %>%
  plot_time_series(date, value, .interactive = TRUE)

# Split data set into training and testing sets
#Let’s split the data into training and test sets using initial_time_split().
# Split Data 80/20
splits <- initial_time_split(visits_tbl, prop = 0.8)
set.seed(1234) 

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))
## frequency = 7 observations per 1 week
## Model 2: arima_boost: Boosted Auto ARIMA (Modeltime)

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))


## Model 3: ETS: Exponential Smoothing (Modeltime)
set.seed(1234)
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))


## Model 4: prophet: Prophet (Modeltime)
set.seed(1234)
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

## Model 5: linear regression: parnsip (Modeltime)

set.seed(1234)
model_fit_lm <- linear_reg() %>%
  set_engine(engine = "lm") %>%
  fit(value ~ date, data = training(splits)) 

## Model 6: random forest: parsnip (Modeltime)

set.seed(1234) 

model_fit_RF <- rand_forest(mode="regression",trees = 500, min_n = 50) %>%
  set_engine("randomForest")  %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))


## Add fitted models to a model table.
models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_RF)

# View table
models_tbl


## Calibrate the model using testing dataset.
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits)) #Using the testing() dataset.

calibration_tbl


## Using the testing set to forecast and plot.
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = visits_tbl
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive  = TRUE)


##Forecast models accuracy evaluation. From the accuracy metrics: Model 5:
##Linear Regression (LM) is the winner with an MAE of 1147.56
# Accuracy Metrics ----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = TRUE)


# Refit trained models to actual dataset & forecast forward (1 year)
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = visits_tbl) 


refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = visits_tbl) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive  = TRUE)
