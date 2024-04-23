#https://rpubs.com/nomarpicasso/1151428
library(tidyverse)
library(modeltime)
library(xgboost)
library(tidymodels)
library(timetk)

bike_transactions_daily <- read_csv("data/day.csv")
glimpse(bike_transactions_daily)

bike_transactions_tbl <- bike_sharing_daily %>%
  select(date = dteday, value = cnt)

glimpse(bike_transactions_tbl)

# Plot data set 

bike_transactions_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Bike Sharing Transactions",
       x = "Date",
       y = "Transactions") +
  theme_minimal()

bike_transactions_tbl %>%
  plot_time_series(date, value, .interactive = TRUE)

# Split data set into training and testing sets
#Let’s split the data into training and test sets using initial_time_split().
# Split Data 80/20
splits <- initial_time_split(bike_transactions_tbl, prop = 0.8)
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
    actual_data = bike_transactions_tbl
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
  modeltime_refit(data = bike_transactions_tbl) 


refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive  = TRUE)



# Train and evaluate models {background-color="#CA225E"}

## Why tidymodels? 

## Why tidymodels?  *Consistency*

. . .

:::: {.columns}

::: {.column width="50%"}

With `lm()`:
  
  ```{r eng-lm-model}
#| echo: true
#| eval: false
model <- 
  lm(mpg ~ ., mtcars)

```

:::
  
  ::: {.column width="50%"}

With tidymodels:
  
  ```{r tm-lm}
#| echo: true
#| eval: false
#| code-line-numbers: "|3"
model <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ ., mtcars)
```


:::
  
  ::::
  
  ## Why tidymodels?  *Consistency*
  
  :::: {.columns}

::: {.column width="50%"}

With glmnet:
  
  ```{r eng-glmnet-model}
#| echo: true
#| eval: false
model <- 
  glmnet(
    as.matrix(mtcars[2:11]),
    mtcars$mpg
  )
```

:::
  
  ::: {.column width="50%"}

With tidymodels:
  
  ```{r tm-glmnet}
#| echo: true
#| eval: false
#| code-line-numbers: "3||3"
model <-
  linear_reg() %>%
  set_engine("glmnet") %>%
  fit(mpg ~ ., mtcars)
```


:::
  
  ::::
  
  
