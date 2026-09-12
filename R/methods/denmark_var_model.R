##========================================================
## Vector Autoregressive Model (VAR) using R
## Original source: https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/
## Improved version with enhanced documentation and practices
##========================================================

# Clean environment
graphics.off()
rm(list = ls())

# Load required packages
suppressPackageStartupMessages({
  library(urca)      # Cointegration analysis
  library(vars)      # VAR modeling
  library(tidyverse) # Data manipulation
  library(tsDyn)     # Threshold VAR models
  library(forecast)  # Forecasting tools
})

##========================================================
## 1. DATA PREPARATION
##========================================================

# Set forecasting horizon
NHOR <- 12  # Use uppercase for constants

# Load Denmark money demand data
# Variables:
# - LRM: log of real money M2
# - LRY: log of real income
# - LPY: log of price deflator
# - IBO: bond rate
# - IDE: bank deposit rate
# Period: 1974:Q1 - 1987:Q3
data(denmark)

cat("Dataset dimensions:", dim(denmark), "\n")
cat("Available variables:", paste(colnames(denmark), collapse = ", "), "\n\n")

# Select relevant variables
df_lev <- denmark %>%
  select(LRM, LRY, IBO, IDE) %>%
  as_tibble()

m_lev <- as.matrix(df_lev)
nr_lev <- nrow(df_lev)

cat("Number of observations:", nr_lev, "\n\n")

# Create quarterly centered dummy variables
substr_q <- as.numeric(substring(denmark$ENTRY, 6, 7))

dum_season <- data.frame(
  Q2 = (substr_q == 2) - 1/4,
  Q3 = (substr_q == 3) - 1/4,
  Q4 = (substr_q == 4) - 1/4
)

##========================================================
## 2. EXPLORATORY DATA VISUALIZATION
##========================================================

# Define descriptive labels
var_labels <- c(
  "LRM = ln(real money M2)",
  "LRY = ln(real income)",
  "IBO = bond rate",
  "IDE = bank deposit rate"
)

# Plot all series
plot_time_series <- function(data, labels, dates) {
  par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
  
  for (i in 1:ncol(data)) {
    plot(data[, i], 
         type = "l", 
         col = "steelblue", 
         lwd = 2,
         main = labels[i],
         xlab = "Time",
         ylab = "Value",
         xaxt = "n")
    
    # Add x-axis with dates at regular intervals
    axis_points <- seq(1, nrow(data), length.out = 8)
    axis(1, at = axis_points, labels = dates[axis_points], las = 2)
    grid(col = "gray90")
  }
}

# Display plot (comment out if running in batch mode)
# x11(width = 12, height = 8)
# plot_time_series(m_lev, var_labels, denmark$ENTRY)

##========================================================
## 3. VAR MODEL IN LEVELS
##========================================================

cat("=== VAR Model in Levels ===\n\n")

# Convert to time series object
ts_lev <- ts(df_lev)

# Optimal lag selection
cat("Lag Selection Criteria:\n")
lag_selection <- VARselect(ts_lev, lag.max = 4, type = "const", season = 4)
print(lag_selection$selection)
cat("\n")

# Estimate VAR model
var_model_lev <- VAR(ts_lev, p = 2, type = "const", season = 4)

cat("VAR Model Summary:\n")
print(summary(var_model_lev))

# Generate forecasts
var_pred <- predict(var_model_lev, n.ahead = NHOR)

# Visualize forecasts
# x11()
# par(mai = rep(0.4, 4))
# plot(var_pred)

# x11()
# par(mai = rep(0.4, 4))
# fanchart(var_pred)

##========================================================
## 4. VAR MODEL IN FIRST DIFFERENCES (using vars package)
##========================================================

cat("\n=== VAR Model in First Differences (vars) ===\n\n")

# First difference transformation
df_diff <- diff(m_lev, lag = 1)
colnames(df_diff) <- c("dLRM", "dLRY", "dIBO", "dIDE")

# Lag selection for differenced data
cat("Lag Selection for Differenced Data:\n")
lag_selection_diff <- VARselect(df_diff, lag.max = 4, type = "const", season = 4)
print(lag_selection_diff$selection)
cat("\n")

# Estimate VAR model on differences
var_model_diff <- VAR(df_diff, p = 1, type = "const", season = 4)

# Forecast differenced data
var_forecast_diff <- predict(var_model_diff, n.ahead = NHOR)

# x11()
# par(mai = rep(0.4, 4))
# plot(var_forecast_diff)

# Convert forecasts back to levels
recover_levels <- function(level_data, diff_forecasts, horizon) {
  n_obs <- nrow(level_data)
  n_vars <- ncol(level_data)
  
  # Initialize matrix with historical data and space for forecasts
  level_forecasts <- rbind(level_data, matrix(NA, horizon, n_vars))
  
  # Extract forecast values
  forecast_matrix <- do.call(cbind, lapply(diff_forecasts$fcst, 
                                           function(x) x[, "fcst"]))
  
  # Convert differences to levels
  for (h in (n_obs + 1):(n_obs + horizon)) {
    forecast_idx <- h - n_obs
    level_forecasts[h, ] <- level_forecasts[h - 1, ] + forecast_matrix[forecast_idx, ]
  }
  
  return(level_forecasts)
}

m_var_forecast_lev <- recover_levels(m_lev, var_forecast_diff, NHOR)

# Visualize level forecasts
# x11(width = 8, height = 8)
# par(mfrow = c(4, 1), mar = c(3, 4, 3, 2))
# 
# for (i in 1:4) {
#   plot(m_var_forecast_lev[, i], 
#        type = "l", 
#        col = "steelblue", 
#        lwd = 2,
#        main = var_labels[i],
#        ylab = "Value",
#        xlab = "Time")
#   abline(v = nr_lev, col = "red", lty = 2, lwd = 2)
#   legend("topleft", legend = "Forecast Start", 
#          col = "red", lty = 2, lwd = 2, bty = "n")
# }

##========================================================
## 5. VAR MODEL IN DIFFERENCES (using tsDyn package)
##========================================================

cat("\n=== VAR Model in First Differences (tsDyn) ===\n\n")

# Estimate linear VAR with tsDyn
linevar_model_diff <- lineVar(
  data = df_lev,
  lag = 1,
  include = "const",
  model = "VAR",
  I = "diff",
  beta = NULL,
  exogen = dum_season
)

cat("tsDyn Model Coefficients:\n")
print(linevar_model_diff)

# Verify consistency between vars and tsDyn
cat("\nCoefficient comparison (vars package):\n")
vars_coefs <- do.call(rbind, lapply(var_model_diff$varresult, 
                                    function(x) x$coefficients))
print(round(vars_coefs, 4))

# Create seasonal dummies for forecast period
dumf_season <- rbind(
  tail(dum_season, 4),
  tail(dum_season, 4),
  tail(dum_season, 4)
)

# Generate forecasts
linevar_forecast <- predict(linevar_model_diff, 
                            n.ahead = NHOR,
                            exoPred = dumf_season)

# Combine historical and forecast data
df_with_forecast <- rbind(df_lev, linevar_forecast)

# Visualize results
# x11(width = 8, height = 8)
# par(mfrow = c(4, 1), mar = c(3, 4, 3, 2))
# 
# for (i in 1:4) {
#   plot(df_with_forecast[, i],
#        type = "l",
#        col = "steelblue",
#        lwd = 2,
#        main = var_labels[i],
#        ylab = "Value",
#        xlab = "Time")
#   abline(v = nr_lev, col = "red", lty = 2, lwd = 2)
#   legend("topleft", legend = "Forecast Start",
#          col = "red", lty = 2, lwd = 2, bty = "n")
# }

##========================================================
## 6. MODEL DIAGNOSTICS AND COMPARISON
##========================================================

cat("\n=== Model Diagnostics ===\n\n")

# Serial correlation test
cat("Serial Correlation Test (Portmanteau):\n")
serial_test <- serial.test(var_model_lev, lags.pt = 10, type = "PT.asymptotic")
print(serial_test)

# Heteroskedasticity test
cat("\nHeteroskedasticity Test (ARCH):\n")
arch_test <- arch.test(var_model_lev, lags.multi = 5)
print(arch_test)

# Normality test
cat("\nNormality Test:\n")
norm_test <- normality.test(var_model_lev)
print(norm_test)

# Stability analysis
cat("\nStability Analysis:\n")
stability <- stability(var_model_lev)
print(stability)

cat("\n=== Analysis Complete ===\n")

##========================================================
## 7. HELPER FUNCTIONS FOR EXPORT
##========================================================

# Function to extract and format forecasts
extract_forecasts <- function(forecast_obj, variables) {
  forecast_df <- lapply(seq_along(variables), function(i) {
    fc <- forecast_obj$fcst[[i]]
    data.frame(
      Variable = variables[i],
      Horizon = 1:nrow(fc),
      Forecast = fc[, "fcst"],
      Lower = fc[, "lower"],
      Upper = fc[, "upper"],
      CI = fc[, "CI"]
    )
  })
  
  do.call(rbind, forecast_df)
}

# Example usage:
# forecast_summary <- extract_forecasts(var_pred, colnames(df_lev))
# write.csv(forecast_summary, "var_forecasts.csv", row.names = FALSE)

##========================================================
## END OF SCRIPT
##========================================================
