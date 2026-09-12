pacman::p_load(
  tidyverse, fpp3, tsibble, feasts, forecast, dygraphs, xts, zoo, janitor,
  knitr, kableExtra, tseries, gridExtra, slider, readxl
)



# Set theme for ggplot
theme_set(theme_minimal(base_size = 12))

# Load the dataset excel file

kse_raw <- read_excel("D:/RepTemplates/timeseries/data/kse_oct25.xlsx")%>% 
  clean_names() |> select(date, open, high, low, close, change)


# Display first few rows
head(kse_raw) %>%
  kable(caption = "First 6 rows of raw KSE-100 data") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


kse <- kse_raw %>%
  mutate(date = as.Date(date))




# Create tsibble object (tidy time series)
kse_tsbl <- kse %>%
  as_tsibble(index = date)

# Summary statistics
summary(kse) %>%
  kable(caption = "Summary Statistics of KSE-100 Variables") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Display structure
glimpse(kse_tsbl)

# Check for gaps in the time series
has_gaps(kse_tsbl)

# Count observations by year
kse_tsbl %>%
  mutate(year = year(date)) %>%
  count(year) %>%
  ggplot(aes(x = factor(year), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Number of Trading Days per Year",
       x = "Year", y = "Count") +
  theme_minimal()

# Static time plot
kse_tsbl %>%
  autoplot(open) +
  labs(title = "KSE-100 Index: Closing Prices",
       y = "Closing Price",
       x = "Date") +
  theme_minimal()

# Interactive time plot
kse_xts <- xts(kse$open, order.by = kse$date)
dygraph(kse_xts, main = "KSE-100 Index (Interactive)") %>%
  dyAxis("y", label = "Close Price") %>%
  dyRangeSelector() %>%
  dyOptions(colors = "#2C3E50", strokeWidth = 1.5)

# Plot all price variables
kse_tsbl %>%
  select(date, open, high, low) %>%
  pivot_longer(cols = -date, names_to = "price_type", values_to = "price") %>%
  ggplot(aes(x = date, y = price, color = price_type)) +
  geom_line(alpha = 0.7) +
  labs(title = "KSE-100: All Price Types",
       y = "Price", x = "Date",
       color = "Price Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate daily returns
kse_returns <- kse_tsbl %>%
  mutate(
    daily_return = (open - lag(open)) / lag(open) * 100,
    abs_return = abs(daily_return)
  )

# Plot returns
kse_returns %>%
  ggplot(aes(x = date, y = daily_return)) +
  geom_line(color = "darkblue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "KSE-100 Daily Returns",
       y = "Daily Return (%)",
       x = "Date") +
  theme_minimal()

# Distribution of returns
kse_returns %>%
  ggplot(aes(x = daily_return)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Daily Returns",
       x = "Daily Return (%)",
       y = "Frequency") +
  theme_minimal()

# Volume over time
kse_tsbl %>%
  ggplot(aes(x = date, y = close)) +
  geom_line(color = "darkgreen", alpha = 0.7) +
  labs(title = "KSE-100 Trading Volume",
       y = "Volume",
       x = "Date") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Price vs Volume relationship
kse_tsbl %>%
  ggplot(aes(x = close, y = open)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Price vs Volume Relationship",
       x = "Volume",
       y = "Close Price") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

# Add time features
kse_seasonal <- kse_tsbl %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE),
    week = week(date),
    wday = wday(date, label = TRUE)
  )

# Monthly patterns
kse_seasonal %>%
  ggplot(aes(x = month, y = open, group = year, color = factor(year))) +
  geom_line(alpha = 0.6) +
  labs(title = "KSE-100: Seasonal Pattern by Month",
       x = "Month", y = "Close Price",
       color = "Year") +
  theme_minimal() +
  theme(legend.position = "right")


# Day of week analysis
weekday_summary <- kse_returns %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(
    avg_return = mean(daily_return, na.rm = TRUE),
    sd_return = sd(daily_return, na.rm = TRUE),
    n = n()
  )

weekday_summary %>%
  ggplot(aes(x = wday, y = avg_return)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_return - sd_return, 
                    ymax = avg_return + sd_return),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Average Daily Returns by Day of Week",
       x = "Day of Week",
       y = "Average Return (%)") +
  theme_minimal()

#weekday_summary %>%
 ##      col.names = c("Day", "Avg Return (%)", "Std Dev", "Count"),
   #     digits = 3) %>%
  #able_styling(bootstrap_options = c("striped", "hover"))

# Aggregate to monthly data
kse_monthly <- kse_tsbl %>%
  mutate(year_month = yearmonth(date)) %>%
  as_tibble() %>%  # Convert to tibble first to remove tsibble structure
  group_by(year_month) %>%
  summarise(
    avg_open = mean(open, na.rm = TRUE),
    max_open = max(open, na.rm = TRUE),
    min_open = min(open, na.rm = TRUE),
    total_vol = sum(close, na.rm = TRUE),
    .groups = "drop"  # Automatically drop grouping
  ) %>%
  as_tsibble(index = year_month)

# Plot monthly average
kse_monthly %>%
  autoplot(avg_open) +
  labs(title = "KSE-100: Monthly Average Closing Price",
       y = "Average open Price",
       x = "Month") +
  theme_minimal()

# Check if we have enough data for classical decomposition
n_months <- nrow(kse_monthly)
cat("Number of months available:", n_months, "\n")

# Classical decomposition requires at least 2 complete periods (24 months)
if (n_months >= 24) {
  # Monthly data decomposition
  dcmp_classical <- kse_monthly %>%
    model(
      classical = classical_decomposition(avg_open, type = "additive")
    )
  
  # Plot components
  components(dcmp_classical) %>%
    autoplot() +
    labs(title = "Classical Decomposition: KSE-100 Monthly Average") +
    theme_minimal()
  
  # Extract components
  components(dcmp_classical) %>%
    as_tibble() %>%
    head(12) %>%
    kable(caption = "First 12 months of decomposed components",
          digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  cat("⚠️ Not enough data for classical decomposition.\n")
  cat("Need at least 24 months, have", n_months, "months.\n\n")
  cat("Using a simple trend extraction instead:\n\n")
  
  # Alternative: simple moving average trend
  kse_monthly %>%
    mutate(
      trend = slider::slide_dbl(avg_open, mean, 
                                .before = 2, .after = 2, .complete = TRUE)
    ) %>%
    pivot_longer(cols = c(avg_open, trend), 
                 names_to = "component", 
                 values_to = "value") %>%
    ggplot(aes(x = year_month, y = value, color = component)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("avg_open" = "gray60", "trend" = "darkblue"),
                      labels = c("avg_open" = "Original", "trend" = "Trend (5-MA)")) +
    labs(title = "KSE-100: Original Series with Trend",
         y = "Average open Price",
         x = "Month",
         color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# STL decomposition also requires sufficient data
if (n_months >= 24) {
  # STL decomposition
  dcmp_stl <- kse_monthly %>%
    model(
      stl = STL(avg_open ~ season(window = "periodic"))
    )
  
  # Plot STL components
  components(dcmp_stl) %>%
    autoplot() +
    labs(title = "STL Decomposition: KSE-100 Monthly Average") +
    theme_minimal()
  
  # Seasonally adjusted series
  kse_monthly %>%
    autoplot(avg_open, color = "gray") +
    autolayer(components(dcmp_stl), season_adjust, color = "blue") +
    labs(title = "KSE-100: Original vs Seasonally Adjusted",
         y = "Average open Price",
         x = "Month") +
    theme_minimal()
} else {
  cat("⚠️ Not enough data for STL decomposition.\n")
  cat("Using alternative trend-cycle extraction:\n\n")
  
  # Use a simple loess smoother for trend
  kse_monthly %>%
    mutate(
      trend = predict(loess(avg_open ~ as.numeric(year_month), 
                           data = ., span = 0.3))
    ) %>%
    ggplot(aes(x = year_month)) +
    geom_line(aes(y = avg_open), color = "gray60", alpha = 0.7) +
    geom_line(aes(y = trend), color = "darkred", size = 1.2) +
    labs(title = "KSE-100: Original Series with LOESS Trend",
         y = "Average open Price",
         x = "Month") +
    theme_minimal()
}

# Extract and analyze trend
if (exists("dcmp_stl") && n_months >= 24) {
  trend_data <- components(dcmp_stl) %>%
    as_tibble() %>%
    select(year_month, trend)
  
  # Plot trend with confidence interval
  ggplot(trend_data, aes(x = year_month, y = trend)) +
    geom_line(color = "darkblue", size = 1) +
    geom_smooth(method = "loess", color = "red", fill = "pink", alpha = 0.3) +
    labs(title = "KSE-100 Trend Component with Smoothed Trend",
         y = "Trend",
         x = "Month") +
    theme_minimal()
  
  # Calculate trend statistics
  trend_stats <- trend_data %>%
    summarise(
      min_trend = min(trend, na.rm = TRUE),
      max_trend = max(trend, na.rm = TRUE),
      avg_trend = mean(trend, na.rm = TRUE),
      trend_range = max_trend - min_trend,
      pct_change = (max_trend - min_trend) / min_trend * 100
    )
  
  trend_stats %>%
    kable(caption = "Trend Component Statistics",
          digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  # Alternative trend analysis using moving average
  kse_monthly %>%
    mutate(
      ma_3 = slider::slide_dbl(avg_open, mean, .before = 1, .after = 1, .complete = TRUE),
      ma_6 = slider::slide_dbl(avg_open, mean, .before = 2, .after = 2, .complete = TRUE)
    ) %>%
    pivot_longer(cols = c(avg_open, ma_3, ma_6),
                 names_to = "series",
                 values_to = "value") %>%
    ggplot(aes(x = year_month, y = value, color = series)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = c("avg_open" = "gray50", "ma_3" = "blue", "ma_6" = "darkred"),
      labels = c("avg_open" = "Original", "ma_3" = "3-Month MA", "ma_6" = "6-Month MA")
    ) +
    labs(title = "KSE-100: Trend Extraction using Moving Averages",
         y = "Average open Price",
         x = "Month",
         color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Simple trend statistics
  kse_monthly %>%
    summarise(
      first_value = first(avg_open),
      last_value = last(avg_open),
      min_value = min(avg_open),
      max_value = max(avg_open),
      total_change = last_value - first_value,
      pct_change = (last_value - first_value) / first_value * 100
    ) %>%
    kable(caption = "Overall Trend Statistics",
          digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

# Note: Stock market data has irregular gaps (weekends, holidays)
# We need to use fill_gaps() or work with the index directly

# ACF plot for closing prices
kse_tsbl %>%
  fill_gaps() %>%  # Fill gaps with NA
  ACF(open, lag_max = 50) %>%
  autoplot() +
  labs(title = "ACF: KSE-100 Closing Prices",
       subtitle = "Strong persistence indicates non-stationarity") +
  theme_minimal()

# Alternative: Use acf() from stats package with regular spacing
acf_open <- acf(kse$open, lag.max = 50, plot = FALSE)
autoplot(acf_open) +
  labs(title = "ACF: KSE-100 Closing Prices (Alternative Method)",
       x = "Lag", y = "ACF") +
  theme_minimal()

# ACF for returns
kse_returns %>%
  fill_gaps() %>%
  ACF(daily_return, lag_max = 50) %>%
  autoplot() +
  labs(title = "ACF: Daily Returns",
       subtitle = "Rapid decay suggests openr to stationarity") +
  theme_minimal()

# Alternative for returns
returns_clean <- na.omit(kse_returns$daily_return)
acf_returns <- acf(returns_clean, lag.max = 50, plot = FALSE)
autoplot(acf_returns) +
  labs(title = "ACF: Daily Returns (Alternative Method)",
       x = "Lag", y = "ACF") +
  theme_minimal()

# PACF plot for closing prices
kse_tsbl %>%
  fill_gaps() %>%
  PACF(open, lag_max = 50) %>%
  autoplot() +
  labs(title = "PACF: KSE-100 Closing Prices",
       subtitle = "High lag-1 value indicates strong AR component") +
  theme_minimal()

# Alternative method
pacf_open <- pacf(kse$open, lag.max = 50, plot = FALSE)
autoplot(pacf_open) +
  labs(title = "PACF: KSE-100 Closing Prices (Alternative Method)",
       x = "Lag", y = "PACF") +
  theme_minimal()

# PACF for returns
kse_returns %>%
  fill_gaps() %>%
  PACF(daily_return, lag_max = 50) %>%
  autoplot() +
  labs(title = "PACF: Daily Returns",
       subtitle = "Values within confidence bands suggest white noise") +
  theme_minimal()

# Alternative for returns
pacf_returns <- pacf(returns_clean, lag.max = 50, plot = FALSE)
autoplot(pacf_returns) +
  labs(title = "PACF: Daily Returns (Alternative Method)",
       x = "Lag", y = "PACF") +
  theme_minimal()

# Ljung-Box test for returns (using filled gaps)
kse_returns_filled <- kse_returns %>% 
  fill_gaps() %>%
  fill(daily_return, .direction = "down")  # Fill NA with previous value

lb_test <- kse_returns_filled %>%
  features(daily_return, ljung_box, lag = 20, dof = 0)

lb_test %>%
  kable(caption = "Ljung-Box Test Results for Daily Returns",
        digits = 4,
        col.names = c("Statistic", "P-value")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Interpretation
cat("\n**Test Result:**\n\n")
if(lb_test$lb_pvalue < 0.05) {
  cat("✓ The returns show significant autocorrelation (p < 0.05)\n")
  cat("  → There is predictable structure in returns\n")
  cat("  → ARIMA models may be appropriate\n")
} else {
  cat("✗ No significant autocorrelation in returns (p ≥ 0.05)\n")
  cat("  → Returns appear to be white noise\n")
  cat("  → Market may be informationally efficient\n")
}

# Additional test on squared returns (test for ARCH effects)
kse_returns_sq <- kse_returns_filled %>%
  mutate(sq_return = daily_return^2)

lb_test_sq <- kse_returns_sq %>%
  features(sq_return, ljung_box, lag = 20, dof = 0)

cat("\n\n**Ljung-Box Test on Squared Returns:**\n\n")
lb_test_sq %>%
  kable(caption = "Testing for Volatility Clustering (ARCH Effects)",
        digits = 4,
        col.names = c("Statistic", "P-value")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

if(lb_test_sq$lb_pvalue < 0.05) {
  cat("\n✓ Significant autocorrelation in squared returns\n")
  cat("  → Volatility clustering present\n")
  cat("  → GARCH models may be appropriate\n")
}

# Plot with rolling mean and std
kse_rolling <- kse_tsbl %>%
  mutate(
    roll_mean = slider::slide_dbl(open, mean, .before = 29, .complete = TRUE),
    roll_sd = slider::slide_dbl(open, sd, .before = 29, .complete = TRUE)
  )

# Rolling mean
ggplot(kse_rolling, aes(x = date)) +
  geom_line(aes(y = open), alpha = 0.5, color = "gray") +
  geom_line(aes(y = roll_mean), color = "blue", size = 1) +
  labs(title = "KSE-100 with 30-Day Rolling Mean",
       y = "Price", x = "Date") +
  theme_minimal()

# Rolling standard deviation
ggplot(kse_rolling, aes(x = date, y = roll_sd)) +
  geom_line(color = "darkred", size = 1) +
  labs(title = "30-Day Rolling Standard Deviation",
       y = "Standard Deviation", x = "Date") +
  theme_minimal()

# ADF test function
adf_test <- function(x) {
  test <- tseries::adf.test(x, alternative = "stationary")
  tibble(
    statistic = test$statistic,
    p_value = test$p.value,
    conclusion = ifelse(test$p.value < 0.05, "Stationary", "Non-Stationary")
  )
}

# KPSS test function
kpss_test <- function(x) {
  test <- tseries::kpss.test(x)
  tibble(
    statistic = test$statistic,
    p_value = test$p.value,
    conclusion = ifelse(test$p.value < 0.05, "Non-Stationary", "Stationary")
  )
}

# Test closing prices
cat("Tests for Closing Prices:\n\n")
adf_open <- adf_test(na.omit(kse$open))
kpss_open <- kpss_test(na.omit(kse$open))

bind_rows(
  adf_open %>% mutate(test = "ADF"),
  kpss_open %>% mutate(test = "KPSS")
) %>%
  select(test, everything()) %>%
  kable(caption = "Stationarity Tests: Closing Prices",
        digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Test returns
cat("\nTests for Daily Returns:\n\n")
returns_clean <- na.omit(kse_returns$daily_return)
adf_returns <- adf_test(returns_clean)
kpss_returns <- kpss_test(returns_clean)

bind_rows(
  adf_returns %>% mutate(test = "ADF"),
  kpss_returns %>% mutate(test = "KPSS")
) %>%
  select(test, everything()) %>%
  kable(caption = "Stationarity Tests: Daily Returns",
        digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# First difference
kse_diff <- kse_tsbl %>%
  mutate(diff_open = difference(open))

# Plot original and differenced series
p1 <- kse_tsbl %>%
  autoplot(open) +
  labs(title = "Original Series", y = "open Price") +
  theme_minimal()

p2 <- kse_diff %>%
  autoplot(diff_open) +
  labs(title = "First Difference", y = "Differenced Price") +
  theme_minimal()

gridExtra::grid.arrange(p1, p2, ncol = 1)

# Test differenced series
diff_clean <- na.omit(kse_diff$diff_open)
cat("Stationarity tests for differenced series:\n\n")

bind_rows(
  adf_test(diff_clean) %>% mutate(test = "ADF"),
  kpss_test(diff_clean) %>% mutate(test = "KPSS")
) %>%
  select(test, everything()) %>%
  kable(caption = "Stationarity Tests: First Difference",
        digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Use last 30 days as test set
train <- kse_monthly %>% slice(1:(n()-6))
test <- kse_monthly %>% slice((n()-5):n())

# Fit benchmark models
fit_bench <- train %>%
  model(
    Mean = MEAN(avg_open),
    Naive = NAIVE(avg_open),
    Drift = RW(avg_open ~ drift()),
    SNaive = SNAIVE(avg_open)
  )

# Generate forecasts
fc_bench <- fit_bench %>% forecast(h = 6)

# Plot forecasts
fc_bench %>%
  autoplot(train, level = NULL) +
  autolayer(test, avg_open, color = "black") +
  labs(title = "Benchmark Forecasts: KSE-100 Monthly",
       y = "Average open Price",
       x = "Month") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Accuracy
accuracy(fc_bench, test) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  arrange(RMSE) %>%
  kable(caption = "Benchmark Model Accuracy",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Fit ETS model
fit_ets <- train %>%
  model(
    auto_ets = ETS(avg_open)
  )

# Report model
report(fit_ets)

# Forecast
fc_ets <- fit_ets %>% forecast(h = 6)

# Plot
fc_ets %>%
  autoplot(train) +
  autolayer(test, avg_open, color = "black") +
  labs(title = "ETS Forecast: KSE-100 Monthly",
       y = "Average open Price") +
  theme_minimal()

# Accuracy
accuracy(fc_ets, test) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  kable(caption = "ETS Model Accuracy",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Fit ARIMA model
fit_arima <- train %>%
  model(
    auto_arima = ARIMA(avg_open, stepwise = FALSE, approximation = FALSE)
  )

# Report model
report(fit_arima)

# Forecast
fc_arima <- fit_arima %>% forecast(h = 6)

# Plot
fc_arima %>%
  autoplot(train) +
  autolayer(test, avg_open, color = "black") +
  labs(title = "ARIMA Forecast: KSE-100 Monthly",
       y = "Average open Price") +
  theme_minimal()

# Residual diagnostics
fit_arima %>%
  gg_tsresiduals() +
  labs(title = "Residual Diagnostics: ARIMA Model")

# Accuracy
accuracy(fc_arima, test) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  kable(caption = "ARIMA Model Accuracy",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Compare all models
all_fc <- bind_rows(
  fc_bench,
  fc_ets,
  fc_arima
)

# Plot comparison
all_fc %>%
  autoplot(train, level = NULL) +
  autolayer(test, avg_open, color = "black", size = 1.2) +
  labs(title = "Forecast Comparison: All Models",
       y = "Average open Price",
       x = "Month") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Accuracy comparison
accuracy(all_fc, test) %>%
  select(.model, RMSE, MAE, MAPE, MASE) %>%
  arrange(RMSE) %>%
  mutate(rank = row_number()) %>%
  kable(caption = "Model Accuracy Comparison (Ranked by RMSE)",
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1, bold = TRUE, background = "#e8f5e9")

# Calculate realized volatility
kse_vol <- kse_returns %>%
  mutate(
    sq_return = daily_return^2,
    abs_return = abs(daily_return)
  ) %>%
  mutate(
    realized_vol = sqrt(slider::slide_dbl(sq_return, mean, 
                                          .before = 19, .complete = TRUE)) * sqrt(252)
  )

# Plot volatility
kse_vol %>%
  ggplot(aes(x = date, y = realized_vol)) +
  geom_line(color = "darkred", alpha = 0.7) +
  labs(title = "Realized Volatility (20-Day, Annualized)",
       y = "Volatility (%)",
       x = "Date") +
  theme_minimal()

# Volatility clustering visualization
kse_vol %>%
  ggplot(aes(x = date, y = abs_return)) +
  geom_line(alpha = 0.5, color = "gray50") +
  geom_smooth(span = 0.1, color = "darkred", se = FALSE) +
  labs(title = "Volatility Clustering in Daily Returns",
       y = "Absolute Return (%)",
       x = "Date") +
  theme_minimal()

# Calculate multiple moving averages
kse_ma <- kse_tsbl %>%
  mutate(
    ma_20 = slider::slide_dbl(open, mean, .before = 19, .complete = TRUE),
    ma_50 = slider::slide_dbl(open, mean, .before = 49, .complete = TRUE),
    ma_200 = slider::slide_dbl(open, mean, .before = 199, .complete = TRUE)
  )

# Plot with moving averages
kse_ma %>%
  filter(date >= max(date) - years(2)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = open), color = "gray50", alpha = 0.5) +
  geom_line(aes(y = ma_20, color = "20-Day MA"), size = 1) +
  geom_line(aes(y = ma_50, color = "50-Day MA"), size = 1) +
  geom_line(aes(y = ma_200, color = "200-Day MA"), size = 1) +
  scale_color_manual(values = c("20-Day MA" = "blue", 
                                "50-Day MA" = "red",
                                "200-Day MA" = "darkgreen")) +
  labs(title = "KSE-100 with Moving Averages (Last 2 Years)",
       y = "open Price", x = "Date",
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Extract time series features
kse_features <- kse_monthly %>%
  features(avg_open, feature_set(pkgs = "feasts"))

# Display key features
kse_features %>%
  select(trend_strength, seasonal_strength_year, 
         acf1, pacf1, diff1_acf1) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
  kable(caption = "Key Time Series Features",
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Spectral analysis
kse_tsbl %>%
  gg_season(open, period = "year") +
  labs(title = "Seasonal Plot by Year") +
  theme_minimal()

# Create summary table (using previous accuracy results)
tibble(
  Model = c("Benchmark (Naive)", "ETS", "ARIMA"),
  `Best For` = c("Simple baseline", "Automatic selection", "Custom tuning"),
  Strengths = c("Simple, interpretable", "Handles trend/seasonality", "Flexible specification"),
  Limitations = c("No trend capture", "Can overfit", "Requires expertise")
) %>%
  kable(caption = "Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# # Complete KSE-100 Time Series Analysis Workflow
# # ================================================
# 
# # 1. SETUP
# library(tidyverse)
# library(fpp3)
# library(tsibble)
# library(feasts)
# library(forecast)
# library(dygraphs)
# library(xts)
# library(zoo)
# library(janitor)
# 
# # 2. DATA LOADING
# kse_raw <- read_csv("docs/data/kse_2025.csv") %>%
#   clean_names()
# 
# # 3. DATA PREPARATION
# kse <- kse_raw %>%
#   mutate(
#     date = as.Date(date, format = "%d-%b-%y"),
#     change_percent = as.numeric(gsub("%", "", change_percent)),
#     vol = as.numeric(gsub(",", "", vol))
#   ) %>%
#   select(date, open, high, low, close, vol, change_percent) %>%
#   arrange(date) %>%
#   drop_na()
# 
# kse_tsbl <- kse %>% as_tsibble(index = date)
# 
# # 4. EXPLORATORY ANALYSIS
# kse_tsbl %>% autoplot(close)
# kse_tsbl %>% ACF(close) %>% autoplot()
# kse_tsbl %>% PACF(close) %>% autoplot()
# 
# # 5. RETURNS CALCULATION
# kse_returns <- kse_tsbl %>%
#   mutate(daily_return = (close - lag(close)) / lag(close) * 100)
# 
# # 6. DECOMPOSITION
# kse_monthly <- kse_tsbl %>%
#   mutate(year_month = yearmonth(date)) %>%
#   group_by(year_month) %>%
#   summarise(avg_close = mean(close)) %>%
#   as_tsibble(index = year_month)
# 
# dcmp <- kse_monthly %>%
#   model(stl = STL(avg_close))
# 
# components(dcmp) %>% autoplot()
# 
# # 7. MODELING
# train <- kse_monthly %>% slice(1:(n()-6))
# test <- kse_monthly %>% slice((n()-5):n())
# 
# fit <- train %>%
#   model(
#     naive = NAIVE(avg_close),
#     ets = ETS(avg_close),
#     arima = ARIMA(avg_close)
#   )
# 
# # 8. FORECASTING
# fc <- fit %>% forecast(h = 6)
# 
# # 9. EVALUATION
# accuracy(fc, test)
# 
# # 10. VISUALIZATION
# fc %>%
#   autoplot(train) +
#   autolayer(test, avg_close)

tibble(
  Variable = c("date", "open", "high", "low", "close", "vol", "change_percent"),
  Description = c(
    "Trading date (YYYY-MM-DD format)",
    "Opening price at market open",
    "Highest price during trading day",
    "Lowest price during trading day",
    "Closing price at market close",
    "Total trading volume",
    "Daily percentage change in closing price"
  ),
  Type = c("Date", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric", "Numeric"),
  Unit = c("-", "Index Points", "Index Points", "Index Points", 
           "Index Points", "Shares", "Percent")
) %>%
  kable(caption = "KSE-100 Data Dictionary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# # Calculate multiple moving averages
# calc_moving_averages <- function(data, price_col, windows = c(20, 50, 200)) {
#   for(w in windows) {
#     col_name <- paste0("ma_", w)
#     data <- data %>%
#       mutate(!!col_name := slider::slide_dbl(
#         .data[[price_col]], mean,
#         .before = w-1, .complete = TRUE
#       ))
#   }
#   return(data)
# }
# 
# # Calculate volatility
# calc_volatility <- function(returns, window = 20, annualize = TRUE) {
#   vol <- sqrt(slider::slide_dbl(
#     returns^2, mean,
#     .before = window-1, .complete = TRUE
#   ))
# 
#   if(annualize) {
#     vol <- vol * sqrt(252)  # Assuming 252 trading days
#   }
# 
#   return(vol)
# }
# 
# # Create interactive time series plot
# plot_interactive_ts <- function(data, date_col, value_col, title = "") {
#   xts_data <- xts(data[[value_col]], order.by = data[[date_col]])
# 
#   dygraph(xts_data, main = title) %>%
#     dyRangeSelector() %>%
#     dyOptions(colors = "#2C3E50", strokeWidth = 1.5)
# }
# 
# # Rolling window cross-validation
# rolling_cv <- function(data, model_fn, h = 1, window = 100) {
#   n <- nrow(data)
#   errors <- numeric(n - window - h + 1)
# 
#   for(i in seq_along(errors)) {
#     train <- data[i:(i+window-1), ]
#     test <- data[i+window+h-1, ]
# 
#     fit <- model_fn(train)
#     fc <- forecast(fit, h = h)
#     errors[i] <- test$value - fc$mean[h]
#   }
# 
#   return(errors)
# }
# 
# # Calculate performance metrics
# calc_metrics <- function(actual, predicted) {
#   errors <- actual - predicted
# 
#   tibble(
#     RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
#     MAE = mean(abs(errors), na.rm = TRUE),
#     MAPE = mean(abs(errors/actual) * 100, na.rm = TRUE),
#     MASE = mean(abs(errors), na.rm = TRUE) /
#            mean(abs(diff(actual)), na.rm = TRUE)
#   )
# }


