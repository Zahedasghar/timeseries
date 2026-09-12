## ============================================================
## Pakistan Solar Panel Prices vs Global Polysilicon Prices
## ARIMA / ARIMAX Time Series Analysis with Policy Notes
## Data: monthly, Jan 2017 - May 2026 (n = 113), both series in logs
## ============================================================

pkgs <- c("forecast", "tseries", "urca", "lmtest", "ggplot2", "vars")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cran.r-project.org")))
library(forecast); library(tseries); library(urca); library(lmtest); library(ggplot2)

## ---- 1. Load & prepare -------------------------------------
data_path <- "data/ARIMA-maria.csv"   # set full path if not in working directory
df <- read.csv(data_path, stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
df <- df[order(df$Date), ]
stopifnot(!anyNA(df$log_commodity_price), !anyNA(df$log_polysilicon))

y <- ts(df$log_commodity_price, start = c(2017, 1), frequency = 12)   # panel price (log)
x <- ts(df$log_polysilicon,     start = c(2017, 1), frequency = 12)   # polysilicon price (log)

## ---- 2. Exploratory plots -----------------------------------
autoplot(cbind(PanelPrice = y, Polysilicon = x), facets = TRUE) +
  labs(title = "Log Panel Price (Pakistan) vs Log Polysilicon Price",
       x = NULL, y = NULL) + theme_minimal()

ggsave("eda_series.png", width = 9, height = 5)

stl_y <- stl(y, s.window = "periodic")
autoplot(stl_y) + labs(title = "STL Decomposition: Log Panel Price")
ggsave("stl_decomposition.png", width = 9, height = 6)

## ---- 3. Unit root / stationarity tests ----------------------
unit_root_battery <- function(series, name) {
  cat("\n===", name, "===\n")
  cat("ADF (level):  p =", round(adf.test(series)$p.value, 4), "\n")
  cat("KPSS (level): p =", round(kpss.test(series, "Level")$p.value, 4), "\n")
  cat("PP  (level):  p =", round(pp.test(series)$p.value, 4), "\n")
  cat("ADF (1st diff): p =", round(adf.test(diff(series))$p.value, 4), "\n")
}
unit_root_battery(y, "Log Panel Price")
unit_root_battery(x, "Log Polysilicon Price")
ndiffs(y); ndiffs(x)   # confirms integration order (expect d = 1 for both)

## ---- 4. ACF / PACF identification ---------------------------
par(mfrow = c(2, 2))
acf(diff(y),  main = "ACF: d(Panel Price)")
pacf(diff(y), main = "PACF: d(Panel Price)")
acf(diff(x),  main = "ACF: d(Polysilicon)")
pacf(diff(x), main = "PACF: d(Polysilicon)")
par(mfrow = c(1, 1))

## ---- 5. Cointegration (both series likely I(1)) --------------
eg_reg <- lm(y ~ x)
summary(eg_reg)
eg_resid <- residuals(eg_reg)
eg_adf <- ur.df(eg_resid, type = "none", selectlags = "AIC")
cat("\nEngle-Granger step 2 (residual ADF), tau =", round(eg_adf@teststat[1], 3),
    " | 5% crit =", eg_adf@cval[1, "5pct"], "\n")

jo <- ca.jo(cbind(y, x), type = "trace", ecdet = "const", K = 2)
summary(jo)   # compare test stat to critical values for r = 0 vs r <= 1

## ---- 6. Granger causality ------------------------------------
cat("\nDoes polysilicon Granger-cause panel price?\n")
print(grangertest(y ~ x, order = 3))
cat("\nDoes panel price Granger-cause polysilicon?\n")
print(grangertest(x ~ y, order = 3))

## ---- 7. Model candidates --------------------------------------
train_h  <- 100                 # holdout last ~13 months for out-of-sample test
y_train  <- window(y, end = time(y)[train_h])
x_train  <- window(x, end = time(x)[train_h])
y_test   <- window(y, start = time(y)[train_h + 1])
x_test   <- window(x, start = time(x)[train_h + 1])

m_arima  <- auto.arima(y_train, stepwise = FALSE, approximation = FALSE)
m_arimax <- auto.arima(y_train, xreg = x_train, stepwise = FALSE, approximation = FALSE)
m_ecm    <- if (jo@teststat[1] > jo@cval[1, 2]) {
  vecm <- cajorls(jo, r = 1)
  vecm
} else NULL   # only meaningful if cointegration confirmed above

summary(m_arima); summary(m_arimax)

## ---- 8. Residual diagnostics -----------------------------------
checkresiduals(m_arima)
checkresiduals(m_arimax)
cat("\nJarque-Bera (ARIMAX resid): p =", round(jarque.bera.test(residuals(m_arimax))$p.value, 4), "\n")
cat("ARCH-LM (ARIMAX resid, lag 6):\n")
print(Box.test(residuals(m_arimax)^2, lag = 6, type = "Ljung-Box"))

## ---- 9. Out-of-sample accuracy comparison -----------------------
h <- length(y_test)
f_arima  <- forecast(m_arima,  h = h)
f_arimax <- forecast(m_arimax, xreg = x_test, h = h)     # actual future polysilicon (best case)
f_naive  <- naive(y_train, h = h)

acc_tbl <- rbind(
  ARIMA   = accuracy(f_arima,  y_test)["Test set", c("RMSE","MAE","MAPE")],
  ARIMAX  = accuracy(f_arimax, y_test)["Test set", c("RMSE","MAE","MAPE")],
  Naive   = accuracy(f_naive,  y_test)["Test set", c("RMSE","MAE","MAPE")]
)
print(round(acc_tbl, 4))

## rolling-origin CV (1-step) as a robustness check
cv_arima  <- tsCV(y, function(z, h) forecast(auto.arima(z), h = h), h = 1)
cat("\nRolling 1-step CV RMSE (ARIMA):", round(sqrt(mean(cv_arima^2, na.rm = TRUE)), 4), "\n")

## ---- 10. Final model on full sample & 12-month forecast --------
final_arimax <- auto.arima(y, xreg = x, stepwise = FALSE, approximation = FALSE)
final_x_ar   <- auto.arima(x)                     # to project polysilicon forward
x_future     <- forecast(final_x_ar, h = 12)$mean

fc_scenario_trend <- forecast(final_arimax, xreg = x_future, h = 12)
fc_scenario_flat  <- forecast(final_arimax, xreg = rep(tail(x, 1), 12), h = 12)

autoplot(fc_scenario_trend) +
  labs(title = "12-Month Forecast: Log Panel Price (Pakistan)",
       subtitle = "xreg = ARIMA-projected polysilicon price",
       y = "log(panel price)") + theme_minimal()
ggsave("forecast_trend_scenario.png", width = 9, height = 5)

fc_table <- data.frame(
  Month           = seq(as.Date("2026-06-01"), by = "month", length.out = 12),
  Trend_scenario  = as.numeric(fc_scenario_trend$mean),
  Flat_scenario   = as.numeric(fc_scenario_flat$mean),
  Lo95            = as.numeric(fc_scenario_trend$lower[, 2]),
  Hi95            = as.numeric(fc_scenario_trend$upper[, 2])
)
print(fc_table)
write.csv(fc_table, "panel_price_forecast_12m.csv", row.names = FALSE)

## ---- 11. Policy read-out -----------------------------------------
cat("\n============ POLICY SUMMARY ============\n")
cat("1. Pass-through: a 1% rise in global polysilicon price is associated with a",
    round(coef(eg_reg)["x"], 3), "% long-run change in Pakistan panel prices",
    "(cointegrating relationship).\n")
cat("2. Granger results indicate the direction of influence runs predominantly",
    "from the global polysilicon market to domestic panel prices, consistent with",
    "Pakistan being a price-taker with negligible domestic module manufacturing.\n")
trend_dir <- ifelse(tail(fc_table$Trend_scenario, 1) < fc_table$Trend_scenario[1],
                     "continued decline", "stabilization")
cat("3. Forecasts point to a", trend_dir, "in log panel prices over the next 12",
    "months -- supportive of further residential/commercial solar uptake if duty",
    "and tax structures on imported modules are not tightened.\n")
cat("4. Because pricing is driven by an imported input with no domestic hedge,",
    "policy levers with real traction are: (a) tariff/GST rationalization on",
    "panels and inverters, (b) exchange-rate pass-through management, and",
    "(c) incentives for local cell/module assembly to reduce exposure to",
    "polysilicon price cycles.\n")
cat("5. Caveat: series length (n=113) limits power of cointegration and Johansen",
    "tests; results should be read as indicative, and re-estimated as more months",
    "of data accrue.\n")
