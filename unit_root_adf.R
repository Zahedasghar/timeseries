### PAGE 366
library(readxl)
library("gdata")
library("tseries")
library("urca")
library(tidyverse)
library(broom)
data <-  read_xls("data/COINT6.xls")

## Add observation index
data$obs_index <- seq(1, nrow(data))


## Use pivot longer command
data_long <- data %>%
  pivot_longer(cols = -obs_index, names_to = "series", values_to = "value")

# Plot using ggplot
ggplot(data_long, aes(x = obs_index, y = value, color = series)) +
  geom_line() +
  labs(title = "Three Columns Plot",
       x = "Observation Index",
       y = "Value") +
  theme_minimal()

p <- ggplot(data_long, aes(x = obs_index, y = value, color = series)) +
  geom_line() +
  labs(title = "Three Columns Plot",
       x = "Observation Index",
       y = "Value") +
  theme_minimal()

## Add 0 line 
p+  geom_hline(yintercept = 0, linetype = "dashed") 

## Fit linear models of y on z, w , z on y, w and w on y, z

lm1 <- lm(y ~ z + w, data = data)
lm2 <- lm(z ~ y + w, data = data)
lm3 <- lm(w ~ y + z, data = data)

lm1 |> tidy()
lm2 |> tidy()
lm3 |> tidy()

library("urca")

## acf and pacf of each series
acf(data$y)
acf(data$z)
acf(data$w)
pacf(data$y)
pacf(data$z)
pacf(data$w)

### adf unit root tests at lag 0 and 4
adf1 <-  ur.df(data$y,lag=0)
adf1@testreg
adf1 <-  ur.df(data$y,lag=4)
adf1@testreg
adf2 <-  ur.df(data$z,lag=0)
adf2@testreg
adf2 <-  ur.df(data$z,lag=4)
adf2@testreg
adf3 <-  ur.df(data$w,lag=0)
adf3@testreg
adf3 <-  ur.df(data$w,lag=4)
adf3@testreg

###  Unit root tests for Residuals of each model
adf1 <-  ur.df(lm1$residuals,lag=0)
adf1@testreg
adf1 <-  ur.df(lm1$residuals,lag=4)
adf1@testreg
adf2 <-  ur.df(lm2$residuals,lag=0)
adf2@testreg
adf2 <-  ur.df(lm2$residuals,lag=4)
adf2@testreg
adf3 <-  ur.df(lm3$residuals,lag=0)
adf3@testreg
adf3 <-  ur.df(lm3$residuals,lag=4)
adf3@testreg


# Read data
data <- read_xls("data/COINT6.xls")

# Add observation index
data$obs_index <- seq(1, nrow(data))

# Pivot longer command
data_long <- data %>%
  pivot_longer(cols = -obs_index, names_to = "series", values_to = "value")

# Plot using ggplot
p <- ggplot(data_long, aes(x = obs_index, y = value, color = series)) +
  geom_line() +
  labs(title = "Three Columns Plot",
       x = "Observation Index",
       y = "Value") +
  theme_minimal()

# Add 0 line 
p_with_line <- p + geom_hline(yintercept = 0, linetype = "dashed")

# Fit linear models of y on z, w; z on y, w; and w on y, z
lm1 <- lm(y ~ z + w, data = data)
lm2 <- lm(z ~ y + w, data = data)
lm3 <- lm(w ~ y + z, data = data)

# Display tidy summaries of linear models
tidy(lm1)
tidy(lm2)
tidy(lm3)

# ACF and PACF plots
plot_acf_pacf <- function(series) {
  acf(series)
  pacf(series)
}

plot_acf_pacf(data$y)
plot_acf_pacf(data$z)
plot_acf_pacf(data$w)

# Phillips-Perron (PP) unit root tests at lag 0 and 4 for y, z, and w
pp_test <- function(series, lag) {
  pp <- ur.pp(series, type = "Z-tau", lag = lag)
  pp@testreg
}

pp_test(data$y, lag = 0)
pp_test(data$y, lag = 4)
pp_test(data$z, lag = 0)
pp_test(data$z, lag = 4)
pp_test(data$w, lag = 0)
pp_test(data$w, lag = 4)

# PP unit root tests for residuals of each model
pp_residual_test <- function(residuals, lag) {
  pp <- ur.pp(residuals, type = "Z-tau", lag = lag)
  pp@testreg
}

pp_residual_test(lm1$residuals, lag = 0)
pp_residual_test(lm1$residuals, lag = 4)
pp_residual_test(lm2$residuals, lag = 0)
pp_residual_test(lm2$residuals, lag = 4)
pp_residual_test(lm3$residuals, lag = 0)
pp_residual_test(lm3$residuals, lag = 4)



