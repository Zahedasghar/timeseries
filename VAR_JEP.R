# Load necessary libraries
library(vars)
library(svars)
library(lmtest)

# Assuming your data is loaded into a data frame called 'data'
library(haven)
sw_df <- read_dta("C:/Users/92300/Dropbox/Applied Econometrics SBP/VAR JEP data.dta")
sw_df |> head()

# Set the time series data

sw_df <- ts(sw_df, start = c(2000, 1), frequency = 4)

sw_df1 <- sw_df[,2:4]

# Estimate VAR model
var_model <- VAR(sw_df1, p = 4, type = "const")
summary(var_model)

## Perform granger causality test
# Assuming var_model is your VAR model with lag order 4
granger_causality <- causality(var_model, cause = "inflation",effect=c( "unrate", "ffr"))
# Perform Granger causality test for each effect variable separately
granger_causality_unrate <- causality(var_model, cause = "inflation")
granger_causality_ffr <- causality(var_model, cause = "unrate")
granger_causality_inf <- causality(var_model, cause = "ffr")
granger_causality_inf
granger_causality_unrate
granger_causality_ffr

library(lmtest)
grangertest(inflation ~ ffr,order = 4, data = sw_df1)

grangertest(inflation ~ unrate,order = 4, data = sw_df1)


## Performa forecast error variance decomposition only at 4, 8 and 12 quarters
fevd_model <- fevd(var_model, n.ahead = c(4, 8, 12))

fevd_model <- fevd(var_model, n.ahead = 12)
fevd_model

fevd(var_model)

# Perform stability test
stability(var_model)

# Estimate impulse response function
irf_model <- irf(var_model, impulse = c("inflation", "unrate", "ffr"), response = c("inflation", "unrate", "ffr"), n.ahead = 20)

# Plot impulse response function
plot(irf_model, yline = 0, ncol = 3, by = "response", main = "Impulse Response Function")

# Define the A and B matrices for SVAR
A <- matrix(c(1, 0, 0, NA, 1, 0, NA, NA, 1), nrow = 3, byrow = TRUE)
B <- matrix(c(NA, 0, 0, 0, NA, 0, 0, 0, 1), nrow = 3, byrow = TRUE)

# Perform SVAR
svar_model <- SVAR(var_model, A = A, B = B)

svar_model

# Additional SVAR example
# Assuming your data for the second SVAR is loaded into a data frame called
# 'data2'
time_series_data2 <- ts(data2, start = c(2005, 1), frequency = 4)
var_model2 <- VAR(time_series_data2, p = 4, type = "const")
svar_model2 <- svar(var_model2, A = A, B = B)
