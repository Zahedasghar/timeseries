# ==============================================================================
# Vector Autoregression (VAR) and Structural VAR (SVAR) Analysis
# ==============================================================================

# Load required libraries --------------------------------------------------
library(vars)  
# VAR modeling
library(svars)     # Structural VAR
library(lmtest)    
library(tidyverse) # Data manipulation
library(haven)     # Read Stata files

# Load and prepare data ----------------------------------------------------
# Read Stata data file
sw_df <- read_dta("data/VAR JEP data.dta")

# Preview data structure
head(sw_df)
tail(sw_df)
glimpse(sw_df)

# Convert to time series object (quarterly data starting 2000 Q1)
sw_ts <- ts(sw_df, start = c(1960, 1), frequency = 4)

# Select relevant variables (columns 2-4: inflation, unemployment rate, fed funds rate)
sw_data <- sw_ts[, 2:4]

# Verify data structure
glimpse(sw_data)

# Estimate VAR model -------------------------------------------------------
# Fit VAR model with 4 lags and constant term
var_model <- VAR(sw_data, p = 4, type = "const")

# Display model summary
summary(var_model)

# Granger causality tests --------------------------------------------------

# Run Granger causality tests for each variable using lags and joint tests by creating lags first 

# Create lagged variables for 4 lags 

sw_data1 <- as.data.frame(sw_data)

sw_data1 <- sw_data %>%
  mutate(
    L1.inflation = lag(inflation, 1),
    L2.inflation = lag(inflation, 2),
    L3.inflation = lag(inflation, 3),
    L4.inflation = lag(inflation, 4),
    L1.unrate   = lag(unrate, 1),
    L2.unrate   = lag(unrate, 2),
    L3.unrate   = lag(unrate, 3),
    L4.unrate   = lag(unrate, 4),
    L1.ffr      = lag(ffr, 1),
    L2.ffr      = lag(ffr, 2),
    L3.ffr      = lag(ffr, 3),
    L4.ffr      = lag(ffr, 4)
  ) %>%
  na.omit()  # Remove rows with NA values due to lagging

m1 <- lm(inflation ~ L1.inflation + L2.inflation + L3.inflation + L4.inflation +
           L1.unrate + L2.unrate + L3.unrate + L4.unrate +
           L1.ffr + L2.ffr + L3.ffr + L4.ffr, data = sw_data1)

m2 <- lm(unrate ~ L1.inflation + L2.inflation + L3.inflation + L4.inflation +
           L1.unrate + L2.unrate + L3.unrate + L4.unrate +
           L1.ffr + L2.ffr + L3.ffr + L4.ffr, data = sw_data1)
m3 <- lm(ffr ~ L1.inflation + L2.inflation + L3.inflation + L4.inflation +
           L1.unrate + L2.unrate + L3.unrate + L4.unrate +
           L1.ffr + L2.ffr + L3.ffr + L4.ffr, data = sw_data1)

# Joint hypothesis tests for each variable using car package

# H0: L1.unrate = L2.unrate = L3.unrate = L4.unrate = 0
car::linearHypothesis(m1, c(
  "L1.unrate = 0",
  "L2.unrate = 0",
  "L3.unrate = 0",
  "L4.unrate = 0"
))

# H0: L1.inflation = L2.inflation = L3.inflation = L4.inflation = 0
car::linearHypothesis(m2, c(
  "L1.inflation = 0",
  "L2.inflation = 0",
  "L3.inflation = 0",
  "L4.inflation = 0"
))

# H0: L1.ffr = L2.ffr = L3.ffr = L4.ffr = 0 

car::linearHypothesis(m1, c(
  "L1.ffr = 0",
  "L2.ffr = 0",
  "L3.ffr = 0",
  "L4.ffr = 0"
)) 



# Test if inflation Granger-causes other variables
granger_inflation <- causality(var_model, cause = "inflation")

# Test if unemployment rate Granger-causes other variables
granger_unrate <- causality(var_model, cause = "unrate")

# Test if federal funds rate Granger-causes other variables
granger_ffr <- causality(var_model, cause = "ffr")

# Display results
cat("\n=== Granger Causality: Inflation ===\n")
print(granger_inflation)

cat("\n=== Granger Causality: Unemployment Rate ===\n")
print(granger_unrate)

cat("\n=== Granger Causality: Federal Funds Rate ===\n")
print(granger_ffr)

# Pairwise Granger causality tests
cat("\n=== Pairwise Granger Tests ===\n")
grangertest(ffr ~ unrate, order = 4, data = sw_data)
grangertest(inflation ~ unrate, order = 4, data = sw_data)

# Forecast Error Variance Decomposition (FEVD) ----------------------------
# Compute FEVD up to 12 quarters ahead
fevd_results <- fevd(var_model, n.ahead = 12)

# Display FEVD results
print(fevd_results)

# Plot FEVD
plot(fevd_results)

# Model diagnostics --------------------------------------------------------
# Check VAR model stability (roots should be inside unit circle)
stability_test <- stability(var_model)
print(stability_test)

# Plot eigenvalues
plot(stability_test)

# Impulse Response Functions (IRF) -----------------------------------------
# Estimate IRFs for all variable combinations
irf_results <- irf(
  var_model,
  impulse = c("inflation", "unrate", "ffr"),
  response = c("inflation", "unrate", "ffr"),
  n.ahead = 20,
  boot = TRUE,      # Bootstrap confidence intervals
  ci = 0.95         # 95% confidence level
)

# Plot IRFs organized by response variable
plot(
  irf_results,
  yline = 0,
  ncol = 3,
  main = "Impulse Response Functions (20 Quarters)"
)

# Structural VAR (SVAR) Analysis -------------------------------------------
# Define recursive identification structure (Cholesky decomposition)
# A matrix: contemporaneous effects
# Order: inflation -> unemployment -> fed funds rate
A_matrix <- matrix(
  c(1,  0,  0,
    NA, 1,  0,
    NA, NA, 1),
  nrow = 3, 
  byrow = TRUE
)

# B matrix: structural shocks standard deviations
B_matrix <- matrix(
  c(NA, 0,  0,
    0,  NA, 0,
    0,  0,  NA),
  nrow = 3, 
  byrow = TRUE
)

# Estimate SVAR model
svar_model <- SVAR(var_model, Amat = A_matrix, Bmat = B_matrix)

# Display SVAR results
summary(svar_model)

# Structural Impulse Response Functions
svar_irf <- irf(
  svar_model,
  n.ahead = 20,
  boot = TRUE,
  ci = 0.95
)

# Plot structural IRFs
plot(
  svar_irf,
  yline = 0,
  main = "Structural Impulse Response Functions"
)


# Save results -------------------------------------------------------------
# Optional: Save key results to file
# saveRDS(var_model, "results/var_model.rds")
# saveRDS(svar_model, "results/svar_model.rds")

# ==============================================================================
# End of Analysis
# ==============================================================================
