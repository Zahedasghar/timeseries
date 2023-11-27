# Load the required libraries
library(plm)
library(vars)

# Load the panel data
data("Grunfeld", package = "plm")

# Convert the data to a panel data format
pdata <- pdata.frame(Grunfeld, index = c("firm", "year"))

# Estimate the Panel VAR model
panel_var <- pvar(pdata, lag = 2, type = "const", season = NULL)

# Summary of the Panel VAR model
summary(panel_var)


