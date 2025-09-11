# Install necessary package
if (!require("WDI")) install.packages("WDI")

# Load necessary libraries
library(WDI)
library(tidyverse)

# Define the indicators
indicators <- c("NE.CON.TOTL.CD", "NY.GDP.MKTP.CD", "SP.POP.TOTL")

# Fetch the data for the indicators from WDI
wdi_data <- WDI(indicator = indicators, start = 2000, end = 2023, extra = TRUE)

# Rename columns for clarity
wdi_data <- wdi_data %>%
  rename(
    Consumption = NE.CON.TOTL.CD,
    GDP = NY.GDP.MKTP.CD,
    Population = SP.POP.TOTL
  )

# Inspect the data
head(wdi_data)

# Select relevant variables for correlation analysis
selected_data <- wdi_data %>%
  select(country, iso2c, year, Consumption, GDP, Population)

## Select Pakistan, India, Bangladesh, and Sri Lanka


selected_data <- selected_data %>%
  filter(country %in% c("Pakistan", "India", "Bangladesh", "Sri Lanka"))

## Convert this to panel data

# Load the plm package
library(plm)

# Convert the data to panel data

panel_data <- pdata.frame(selected_data, index = c("country", "year"))

# Check the structure of the panel data

str(panel_data)

panel_data |> head()

## Run pooled OLS regression Consumption ~ GDP + Population

# Run the pooled OLS regression

pooled_ols <- plm(Consumption ~ GDP + Population, data = panel_data, model = "pooling")

# Summarize the results

summary(pooled_ols)

## Run fixed effects model

# Run the fixed effects model

fixed_effects <- plm(Consumption ~ GDP + Population, data = panel_data, model = "within")

# Summarize the results

summary(fixed_effects)

## Run random effects model

# Run the random effects model

random_effects <- plm(Consumption ~ GDP + Population, data = panel_data, model = "random")

# Summarize the results

summary(random_effects)

## Hausman test

# Perform the Hausman test
