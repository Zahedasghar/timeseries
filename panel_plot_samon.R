# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Read the Excel file
df <- read_excel("D:/RepTemplates/applied_econ_with_r/data/samon.xlsx")

# Check the structure of the data
glimpse(df)

# Display the first few rows
head(df)
# Check unique countries in the dataset
unique_countries <- unique(df$countries)
print(unique_countries)

# Check the number of observations per country
country_counts <- table(df$countries)
print(country_counts)

# Check for missing values in key variables
missing_gdp <- sum(is.na(df$gdpecog))
missing_iqi <- sum(is.na(df$iqi))
print(paste("Missing values in GDP growth:", missing_gdp))
print(paste("Missing values in IQI:", missing_iqi))
# Load necessary libraries  
library(ggplot2)  

# Unique Countries and counts  
unique_countries <- unique(df$countries)  
print("Unique Countries:")  
print(unique_countries)  

country_counts <- table(df$countries)  
print("Observations per country:")  
print(country_counts)  

# Check for missing values in key variables: GDP growth and IQI  
missing_gdp <- sum(is.na(df$gdpecog))  
missing_iqi <- sum(is.na(df$iqi))  
print(paste("Missing values in GDP growth:", missing_gdp))  
print(paste("Missing values in IQI:", missing_iqi))  

# Faceted scatter plot with regression lines  
p <- ggplot(df, aes(x = iqi, y = gdpecog)) +   
  geom_point(alpha = 0.6) +   
  geom_smooth(method = "lm", se = FALSE, color = "blue") +   
  facet_wrap(~ countries) +  
  labs(title = "GDP Growth Rate vs IQI by Country",  
       x = "IQI (Institutional Quality Index)",  
       y = "GDP Growth Rate (Constant 2015 USD)") +  
  theme_minimal()  
print(p)  

# Regression analysis for each country; storing summary in list  
regression_results <- lapply(unique(df$countries), function(ctry) {  
  sub_df <- df[df$countries == ctry, ]  
  reg <- lm(gdpecog ~ iqi, data = sub_df)  
  summary(reg)  
})  
names(regression_results) <- unique(df$countries)  
print(regression_results)  
