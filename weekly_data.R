library(tidyverse)
library(readxl)
library(forecast)
library(zoo)
library(xts)
library(fpp2)
library(fpp3)
library(tsibble)
# Read the data from a CSV file
m2w <- read_excel("data/Broad_money_weekly.xlsx")

m2w


library(janitor)
m2w <- m2w |> clean_names()



# Convert the date column to a Date object (if not already)

m2w$date <- as.Date(m2w$weekly)

# Create a time series object
ts_data <- ts(m2w$broad_money_m2, frequency = 52, start = min(m2w$date))

autoplot(ts_data)




# Second-method -----------------------------------------------------------



# Create an xts object
ts_data <- xts(m2w$broad_money_m2, order.by = m2w$date)

# Locate the weeks


endpoints(ts_data,on="weeks")

# Locate every two weeks
endpoints(ts_data, on = "weeks", k = 2)

# Locate every 4 weeks
endpoints(ts_data, on = "weeks", k = 4)


# Calculate the weekly endpoints
ep <- endpoints(ts_data, on = "months")
ep

# Now calculate the weekly mean and display the results
period.apply(ts_data, INDEX = ep, FUN = mean) 




# Or  Following two commands are very similar to period.apply----------------------------------------------------------------------

# Split temps by week
ts_monthly <- split(ts_data, f = "months")

# Create a list of weekly means, temps_avg, and print this list
ts_avg <- lapply(X =ts_monthly, FUN = mean)
ts_avg


# Split, rbind, lapply ----------------------------------------------------

# Use the proper combination of split, lapply and rbind






ts_data |> glimpse()

autoplot(ts_data)

autoplot(ts_data)
