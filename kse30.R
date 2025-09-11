
library(tidyverse)
library(forecast)
 library(zoo)
library(ggfortify)
library(readxl)
library(xts)

# https://www.investing.com/indices/karachi-30-historical-data 

kse30 <- read_csv("docs/data/kse_30_index_v2.csv")

kse30 |> dim()

kse30 |>  glimpse()



kse30$date <- as.Date(kse30$date,format = "%d-%b-%y")

kse30$day <- weekdays(kse30$date)

kse30 |> glimpse()

# rename hig as high
kse30 <- kse30 |> rename(high = hig)
# Create an xts time series object
kse_xts <- xts(kse30[, c("close", "open", "high", "low")], order.by = kse30$date)

kse_xts |> head()
# Locate the weeks


endpoints(kse_xts,on="weeks")

# Locate every two weeks
endpoints(kse_xts, on = "weeks", k = 2)

# Locate every 4 weeks
endpoints(kse_xts, on = "weeks", k = 4)

library(xts)

# Example: Create monthly endpoints
ep <- endpoints(kse_xts, on = "weeks")

# Apply mean across each period (by column)
week_means <- period.apply(kse_xts, INDEX = ep, FUN = colMeans)

# Now calculate the weekly mean and display the results
period.apply(kse_xts, INDEX = ep, FUN = mean) 


# Split  by week
ts_weekly <- split(kse_xts, f = "weeks")
ts_weekly |> head(15)

ts_weekly |> head()
# Create a list of weekly means, temps_avg, and print this list
ts_avg <- lapply(X =ts_weekly, FUN = mean)
ts_avg






# Convert kse to weekly and assign to kse_weekly
kse_weekly <- to.period(kse_xts, period = "weeks",OHLC = FALSE)


# Convert kse to monthly and assign to kse_monthly
kse_monthly<- to.period(kse_xts, period = "months",OHLC = FALSE)

# Convert eq_mkt to quarterly OHLC
kse_quarterly <- to.period(kse_xts, period = "quarters",OHLC = FALSE)

# Convert kse to yearly univariate and assign to kse_yearly
kse_yearly <- to.period(kse_xts, period = "years", OHLC = FALSE)



# Convert eq_mkt to quarterly using shortcut function
kse_quarterly2 <- to.quarterly(kse_xts, name = "kse_index", indexAt = "firstof")

kse_quarterly |> head()

kse_quarterly2 |> head()


# Split kse into years
kse_years <- split(kse_xts , f = "years")

# Use lapply to calculate the cumsum for each year in kse
kse_ytd <- lapply(kse_years, FUN = cumsum)

# Use do.call to rbind the results
kse1_xts <- do.call(rbind, kse_ytd)


# Use rollapply to calculate the rolling weekly period sd of kse30
kse_sd <- rollapply(kse_xts, width=5, FUN = sd)


# Calculate the periodicity of temps
periodicity(kse_xts)


# Convert kse_xts to yearly
kse_yearly <- to.yearly(kse_xts)
kse_yearly
# Calculate the periodicity of kse_yearly
periodicity(kse_yearly)

# Count the days

ndays(kse_xts)

# Count the weeks
nweeks(kse_xts)

# Count the months

nmonths(kse_xts)
# Count the quarters
nquarters(kse_xts)

# Count the years

nyears(kse_xts)


## Time zone

tzone(kse_xts)


# Explore underlying units of kse_xts in two commands: .index() and .indexwday()
.index(kse_xts)
.indexwday(kse_xts)










# Second part -------------------------------------------------------------



# Create an index of weekend days using which()
index <- which(.indexwday(kse_xts) == 0 | .indexwday(kse_xts) == 6)

# Select the index
kse_xts[index]





kse30 |> select(close, date) |> glimpse()

# Create an xts time series
daily_kse30 <- xts(kse30)

ggplot(kse30)+aes(x=date,y=close)+
  geom_line() +geom_smooth(method="lm",se=FALSE)




library(dygraphs)


kse30 |> 
  select(date, open) |> 
  dygraph(main="KSE-30 price index") |> 
  dyAxis("y", label = "Open") |> 
  dyAxis("x", label = "date") |> 
  dyOptions(drawPoints = TRUE)


#https://www.geeksforgeeks.org/how-to-use-interactive-time-series-graph-using-dygraphs-in-r/
 
# Assuming kse30 is your data frame containing 'date' and 'Price' columns
library(dygraphs)
library(xts)

# Convert to xts object first
kse30_xts <- xts(kse30[, -1], order.by = as.Date(kse30$date))

# Ensure all columns except date are numeric, and preserve column names
kse30_xts <- xts(x = as.matrix(kse30[, c("open", "high", "low", "close", "volume", "change", "day")]),
                 order.by = as.Date(kse30$date))

# Plotting
dygraph(kse30_xts, main = "KSE-30 Price Index") %>%
  dySeries("close", label = "KSE-30", color = "black") %>%
  dyShading(from = "2022-10-13", to = "2025-04-11", color = "#FFE6E6") %>%
  dyShading(from = "2024-01-01", to = "2025-01-01", color = "#CCEBD6")
kse30 |> colnames()
# plot graph 

dygraph(kse30, main = "KSE-30 Price Index") %>%  
  dySeries("Index", label = "KSE-30", color = "black") %>% 
  dyShading(from = "2022-10-13", to = "2025-04-11", color = "#FFE6E6") %>% 
  dyShading(from = "2024-01-01", to = "2025-01-01", color = "#CCEBD6")

  
kse30  

kse30 %>% 
  select(date, open) %>%
  dygraph(main = "KSE-30 price index") %>%
  dyAxis("y", label = "Open") %>%
  dyAxis("x", label = "date")  |> 
  dyRangeSelector()

  
  kse30 %>% mutate(p_diff=Price - lag(Price)) |> 
    select(date, p_diff) %>%
    dygraph(main = "KSE-30 price index") %>%
    dyAxis("y", label = "Price") %>%
    dyAxis("x", label = "date")  |> 
    dyRangeSelector()
  
  
  kse30 %>% mutate(p_diff=Price - lag(Price)) |> 
    select(date, p_diff) %>%
    dygraph(main = "KSE-30 price index") |> 
  
  dyShading(from = "2020-1-1", to = "2021-6-1", color = "#FFE6E6") %>%
    dyShading(from = "2022-4-1", to = "2023-4-1", color = "#CCEBD6")

  