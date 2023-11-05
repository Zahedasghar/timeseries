## Time Series

# attach the package 'quantmod'
# load US macroeconomic data
library(tidyverse)
library(forecast)
library(zoo)
library(readxl)
daily <- read_excel("data/2022_daily.xlsx")


daily |> glimpse()
daily$date <- as.Date(daily$Date,format = "%m/%d/%Y")

colnames(daily) <- c("date",'flour',"karta","riceselling", "redemption")

# Create an xts time series object
daily_xts <- xts(daily[, c('flour',"karta","riceselling", "redemption")], order.by = daily$date)

autoplot(daily_xts$flour)

library(plotly)

p <- autoplot(na.locf(daily_xts$flour))+theme_minimal()

ggplotly(p)
autoplot(na.locf(daily_xts$riceselling,fromLast = TRUE))

autoplot(na.approx(daily_xts$flour))
+geom_line(aes(color = "steelblue"), linewidth = 1.0,
                                              show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="", title="Plotting daily ....")+
  theme_minimal()

ggplotly(p)


autoplot(na.locf(daily_xts$karta))

autoplot(daily_xts$riceselling)

autoplot(daily_xts$redemption)
# Locate the weeks


endpoints(daily_xts,on="weeks")

# Locate every two weeks
endpoints(daily_xts, on = "weeks", k = 2)

# Locate every 4 weeks
endpoints(daily_xts, on = "weeks", k = 4)


# Calculate the weekly endpoints
ep <- endpoints(daily_xts, on = "weeks")

# Now calculate the weekly mean and display the results
period.apply(daily_xts, INDEX = ep, FUN = mean,na.rm=TRUE) 

# Or  Following two commands are very similar to period.apply----------------------------------------------------------------------

# Split temps by week
ts_weekly <- split(daily_xts, f = "weeks")

# Create a list of weekly means, temps_avg, and print this list
ts_avg <- lapply(X =ts_weekly, FUN = mean,na.rm=TRUE)
ts_avg






# Convert daily to weekly and assign to daily_weekly
daily_weekly <- to.period(daily_xts, period = "weeks")

# Convert daily to monthly and assign to daily_monthly
daily_monthly<- to.period(daily_xts, period = "months")

autoplot(daily_monthly)

# Convert daily to yearly univariate and assign to daily_yearly
daily_yearly <- to.period(daily_xts, period = "years", OHLC = FALSE)


# Convert eq_mkt to quarterly OHLC
daily_quarterly <- to.period(daily_xts, period = "quarters")

# Convert eq_mkt to quarterly using shortcut function
daily_quarterly2 <- to.quarterly(daily_xts, name = "daily_index", indexAt = "firstof")

daily_quarterly

daily_quarterly2


# Split edhec into years
daily_years <- split(daily_xts , f = "years")

daily_years



# Explore underlying units of daily_xts in two commands: .index() and .indexwday()
.index(daily_xts)
.indexwday(daily_xts)

# Create an index of weekend days using which()
index <- which(.indexwday(daily_xts) == 0 | .indexwday(daily_xts) == 6)

# Select the index
daily_xts[index]

autoplot(daily_xts[index])


