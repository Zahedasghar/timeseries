library(readxl)
library(tidyverse)
library(xts)

MA_SARA <- read_excel("data/Moving_Average.xlsx")

MA_SARA |> head()
## Convert to xts object

MA_xts <- xts(MA_SARA$X, order.by = as.POSIXct(MA_SARA$date))




# Calculate the 7-period lagged moving average
lagged_ma <- zoo::rollapply(MA_xts, width = 7, FUN = mean, align = "right", fill = NA)

median <- median(lagged_ma,na.rm = TRUE)

## Take the difference of median_moving_avg and lagged_ma
diff <- lagged_ma- median

View(diff)

