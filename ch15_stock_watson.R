## VAR models example
library(AER)
library(quantmod)
library(dynlm)
#install.packages("orcutt")
library(orcutt)
library(nlme)
library(stargazer)
#library(tidyverse)
library(vars)
library(forecast)
library(tseries)
library(urca)
library(broom)
library(xts)
library(zoo)


# load the frozen orange juice data set
data("FrozenJuice")
# compute the price index for frozen concentrated juice
FOJCPI <- FrozenJuice[, "price"]/FrozenJuice[, "ppi"]
FOJC_pctc <- 100 * diff(log(FOJCPI))
FDD <- FrozenJuice[, "fdd"]
# convert series to xts objects
FOJCPI_xts <- as.xts(FOJCPI)
FDD_xts <- as.xts(FrozenJuice[, 3])
# Plot orange juice price index
library(ggplot2)

# Assuming FDD_xts is your xts object
ggplot(data = data.frame(Date = index(FOJCPI_xts), Price = coredata(FOJCPI_xts)),
       aes(x = Date, y = Price)) +
  geom_line(color = "steelblue", linewidth=1.5) +
  labs(x = "Date", y = "Price index", title = "Frozen Concentrated Orange Juice")

FOJCPI_xts |> head()

plot(as.zoo(FOJCPI),
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Price index",
     main = "Frozen Concentrated Orange Juice")



plot(as.zoo(FOJC_pctc),
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent",
     cex.main=0.8,
     main = "Monthly Changes in the Price of Frozen Conentrated Orange Juice")

## Plot FOJC_pctc
FOJC_pctc_xts <- as.xts(FOJC_pctc)
ggplot(data = data.frame(Date = index(FOJC_pctc_xts), Percent = coredata(FOJC_pctc_xts)),
       aes(x = Date, y = Percent)) +
  geom_line(color = "steelblue", linewidth=1.0) +
  labs(x = "Date", y = "Percent", title = "Monthly Changes in the Price of Frozen Conentrated Orange Juice")



# plot freezing degree days
FDD_xts <- as.xts(FDD)
ggplot(data = data.frame(Date = index(FDD_xts), FDD = coredata(FDD_xts)),
       aes(x = Date, y = FDD)) +
  geom_line(color = "steelblue", linewidth=1.0) +
  labs(x = "Date", y = "Freezing degree days", title = "Monthly Freezing Degree Days in Orlando, FL")


plot(as.zoo(FDD),
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Freezing degree days",
     cex.main=0.8,
     main = "Monthly Freezing Degree Days in Orlando, FL")

# simple regression of percentage changes on freezing degree days
orange_SR <- dynlm(FOJC_pctc ~ FDD)
coeftest(orange_SR, vcov. = vcovHAC)

orange_SR |> tidy()

# distributed lag model with 6 lags of freezing degree days
orange_DLM <- dynlm(FOJC_pctc ~ FDD + L(FDD, 1:6))
coeftest(orange_DLM, vcov. = vcovHAC)
