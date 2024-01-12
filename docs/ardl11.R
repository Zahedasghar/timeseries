#Building an ARDL Model in R
#Justin S. Eloriaga
#Code Obtained and Modified from Soren Jordan and Andrew Q. Philips
#install.packages("dynamac")
library(dynamac)
library(forecast)
library(tidyverse)
library(tseries)
library(urca)
library(TSstudio)
library(janitor)
#Loading the Dataset
df <- read_csv("data/Philippine_SVAR.csv")
df <- df |> clean_names()
#Declaring the Time Series Variables
y <- ts(df$output_gap, start = c(2000,1,1), frequency = 4)
i <- ts(df$rrp, start = c(2000,1,1), frequency = 4)
pi <- ts(df$cpi, start = c(2000,1,1), frequency = 4)
#Plotting the Series
ts_plot(y, Xtitle = "date", Ytitle = "output_gap", title = "output_gap")
ts_plot(i, Xtitle = "date", Ytitle = "Interest Rate", title = "Interest Rate")
ts_plot(pi, Xtitle = "Date", Ytitle = "Inflation Rate", title = "Inflation Rate")
#Some Stationarity Tests
pp.test(y) #Stationary
pp.test(i) #Non-Stationary
pp.test(pi) #Non-Stationary
#Estimating and ARDL using dynardl
res1 <- dynardl(rrp ~ output_gap + cpi, data = df,
                lags = list("rrp" = 1, "cpi" = 2, "output_gap" = 1),
                diffs = "cpi",
                ec = TRUE, simulate = FALSE)
summary(res1)
dynardl.auto.correlated(res1)
library(modelsummary)
library(broom)

dynardl(rrp ~ output_gap + cpi, data = df,
        lags = list("rrp" = 1, "output_gap" = 1),
        diffs = "cpi",
        ec = TRUE, simulate = FALSE) 
