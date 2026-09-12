library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(scales)
library(quantmod)
library(urca)
library(broom)
library(tidyverse)

## Using Regression Model for Forecasting

data(CASchools)
CASchools <- CASchools |> mutate(str=students/teachers)
CASchools <- CASchools |> mutate(score=read+math)
mod <- lm(score ~ str, data = CASchools)

mod |> tidy()


predict(mod, newdata = data.frame("str" = 25))



## Time Series

# attach the package 'quantmod'
# load US macroeconomic data
USMacroSWQ <- read_xlsx("data/us_macro_quarterly.xlsx",
                        sheet = 1,col_types = c("text", rep("numeric", 9)))

USMacroSWQ |> glimpse()

USMacroSWQ |> colnames()
# format date column
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")
# adjust column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI",
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")


# GDP series as xts object
GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]
# GDP growth series as xts object
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))


autoplot(GDP)+ggtitle("GDP Plot")

autoplot(GDPGrowth)+ geom_line(aes(color = "steelblue"), size = 1.0,
                               show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="Logarithm", title="U.S. Real GDP Growth Rates")+
  theme_minimal()



# compute logarithms, annual growth rates and 1st lag of growth rates
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}


# obtain a data.frame with level, logarithm, annual growth rate and its 1st lag of GDP
quants(GDP["2011-07::2013-01"])

acf(na.omit(GDPGrowth), lag.max = 4, plot = F)




# define series as xts objects
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]
DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]
JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP), USMacroSWQ$Date)["1960::2013"]
# attach NYSESW data
data("NYSESW")
NYSESW <- xts(Delt(NYSESW))



# plot the series

autoplot(USUnemp) + geom_line(aes(color = "steelblue"), size = 1.0,
                               show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="Percent", title="U.S. Unemployment Rates")+
  theme_minimal()


autoplot(DollarPoundFX) + geom_line(aes(color = "steelblue"), size = 1.0,
                              show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="Dollar per pound", title="U.S. Dollar / B. Pound Exchange Rate",
       caption="Source:SW by @zahed")+
  theme_minimal()

autoplot(JPIndProd) + geom_line(aes(color = "steelblue"), size = 1.0,
                                    show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="Logarithm", title="Japanese Industrial Production")+
  theme_minimal()

autoplot(NYSESW) + geom_line(aes(color = "steelblue"), size = 1.0,
                                show.legend = FALSE) +
  scale_color_manual(values = "steelblue") +
  labs(x="Date",y="Percent per Day", title="New York Stock Exchange Composite Index")+
  theme_minimal()


# compute sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), plot = F, lag.max = 10)

ggAcf(na.omit(NYSESW), plot = T, lag.max = 10)

ggPacf(na.omit(NYSESW), plot = T, lag.max = 10)

# plot sample autocorrelation for the NYSESW series
ggAcf(na.omit(NYSESW), main = "Sample Autocorrelation for NYSESW Data")







## Arima

# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

ar_model <- ar(GDPGRSub, aic = TRUE)

ar_model
library(lmtest)


# Convert the AR model to an lm object
lm_model <- ar_model$model
ar_model
# Perform a coeftest with heteroscedasticity-robust standard errors
coeftest_result <- coeftest(lm_model, vcov. = vcovHC, type = "HC1")
coeftest(ar_model, vcov. = vcovHC, type = "HC1")
# Estimate an AR(1) model
arima_model <- arima(GDPGRSub, order = c(1, 0, 0))
arima_model |> tidy()

# Extract the residuals from the ARIMA model
arima_residuals <- residuals(arima_model)

# Create a linear regression model with the residuals as the dependent variable
lm_model <- lm(arima_residuals ~ 1)

# Perform coefficient tests with heteroscedasticity-robust standard errors
coeftest_result <- coeftest(lm_model, vcov = vcovHC, type = "HC1")

coeftest_result
# Estimate an ARIMA model using automatic order selection
arima_model <- forecast::auto.arima(GDPGRSub)

arima_model

# Estimate an ARIMA model using automatic order selection
arima_model <- forecast::auto.arima(GDPGRSub)

# Display the estimated ARIMA model
print(arima_model)



# Assuming you have an AR model named 'ar_model'

# Number of periods to forecast ahead
n_periods <- 12  # Adjust as needed

# Forecast future values
forecast_values <- predict(ar_model, n.ahead = n_periods)

# Extract the point forecasts
forecast_point_estimates <- forecast_values$pred

# Display the forecasted values
print(forecast_point_estimates)


library(dynlm)
# estimate the AR(2) model
GDPGR_AR2 <- dynlm(ts(GDPGrowth)~L(ts(GDPGrowth,1))+L(ts(GDPGrowth,2)))
coeftest(GDPGR_AR2, vcov. = sandwich)

# AR(2) forecast of GDP growth in 2013:Q1
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGrowth[N-1],
                                                GDPGrowth[N-2]))




ar_2 <- arima(GDPGRSub, order = c(2, 0, 0))
ar_2

class(GDP)

frequency(GDP)

# Use snaive() to forecast 
fcGDP <- snaive(GDP,h=12)
fcn <- naive(GDP,h=12)
accuracy(fcGDP,fcn)
class(fcGDP)
class(fcn)
autoplot(fcGDP)
autoplot(fcn)

GDP %>% snaive()%>% checkresiduals



## 14.4 Can You Beat the Market? (Part I)




# read in data on stock returns
SReturns <- read_excel("data/Stock_Returns_1931_2002.xls",
                      sheet = 1,
                      col_types = "numeric")
#Alternatively, ASC cen be read directly from internet.
#SReturns <- read.csv("https://www.princeton.edu/~mwatson/Stock-Watson_3u/Students/EE_Datasets/Stock_Rsep = "\t", header = FALSE )


colnames(SReturns) <- c( "Year", "Month", "ExReturn", "ln_DivYield" )

#We continue by converting the data to an object of class ts.
# convert to ts object
StockReturns <- ts(SReturns[, 3:4],
start = c(1931, 1),
end = c(2002, 12),
frequency = 12)

# estimate AR models:
# AR(1)
SR_AR1 <- dynlm(ExReturn ~ L(ExReturn),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))
# AR(2)
SR_AR2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))
# AR(4)
SR_AR4 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2:4),
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))



#After computing robust standard errors, we gather the results in a table
#generated by stargazer().
# compute robust standard errors
rob_se <- list(sqrt(diag(sandwich(SR_AR1))),
               sqrt(diag(sandwich(SR_AR2))),
               sqrt(diag(sandwich(SR_AR4))))

models <- list("SR_AR1"=SR_AR1,"SR_AR2"=SR_AR2,"SR_AR4"=SR_AR4)

modelsummary::msummary(models)

modelsummary::msummary(models, vcov="robust")






#Forecasting GDP Growth Using the Term Spread
#Interest rates on long-term and short term treasury bonds are closely linked to macroeconomic conditions.
# While interest rates on both types of bonds have the same long-run tendencies,
# they behave quite differently in the short run. The difference in interest
# rates of two bonds with distinct maturity is called the term spread. The
# following code chunks reproduce Figure 14.3 of the book which displays
# interest rates of 10-year U.S. Treasury bonds and 3-month U.S. Treasury bills
# from 1960 to 2012.
# 3-month Treasury bills interest rate 

TB3MS <- xts(USMacroSWQ$TB3MS, USMacroSWQ$Date)["1960::2012"]

# 10-year Treasury bonds interest rate

TB10YS <- xts(USMacroSWQ$GS10, USMacroSWQ$Date)["1960::2012"]

# term spread

TSpread <- TB10YS - TB3MS

# reproduce Figure 14.2 (a) of the book
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)),
     plot.type = "single",
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Interest Rates")
# define function that transform years to class 'yearqtr'
YToYQTR <- function(years) {
  return(
    sort(as.yearqtr(sapply(years, paste, c("Q1", "Q2", "Q3", "Q4"))))
  )
}
# recessions
recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001,
                        2007:2008))
# add color shading for recessions

xblocks(time(as.zoo(TB3MS)),
        c(time(TB3MS) %in% recessions),
        col = alpha("steelblue", alpha = 0.3))
# add a legend
legend("topright",
       legend = c("TB3MS", "TB10YS"),
       col = c("darkred", "steelblue"),
       lwd = c(2, 2))


# reproduce Figure 14.2 (b) of the book
plot(as.zoo(TSpread),
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")
# add color shading for recessions
xblocks(time(as.zoo(TB3MS)),
        c(time(TB3MS) %in% recessions),
        col = alpha("steelblue", alpha = 0.3))

# Assuming you have time series data in two zoo objects, TB3MS and TB10YS
# Convert the data to a ts object
library(forecast)


ts_data <- ts(merge(as.zoo(TB3MS), as.zoo(TB10YS)), start = c(1960, 1), frequency = 4)

# Create the plot using autoplot
autoplot(ts_data, series = c("TB3MS", "TB10YS")) +
  labs(x = "Date", y = "Percent per annum", title = "Interest Rates") +
  scale_color_manual(values = c("darkred", "steelblue")) +
  scale_linetype_manual(values = c(1, 2)) +
  theme_minimal()




# convert growth and spread series to ts objects
GDPGrowth_ts <- ts(GDPGrowth,
                   start = c(1960, 1),
                   end = c(2013, 4),
                   frequency = 4)
TSpread_ts <- ts(TSpread,
                 start = c(1960, 1),
                 end = c(2012, 4),
                 frequency = 4)
# join both ts objects
ADLdata <- ts.union(GDPGrowth_ts, TSpread_ts)
# estimate the ADL(2,1) model of GDP growth
GDPGR_ADL21 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) +
                       L(TSpread_ts), start = c(1962, 1), end = c(2012, 4))
coeftest(GDPGR_ADL21, vcov. = sandwich)




# 2012:Q3 / 2012:Q4 data on GDP growth and term spread
subset <- window(ADLdata, c(2012, 3), c(2012, 4))
# ADL(2,1) GDP growth forecast for 2013:Q1
ADL21_forecast <- coef(GDPGR_ADL21) %*% c(1, subset[2, 1], subset[1, 1],
                                          subset[2, 2])
ADL21_forecast
#> [,1]
#> [1,] 2.241689
# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL21_forecast
#> Qtr1
#> 2013 -1.102487
# Model (14.4) predicts the GDP growth in 2013:Q1 to be 2.24% which leads to a
# forecast error of −1.10%. We estimate the ADL(2,2) specification to see
# whether adding additional information on past term spread improves the
# forecast.
# estimate the ADL(2,2) model of GDP growth 

GDPGR_ADL22 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2)
                     + L(TSpread_ts) + L(TSpread_ts, 2),
                     start = c(1962, 1), end = c(2012, 4))
coeftest(GDPGR_ADL22, vcov. = sandwich)


# ADL(2,2) GDP growth forecast for 2013:Q1
ADL22_forecast <- coef(GDPGR_ADL22) %*% c(1, subset[2, 1], subset[1, 1],
                                          subset[2, 2], subset[1, 2])
ADL22_forecast
# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL22_forecast



# The ADL(2,2) forecast of GDP growth in 2013:Q1 is 2.27% which implies a
# forecast error of 1.14%. Do the ADL models (14.4) and (14.5) improve upon the
# simple AR(2) model (14.3)?


# compare adj. R2
c("Adj.R2 AR(2)" = summary(GDPGR_AR2)$adj.r.squared,
  "Adj.R2 ADL(2,1)" = summary(GDPGR_ADL21)$adj.r.squared,
  "Adj.R2 ADL(2,2)" = summary(GDPGR_ADL22)$adj.r.squared)
#> Adj.R2 AR(2) Adj.R2 ADL(2,1) Adj.R2 ADL(2,2)
#> 0.1338873 0.1620156 0.1691531
# compare SER
c("SER AR(2)" = summary(GDPGR_AR2)$sigma,
  "SER ADL(2,1)" = summary(GDPGR_ADL21)$sigma,
  "SER ADL(2,2)" = summary(GDPGR_ADL22)$sigma)
#> SER AR(2) SER ADL(2,1) SER ADL(2,2)
#> 3.132122 3.070760 3.057655
# F-test on coefficients of term spread
linearHypothesis(GDPGR_ADL22,
                 c("L(TSpread_ts)=0", "L(TSpread_ts, 2)=0"),
                 vcov. = sandwich)


# set seed
set.seed(1234)
# simulate the time series
Y <- arima.sim(list(order = c(2, 0, 0), ar = c(0.2, 0.2)), n = 200)
# estimate an AR(2) model using 'arima()', see ?arima
model <- arima(Y, order = c(2, 0, 0))
# compute points forecasts and prediction intervals for the next 25 periods
fc <- forecast(model, h = 25, level = seq(5, 99, 10))

autoplot(fc)
# plot a fan chart
plot(fc,
     main = "Forecast Fan Chart for AR(2) Model of Simulated Data",
     showgap = F,
     fcol = "red",
     flty = 2)
