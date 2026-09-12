library(tidyverse)
library(forecast) # for time series models and auto.arima
library(stats)
library(ggplot2)
phi <- 0.8  # Autoregressive coefficient
n <- 200    # Number of time points

# Simulate data from the AR(1) model
set.seed(123)  # For reproducibility
ar1_data <- arima.sim(model = list(ar = phi), n = n)
# Create a data frame with time series data
time_series_data <- data.frame(Time = 1:n, Value = ar1_data)

# Create a ggplot2 line plot
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +labs(title = "AR(1) Model ", x = "Time", y = "Value") +
  theme_minimal()



## ------------------------------------------------------------------


# Set the parameters
mu <- 0       # Mean of the series
theta <- 0.6  # Moving average coefficient
n <- 200      # Number of time points

# Simulate data from the MA(1) model
set.seed(123)  # For reproducibility
ma1_data <- arima.sim(model = list(ma = theta), n = n, mean = mu)

# Create a data frame with time series data
time_series_data <- data.frame(Time = 1:n, Value = ma1_data)

# Create a ggplot2 line plot
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +labs(title = "MA(1) Model ", x = "Time", y = "Value") +
  theme_minimal()


# Set the parameters
alpha <- 0.5  # Intercept or constant term
beta <- 0.1   # Slope of the trend
phi <- 0.7    # Autoregressive coefficient
n <- 200      # Number of time points

# Create a time sequence
time <- 1:n

# Simulate data from the AR(1) model with a trend
set.seed(123)  # For reproducibility
ar1_with_trend_data <- alpha + beta * time + arima.sim(model = list(ar = phi), n = n)
# Create a data frame with time series data
time_series_data <- data.frame(Time = 1:n, Value = ar1_with_trend_data)

# Create a ggplot2 line plot
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +labs(title = "AR(1) with trend Model ", x = "Time", y = "Value") +
  theme_minimal()

# Set a random seed for reproducibility
set.seed(123)

# Length of the time series
n <- 200

# Generate a time series with an AR(1) process before the break
ar1_before <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.8), n = n/2)

# Generate a time series with a different AR(1) process after the break
ar1_after <- arima.sim(model = list(order = c(1, 0, 0), ar = -0.5), n = n/2)

# Combine the two time series at the break point
break_point <- n/2
time_series <- c(ar1_before, ar1_after)

# Create a data frame with the time series data
data <- data.frame(Time = 1:n, Value = time_series)

# Create a plot to visualize the time series
ggplot(data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +
  geom_vline(xintercept = break_point, linetype = "dashed", color = "red") +
  labs(title = "AR(1) Model with Break", x = "Time", y = "Value") +
  theme_minimal()



## ------------------------------------------------------------------
set.seed(123)

# Define AR(1) model parameters
phi <- 0.7   # Autoregressive coefficient
beta <- 0.1  # Trend coefficient
n <- 200     # Number of time periods

# Simulate AR(1) time series data with a trend
ar1_trend_data <- arima.sim(model = list(order = c(1, 0, 0), ar = phi), n = n) + beta * 1:n

# Calculate the difference of y at each time point
differences <- diff(ar1_trend_data)

# Create a data frame with time series data and differences
time_series_data <- data.frame(Time = 1:n, Value = ar1_trend_data, Difference = c(NA, differences))

# Create a ggplot2 line plot of the differences
ggplot(time_series_data, aes(x = Time, y = Difference)) +
  geom_line(linewidth=1) +
  labs(title = "AR(1) Model with Trend: Differences", x = "Time", y = "Difference") +
  theme_minimal()


## ------------------------------------------------------------------


# Set random seed for reproducibility
set.seed(123)

# Simulate an AR(1) time series with a coefficient of 0.7
n <- 200  # Number of time periods
phi <- 0.7  # AR(1) coefficient
ar1_data <- arima.sim(model = list(order = c(1, 0, 0), ar = phi), n = n)

# Create a data frame with the simulated data
time_series_data <- data.frame(Time = 1:n, Value = ar1_data)

# Create a ggplot2 line plot of the simulated AR(1) time series
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = "AR(1) Model with Coefficient 0.7", x = "Time", y = "Value") +
  theme_minimal()+theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

# Plot the ACF of the simulated data
acf(ar1_data, main = "Autocorrelation Function (ACF) of AR(1) Model")+
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title



# Set random seed for reproducibility
set.seed(123)

# Simulate MA(1) model with coefficient 0.7
n <- 200  # Number of time periods
ma1_data <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.7), n = n)

# Create a time series object
ma1_ts <- ts(ma1_data)

# Plot the MA(1) time series
ggplot(data.frame(Time = 1:n, Value = ma1_ts), aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = "Simulated MA(1) Model (Coefficient = 0.7)", x = "Time", y = "Value") +
  theme_minimal()+theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

# Plot the ACF of the MA(1) model
acf(ma1_ts, main = "ACF of Simulated MA(1) Model")+theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

## ------------------------------------------------------------------
# Set random seed for reproducibility
set.seed(123)

# Simulate AR(3) time series data with coefficients 1.3, -0.6, and 0.1
n <- 200
ar3_data <- arima.sim(model = list(order = c(3, 0, 0), ar = c(1.4, -0.6, 0.1)), n = n)

# Calculate the ACF of the simulated data
acf_values <- acf(ar3_data, plot = FALSE)

# Extract ACF values and lags
acf_data <- data.frame(Lag = acf_values$lag, ACF = acf_values$acf)

# Create and customize the correlogram plot
library(ggplot2)

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Correlogram for AR(3) Model",
       x = "Lag", y = "ACF") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

# Print the correlogram plot
print(acf_plot)


# Simulate AR(3) time series data with coefficients 1.3, -0.6, and 0.1
n <- 100
ar3_data <- arima.sim(model = list(order = c(3, 0, 0), ar = c(-0.2, 0.7, 0.3)), n = n)

# Calculate the ACF of the simulated data
acf_values <- acf(ar3_data, plot = FALSE)

# Extract ACF values and lags
acf_data <- data.frame(Lag = acf_values$lag, ACF = acf_values$acf)

# Create and customize the correlogram plot
library(ggplot2)

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Correlogram for AR(3) Model",
       x = "Lag", y = "ACF") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

# Print the correlogram plot
print(acf_plot)



#' ## Patterns for PACF
## ------------------------------------------------------------------

# Set random seed for reproducibility
set.seed(123)

# Simulate AR(1) time series data with a coefficient of 0.8
n <- 100
ar1_data <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.8), n = n)

# Calculate the PACF of the simulated data
pacf_values <- pacf(ar1_data, lag.max = 20)

# Extract PACF values
pacf_data <- data.frame(Lag = 1:20, PACF = pacf_values$acf)

# Create a custom plot of the PACF
ggplot(data = pacf_data, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Partial Autocorrelation Function (PACF) of AR(1) with 0.8 coefficient Model",
       x = "Lag", y = "PACF") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

#' 
## ------------------------------------------------------------------

# Set random seed for reproducibility
set.seed(123)

# Simulate AR(1) time series data with a coefficient of 0.8
n <- 100
ar1_data <- arima.sim(model = list(order = c(1, 0, 0), ar = -0.8), n = n)

# Calculate the PACF of the simulated data
pacf_values <- pacf(ar1_data, lag.max = 20)

# Extract PACF values
pacf_data <- data.frame(Lag = 1:20, PACF = pacf_values$acf)

# Create a custom plot of the PACF
ggplot(data = pacf_data, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Partial Autocorrelation Function (PACF) of AR(1) with -0.8 coeficient Model",
       x = "Lag", y = "PACF") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

#' :::
#' 
#' ::: {.column width="50%"}
## ------------------------------------------------------------------
# Load necessary library (if not already loaded)
library(stats)

# Simulate AR(3) time series data with coefficients 1.4, -0.6, and 0.1
n <- 100
ar3_data <- arima.sim(model = list(order = c(3, 0, 0), ar = c(1.4, -0.6, 0.1)), n = n)

# Calculate the PACF of the simulated data
pacf_values <- pacf(ar3_data, lag.max = 10)


# Plot the PACF
plot(pacf_values, main = "Partial Autocorrelation Function (PACF) of AR(3) Model",
     xlab = "Lag", ylab = "PACF", col = "blue")

#' 
## ------------------------------------------------------------------
# Load necessary library (if not already loaded)
library(stats)

# Simulate AR(3) time series data with coefficients 1.4, -0.6, and 0.1
n <- 100
ar3_data <- arima.sim(model = list(order = c(3, 0, 0), ar = c(-0.2, 0.7, 0.3)), n = n)

# Calculate the PACF of the simulated data
pacf_values <- pacf(ar3_data, lag.max = 10)


# Plot the PACF
plot(pacf_values, main = "Partial Autocorrelation Function (PACF) of AR(3) Model",
     xlab = "Lag", ylab = "PACF", col = "blue")

#' :::
#' :::
#' 
#' ## 
#' 
## ------------------------------------------------------------------
# Load necessary library (if not already loaded)
library(stats)

# Simulate MA(3) time series data with coefficients 0.7, -0.3, and -0.2
n <- 100
ma3_data <- arima.sim(model = list(order = c(0, 0, 3), ma = c(0.7, -0.3, -0.2)), n = n)

# Calculate the ACF and PACF of the simulated data
acf_values <- acf(ma3_data, lag.max = 10, plot = FALSE)
pacf_values <- pacf(ma3_data, lag.max = 10, plot = FALSE)

# Plot the ACF
par(mfrow = c(2, 1))  # Create a 2x1 grid of plots
plot(acf_values, main = "Autocorrelation Function (ACF) of MA(3) Model",
     xlab = "Lag", ylab = "ACF", col = "blue")

# Plot the PACF
plot(pacf_values, main = "Partial Autocorrelation Function (PACF) of MA(3) Model",
     xlab = "Lag", ylab = "PACF", col = "red")


#' 
#' ## Stationary Time Series
#' 
#' We now have a tool (ACF, PACF) to help us identify the stochastic stochastic process underlying time series we are observing. Now we will: - Summarize the basic patterns to look for - Observe an actual data series and make an initial guess Observe an actual data series and make an initial guess - Next step: estimate (several alternatives) based on this guess
#' 
#' ## Summary Table Pattern
#' 
#' ![Pattern for model identification](images/summary_pattern.png){.r-stretch}
#' 
#' ## Tips
#' 
#' -   ACF's that do not go to zero could be sign of nonstationarity
#' 
#' -   ACF of both AR, ARMA decay gradually, drops to 0 for MA
#' 
#' -   PACF decays gradually for ARMA, MA, drops to 0 for AR
#' 
#' Possible approach: begin with parsimonious low order AR, check residuals to decide on possible MA terms.
#' 
#' ------------------------------------------------------------------------
#' 
#' When looking at ACF, PACF
#' 
#' -   Box-Jenkins provide sampling variance of the observed ACF and PACFs ($r_s$ and $b_s$)
#' -   Permits one to construct confidence intervals around each → assess whether significantly $\neq0$
#' -   Computer packages provide this automatically
#' 
#' ## Estimation and Model Selection
#' 
#' -   Decide on plausible alternative specifications (ARMA)
#' -   Estimate each specification
#' -   Choose "best" model, based on:
#' -   Significance of coefficients
#' -   Fit vs parsimony (criteria)
#' -   White noise residuals
#' -   Ability to forecast
#' -   Account for possible structural breaks
#' 
#' ## Fit vs parsimony (information criteria):
#' 
#' -   Additional parameters (lags) automatically improve fit but reduce forecast quality
#' -   Tradeoff between fit and parsimony; widely used criteria:
#' -   Akaike Information Criterion (AIC) $AIC=T ln(SST)+2(p+q+1)$
#' -   Schwartz Bayesian Criterion (BIC) $SBC= T ln(SST)+(p+q+1)ln(T)$
#' -   SBC is considered to be preferable for having more parsimonious models than AIC
#' 
#' ## White noise errors :
#' 
#' -   Aim to eliminate autocorrelation in the residuals (could indicate that model does not reflect the lag structure well)
#' 
#' -   Plot "standardized residuals" ($\epsilon_{it}$ ) No more than 5% of them should lie outside \[-2,+2\] over all periods
#' 
#' -   Look at $r_s$, $b_s$ (and significance) at different lags Box-Pierce Statistic: joint significance test up to lag $s$: \$\bar{x} = \frac{1}{n}
#' 
#' $Q=T \sum_{k=1}^{s} r_k^2$
#' 
#' $H_0$ : all $r_k=0$, $H_1$: at least one $r_k\neq0$
#' 
#' ## Forecastability
#' 
#' Can assess how well the model forecasts " out of sample":
#' 
#' -   Estimate the model for a sub-sample (for example, the first 250 out of 300 observations).
#' -   Use estimated parameters to forecast for the rest of the sample (last 50)
#' -   Compute the " forecast errors" and assess:
#' -   Mean Squared Prediction Error
#' -   Granger-Newbold Test
#' -   Diebold-Mariano Test
#' 
#' ## Account for possible structural breaks:
#' 
#' -   Does the same model apply equally well to the entire sample, or do parameters change (significantly) within the sample?
#' 
#' How to approach:
#' 
#' -   Own priors/suspicion : Chow test for parameter change
#' 
#' -   If priors not strong, recursive estimation, tests for parameter stability over the sample, for example, CUSUM
#' 
#' ## Learning all above with simulated data
#' 
#' Lets simulate MA(1), AR(1) series in Excel, R and one can use STATA/EVIEWS as well
#' 
#' ::: panel-tabset
#' ## MA-1 code
#' 
## ------------------------------------------------------------------
#| echo: true
#| eval: false
## # Set the parameters
## mu <- 0       # Mean of the series
## theta <- 0.6  # Moving average coefficient
## n <- 200      # Number of time points
## 
## # Simulate data from the MA(1) model
## set.seed(123)  # For reproducibility
## ma1_data <- arima.sim(model = list(ma = theta), n = n, mean = mu)
## 
## # Create a data frame with time series data
## time_series_data <- data.frame(Time = 1:n, Value = ma1_data)
## 
## # Create a ggplot2 line plot
## ggplot(time_series_data, aes(x = Time, y = Value)) +
##   geom_line(linewidth=1) +labs(title = "MA(1) Model ", x = "Time", y = "Value") +
##   theme_minimal()
## 

#' 
#' ## MA1-output
#' 
## ------------------------------------------------------------------
#| echo: false
#| eval: true
# Set the parameters
mu <- 0       # Mean of the series
theta <- 0.6  # Moving average coefficient
n <- 200      # Number of time points

# Simulate data from the MA(1) model
set.seed(123)  # For reproducibility
ma1_data <- arima.sim(model = list(ma = theta), n = n, mean = mu)

# Create a data frame with time series data
time_series_data <- data.frame(Time = 1:n, Value = ma1_data)

# Create a ggplot2 line plot
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +labs(title = "MA(1) Model ", x = "Time", y = "Value") +
  theme_minimal()


#' 
#' ## AR-1 code
#' 
## ------------------------------------------------------------------
#| echo: true
#| eval: false
## phi <- 0.8  # Autoregressive coefficient
## n <- 200    # Number of time points
## 
## # Simulate data from the AR(1) model
## set.seed(123)  # For reproducibility
## ar1_data <- arima.sim(model = list(ar = phi), n = n)
## # Create a data frame with time series data
## time_series_data <- data.frame(Time = 1:n, Value = ar1_data)
## # Create a ggplot2 line plot
## ggplot(time_series_data, aes(x = Time, y = Value)) +
##   geom_line(linewidth=1) +labs(title = "AR(1) Model ", x = "Time", y = "Value") +
##   theme_minimal()
## 

#' 
#' ## AR-1 output
#' 
## ------------------------------------------------------------------
#| echo: false
#| eval: true
phi <- 0.8  # Autoregressive coefficient
n <- 200    # Number of time points

# Simulate data from the AR(1) model
set.seed(123)  # For reproducibility
ar1_data <- arima.sim(model = list(ar = phi), n = n)
# Create a data frame with time series data
time_series_data <- data.frame(Time = 1:n, Value = ar1_data)
# Create a ggplot2 line plot
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line(linewidth=1) +labs(title = "AR(1) Model ", x = "Time", y = "Value") +
  theme_minimal()

#' :::
#' 
#' ## ACF and PACF of AR-1 and MA-\`
#' 
#' ::: panel-tabset
#' ## ACF of AR-1
#' 
## ------------------------------------------------------------------
# Plot the ACF of the simulated data
acf(ar1_data, main = "Autocorrelation Function (ACF) of AR(1) Model")+
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title


#' 
#' ## PACF of AR-1
#' 
## ------------------------------------------------------------------
# Calculate the PACF of the simulated data
pacf_values <- pacf(ar1_data, lag.max = 20)

# Extract PACF values
pacf_data <- data.frame(Lag = 1:20, PACF = pacf_values$acf)

# Create a custom plot of the PACF
ggplot(data = pacf_data, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Partial Autocorrelation Function (PACF) of AR(1) with -0.8 coeficient Model",
       x = "Lag", y = "PACF") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

#' 
#' ## ACF-MA1
#' 
## ------------------------------------------------------------------
# Set random seed for reproducibility
set.seed(123)

# Simulate MA(1) model with coefficient 0.7
n <- 200  # Number of time periods
ma1_data <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.7), n = n)

# Create a time series object
ma1_ts <- ts(ma1_data)

# Plot the MA(1) time series
ggplot(data.frame(Time = 1:n, Value = ma1_ts), aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = "Simulated MA(1) Model (Coefficient = 0.7)", x = "Time", y = "Value") +
  theme_minimal()+theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

# Plot the ACF of the MA(1) model
acf(ma1_ts, main = "ACF of Simulated MA(1) Model")+theme(axis.text = element_text(size = 12),  # Adjust font size for axis labels
        plot.title = element_text(size = 14))  # Adjust font size for title

#' 
#' ## PACF-MA1
#' 
## ------------------------------------------------------------------
# Load the required package
library(stats)

# Generate MA(1) data
set.seed(123)
ma_data <- arima.sim(model = list(ma = 0.7), n = 200)

# Compute PACF for MA(1) model
pacf_values <- pacf(ma_data, lag.max = 10, plot = FALSE)

# Plot PACF
plot(pacf_values, type = "h", main = "PACF of MA(1) Model")


#' :::
#' 
#' ## 
#' 
#' **Now let's work with real world data**
#' 
## ------------------------------------------------------------------
pe <- read_csv("data/pe.csv")
pe |> glimpse()  #Inspect data

#' 
#' ## Pick pe_usa
#' 
## ------------------------------------------------------------------
pe_us <- pe |> select(date,pe_usa)
library(ggthemes)

ggplot(pe_us) + aes(x=date,y=pe_usa) + geom_line()+theme_wsj()

#' 
#' ## Identify model
#' 
#' Use SACF and SPACF to choose model(s) and also use auto.arima and see which of these two wins: your judged model or auto.arima
#' 
## ------------------------------------------------------------------
acf(pe_us$pe_usa)
pacf(pe_us$pe_usa)

#' 
#' acf and pacf patterns indicate series is non-stationary. So here we run acf and pacf of difference of the series.
#' 
## ------------------------------------------------------------------
acf(diff(pe_us$pe_usa))
pacf(diff(pe_us$pe_usa))

#' 
#' From these two graphs it seems model is ARIMA(0,1,1)
#' 
## ------------------------------------------------------------------

auto.arima(pe_us$pe_usa)


#' 
#' So our model was ARIMA(0,1,1) while auto.ARIMA is ARIMA(1,1,1) not bad.
#' 
#' # Nonstationary Series
#' 
#' ## Nonstationary Series
#' 
#' -   Introduction:
#' -   Key Questions:
#' -   What is nonstationarity?
#' -   Why is it important?
#' -   How do we determine whether a time series is nonstationary?
#' 
#' ## What is nonstationarity?
#' 
#' Recall from earlier part on stationarity:
#' 
#' -   Covariance stationarity of y implies that, over time, y has:
#' -   Constant mean
#' -   Constant variance
#' -   Co-variance between different observations that do not depend on that time (t), only on the "distance" or "lag" between them (j):
#' 
#' $Cov(Y_t,Y_{tj})= Cov(Y_s,Y_{s+j})= \gamma_j$
#' 
#' ## What is nonstatinarity?
#' 
#' Thus, if any of these conditions does not hold, we say that y is nonstationary:
#' 
#' There is no long-run mean to which the series returns (economic concept of long-term [equilibrium]{style="color:red;"})
#' 
#' The variance is time-dependent. For example, could go to infinity as the number of observations goes to infinity
#' 
#' Theoretical autocorrelations do not decay, sample autocorrelations do so very slowly.  
#' 
#' - - -
#' 
#' Nonstationary series can have a trend: 
#' 
#' -   Deterministic: nonrandom function of time:  
#' 
#' $y_t=\mu+\beta t+u_t$ , where $u_t$ is "iid"
#' 
#' - Example $\beta=0.45$  
#' 
## ------------------------------------------------------------------
library(forecast)
library(tidyverse)
set.seed(1269)
# Generate a time series object
date <- ts(c(1:100))

# Define the trend component
trend <- 0.45 * (1:length(date))


# Generate random noise
noise <- rnorm(length(date), mean = 0, sd = 1)

yt <- trend+noise


# Add the trend component and random noise to the data


# Create a data frame

df <- data.frame(date=date,yt=yt)


ggplot(df)+aes(x=date,y=yt)+geom_line()




#' 
#' 
