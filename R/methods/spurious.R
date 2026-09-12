library(tidyverse) # An umbrella package that installs the tidyverse packages

library(readxl) # To read excel files

library(broom) # To tidy up the regression output

library(moderndive) # To get regression points

library(ggfortify) # To plot regression diagnostics

library(ggResidpanel) # To plot regression diagnostics

T <- 1000
set.seed(1357)
y <- numeric(T)
vy <- rnorm(T)
for (t in 2:T){
  y[t] <- y[t-1]+vy[t]
}

set.seed(4365)
x <- numeric(T)
vx <- rnorm(T)
for (t in 2:T){
  x[t] <- x[t-1]+vx[t]
}
y <- ts(y[300:1000])
x <- ts(x[300:1000])
ts.plot(y,x, ylab="y and x")

ts.plot(rnorm(1000))
# barplot of residuals
summary(lm(y~x))
resid <- residuals(lm(y~x))
barplot(resid, ylab="Residuals", xlab="Observation", main="Residuals of y~x")


resid_panel(lm(y~x))
