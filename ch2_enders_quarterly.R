library(tidyverse)
library("zoo")
library(xts)
library(forecast)


### PAGE 88
data = read_excel("data/quarterly.xlsx")

data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill
# 
# spread_xts <- xts(data$spread,order.by=data$DATE)
# colnames(spread_xts) <- c("spread")
# ## Make an autoplot and mean line of spread
# 
# autoplot(spread_xts) 
# 
# autoplot(diff(spread_xts)) 

par(mfrow=c(2,1))
plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

par(mfrow=c(2,1))
acf(data$spread,lag=12,tck=.02,xlab="",ylab="",main="",las=1)
pacf(data$spread,lag=12,tck=.02,xlab="",ylab="",main="",las=1)


### PAGE 91

# Fit AR model on xts
library(xts)
library(lmtest)
library(modelsummary)
library(dynlm)
# Assuming xts object is named 'my_xts'
ar7 <- Arima(data$spread,order = c(7,0,0)) 
ar6 <- Arima(data$spread,order=c(6,0,0))
ar2 <- Arima(data$spread,order=c(2,0,0))

# Fit ARIMA with 1, 2, and 7 AR lags
# Create an order object with multiple AR lags

# Fit ARIMA model with partial AR lags
ar127 <- arima(data$spread, 
                     xreg = data.frame(ar1 = lag(data$spread, 1), ar2 = lag(data$spread, 2), ar7 = lag(data$spread, 7)))

msummary(ar127)
arma11 <- Arima(data$spread,order=c(1,0,1))


arma21 <- Arima(data$spread,order=c(2,0,1))

# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Assuming your time series data is in the vector 'spread_data'
# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Fit ARIMA model with AR(1), MA(1), and MA(7)
# Fit ARIMA model with AR(1), MA(1), and MA(7)
Arima(data$spread, order = c(1, 0, c(1, 0, 7)))

# Fit ARIMA model with AR(1), MA(1), and MA(7)
arima1_17 <- Arima(data$spread, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period = NA),
                     xreg = cbind(lag(data$spread, 1), lag(data$spread, 7)))
msummary(arima1_17)

models <- list(ar7,ar6,ar2,ar127, arma11,arma21, arima1_17)

# Names you want to assign to each model
model_names <- c("ar7", "ar6", "ar2")

# Use add_models to add models with custom names
ms <- modelsummary::add_models(NULL, models, model.names = model_names)

# Assuming you have a list of time series models named 'models'
msummary(models)

# Create a modelsummary table
summary_table <- modelsummary(
  models,
  stars = TRUE,  # Display stars for significance
  gof_omit = c("AIC", "BIC"),  # Omit AIC and BIC from goodness-of-fit statistics
  coef_map = list(ar7 = "AR(7)", ar6 = "AR(6)", ar2 = "AR(2)", ar127 = "AR(1,7)", 
                  arma11 = "ARMA(1,1)", arma21 = "ARMA(2,1)", arima1_17 = "ARIMA(1,1,7)")
)



# Create a list to store the ARIMA models
arima_models <- models

# Fit 7 ARIMA models and store them in the list
for (i in 1:7) {
  arima_models[[i]] <- Arima(data$spread, order = c(i, 0, 0))
}

# Perform Ljung-Box test for each model at lags 4, 8, and 12
lags <- c(4, 8, 12)
for (lag in lags) {
  cat("Ljung-Box test at lag", lag, ":\n")
  for (i in 1:7) {
    cat("Model", i, ":")
    print(Box.test(arima_models[[i]]$residuals, lag = lag, type = "Ljung-Box"))
  }
}
library(broom)

# Create a list to store the test results
test_results <- list()

# Perform Ljung-Box test for each model at lags 4, 8, and 12
lags <- c(4, 8, 12)
for (lag in lags) {
  for (i in 1:7) {
    test <- tidy(Box.test(arima_models[[i]]$residuals, lag = lag, type = "Ljung-Box"))
    test$model <- paste("Model", i)
    test$lag <- lag
    test_results[[length(test_results) + 1]] <- test
  }
}

# Combine the test results into a data frame
test_df <- do.call(rbind, test_results)

# Print the test results
test_df 
as_tibble(test_df) |> select(lag, everything())

# Print the summary table
print(summary_table)


library(broom)



# Print the model summary
modelsummary(model_summary)
# Print the summary of the model
library(tseries)
arma(data$spread, lag=list(ar=1,ma=c(1,7))) 


 arima(data$spread, order = c(1, 0, c(1, 7)))
msummary(arima_model)
# Fit ARIMA model with the specified order
arima_model <- arima(spread_data, order = ar_order)

# Summarize AR model using modelsummary
summary_table <- modelsummary(ar_model)
print(summary_table)
### TABLE 2.4
# Install and load the fracdiff package
#install.packages("fracdiff")
library(fracdiff)

# Now you should be able to use the arfimaspec function
spec.ar7 = arfimaspec(mean.model = list(armaOrder = c(7, 0), include.mean = TRUE))

spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE))
fit.ar7 = arfimafit(spec=spec.ar7,data=data$spread)
fit.ar7
res.ar7 = fit.ar7@fit$residuals
Box.test(res.ar7,lag=4,type="Ljung-Box")
Box.test(res.ar7,lag=8,type="Ljung-Box")
Box.test(res.ar7,lag=12,type="Ljung-Box")


spec.ar6 = arfimaspec(mean.model=list(armaOrder=c(6,0),include.mean=TRUE))
fit.ar6 = arfimafit(spec=spec.ar6,data=data$spread)
fit.ar6
res.ar6 = fit.ar6@fit$residuals
Box.test(res.ar6,lag=4,type="Ljung-Box")
Box.test(res.ar6,lag=8,type="Ljung-Box")
Box.test(res.ar6,lag=12,type="Ljung-Box")


spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
fit.ar2 = arfimafit(spec=spec.ar2,data=data$spread)
fit.ar2
res.ar2 = fit.ar2@fit$residuals
Box.test(res.ar2,lag=4,type="Ljung-Box")
Box.test(res.ar2,lag=8,type="Ljung-Box")
Box.test(res.ar2,lag=12,type="Ljung-Box")


spec.ar27 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE),
                       fixed.pars=list(ar3=0,ar4=0,ar5=0,ar6=0))
fit.ar27 = arfimafit(spec=spec.ar27,data=data$spread)
fit.ar27
res.ar27 = fit.ar27@fit$residuals
Box.test(res.ar27,lag=4,type="Ljung-Box")
Box.test(res.ar27,lag=8,type="Ljung-Box")
Box.test(res.ar27,lag=12,type="Ljung-Box")


spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE))
fit.arma11 = arfimafit(spec=spec.arma11,data=data$spread)
fit.arma11
res.arma11 = fit.arma11@fit$residuals
Box.test(res.arma11,lag=4,type="Ljung-Box")
Box.test(res.arma11,lag=8,type="Ljung-Box")
Box.test(res.arma11,lag=12,type="Ljung-Box")


spec.arma21 = arfimaspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE))
fit.arma21 = arfimafit(spec=spec.arma21,data=data$spread)
fit.arma21
res.arma21 = fit.arma21@fit$residuals
Box.test(res.arma21,lag=4,type="Ljung-Box")
Box.test(res.arma21,lag=8,type="Ljung-Box")
Box.test(res.arma21,lag=12,type="Ljung-Box")


spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27 = arfimafit(spec=spec.arma27,data=data$spread,solver="nlminb")
fit.arma27
res.arma27 = fit.arma27@fit$residuals
Box.test(res.arma27,lag=4,type="Ljung-Box")
Box.test(res.arma27,lag=8,type="Ljung-Box")
Box.test(res.arma27,lag=12,type="Ljung-Box")


### PAGE 94
data$DATE[163]
data$spread[163] # Typo in the book. Instead of 0.04 it is written 0.4

which(data$DATE=="2000 Q2")
data$DATE[162]
actual = data$spread[-c(1:162)]

### 1-STEP AHEAD ROLLING WINDOW FORECAST
fore.arma27 = fore.ar7 = NULL
for (i in 1:50){
  fit.ar7=arfimafit(spec=spec.ar7,data=data$spread[1:(162+i-1)],solver="nlminb")
  fore.ar7[i]=arfimaforecast(fit.ar7,n.ahead=1)@forecast$seriesFor
  fit.arma27=arfimafit(spec=spec.arma27,data=data$spread[1:(162+i-1)],solver="gosolnp")
  fore.arma27[i]=arfimaforecast(fit.arma27,n.ahead=1)@forecast$seriesFor
}
mean(fore.ar7)
mean(fore.arma27)
var(fore.ar7)
var(fore.arma27)

### FORECAST ERROR and FORECAST ERROR VARIANCE
fore.error.ar7 = fore.ar7-actual
fore.error.arma27 = fore.arma27-actual
var(fore.error.ar7)
var(fore.error.arma27)

### PAGE 95
summary(lm(actual~fore.ar7))
summary(lm(actual~fore.arma27))


### GRANGER-NEWBOLD TEST
x = fore.error.ar7+fore.error.arma27
z = fore.error.ar7-fore.error.arma27
corxz = cor(z,x)
corxz/( sqrt( (1-corxz^2)/(length(fore.error.ar7)-1)))

### DIEBOLD-MARIANO TEST
dm.test(fore.error.ar7,fore.error.arma27, h=1, power=4)

d = (fore.error.ar7)^4-(fore.error.arma27)^4
DM = mean(d)/(var(d)/(length(d)-1))^0.5
DM
acf.d = acf(d)
acf.d


### SEASONALITY
### PAGE 98
par(mfrow=c(2,1))
plot(data$DATE,data$M1NSA,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4",ylim=c(0,2500))

mg = 100*diff(log(data$M1NSA))
plot(data$DATE[-1],mg,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4",ylim=c(-4,8))
abline(h=0)

### PANEL A
par(mfrow=c(2,1))
acf.mg = acf(mg,lag=25,tck=.02,xlab="",ylab="",main="")
acf.mg
pacf.mg = pacf(mg,lag=25,tck=.02,xlab="",ylab="",main="")
pacf.mg

par(mfrow=c(1,1))
smg = diff(diff(log(data$M1NSA),4))
plot(smg,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="",tck=0.02,col="steelblue4")
abline(h=0)

### PANEL B
par(mfrow=c(2,1))
acf.mg = acf(smg,lag=25,tck=.02,xlab="",ylab="",main="")
acf.mg
pacf.mg = pacf(smg,lag=25,tck=.02,xlab="",ylab="",main="")
pacf.mg

spec.arma14 = arfimaspec(mean.model=list(armaOrder=c(4,4),include.mean=TRUE),
                         fixed.pars=list(ar2=0,ar3=0,ar4=0,ma1=0,ma2=0,ma3=0))
fit.arma14 = arfimafit(spec=spec.arma14,data=smg,solver="nlminb")
fit.arma14
res.arma14 = fit.arma14@fit$residuals
Box.test(res.arma14,lag=4,type="Ljung-Box")
Box.test(res.arma14,lag=8,type="Ljung-Box")
Box.test(res.arma14,lag=12,type="Ljung-Box")




### PARAMETER INSTABILITY
data = read.xls("/Users/user/Google Drive/Website/Book/Enders/QUARTERLY.xls")
library("zoo")
data$DATE = as.yearqtr(data$DATE)
data$spread = data$r5-data$Tbill

par(mfrow=c(2,1))
plot(data$DATE,data$spread,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="The Interest Rate Spread",tck=0.02,col="steelblue4",ylim=c(-2,4))
abline(h=0)
plot(data$DATE[-1],diff(data$spread),type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",main="First-Difference of The Spread",tck=0.02,col="steelblue4",ylim=c(-3,3))
abline(h=0)

spec.arma27 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                         fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.pre = arfimafit(spec=spec.arma27,data=data$spread[1:88])
fit.arma27.pre
fit.arma27.post = arfimafit(spec=spec.arma27,data=data$spread[89:193],solver="gosolnp")
fit.arma27.post
sum(fit.arma27.pre@fit$residuals^2)
sum(fit.arma27.post@fit$residuals^2)

data$Indicator = 0
data$Indicator[which(data$DATE=="1982 Q1"):nrow(data)]=1
data$Indicator

spec.arma27.ex = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE,external.regressors=matrix(data$Indicator)),
                            fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
fit.arma27.ex = arfimafit(spec=spec.arma27.ex,data=data$spread)
fit.arma27.ex


### ENDOGENOUS BREAKS
### PAGE 104
Break = read.xls("/Users/user/Google Drive/Website/Book/Enders/y_break.xls")
br = Break[,-1]
par(mfrow=c(1,1))

plot(br,type="l",xax="i",las=1,xaxs="i",tck=0.02,col="steelblue4",xlab="",ylab="")
spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE))
fit.ar1 = arfimafit(spec.ar1,data=br)
fit.ar1
fit.ar1@fit$coef[1]*(1-fit.ar1@fit$coef[2]) # arma(br, order=c(1,0)) # both are correct however one shows the unconditional mean and the other the intercept

Break$Indicator=1
Break$Indicator[1:100]=0
break_y = embed(as.matrix(Break),2)
break_y = break_y[,-c(1,4,6)]

### PAGE 108
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]))
summary(lm(break_y[,1]~break_y[,3]+break_y[,2]+I(break_y[,3]*break_y[,2])))

arma(br,c(1,0,0))
arma(br[c(1:100)],c(1,0,0))
arma(br[-c(1:100)],c(1,0,0))


# AR1
# CUMSUM PLOT
forecasts = NULL
for (i in 1:(length(br)-2)){
  model = arima(br[1:(2+i-1)],c(1,0,0))
  forecasts[i] = forecast(model,h=1)$mean # Gives the one-step forecast
}
res = br[-c(1:2)]-forecasts

# INTERCEPT PLOT
slope.est=intercept.est=slope.sigma.est=int.sigma.est=NULL
for (i in 1:(length(br)-3)){
  y.br = embed(br[1:(3+i)],2)
  model=lm(y.br[,1]~y.br[,-1])
  intercept.est[i]=coef(model)[1]
  slope.est[i]=coef(model)[2]
  int.sigma.est[i]=summary(model)$coefficients[1,2]
  slope.sigma.est[i]=summary(model)$coefficients[2,2]
}
int.upper.band=intercept.est+2*int.sigma.est
int.lower.band=intercept.est-2*int.sigma.est
slope.upper.band=slope.est+2*slope.sigma.est
slope.lower.band=slope.est-2*slope.sigma.est

par(mfrow=c(3,1))
plot(intercept.est, type="l",xlab="",ylab="",ylim=c(-2,4),xaxs="i",las=1,tck=.02,main="Intercept")
lines(int.upper.band, col="steelblue4")
lines(int.lower.band, col="steelblue4")
abline(h=0,lty=2)

plot(slope.est, type="l",xlab="",ylab="",ylim=c(-2,2),xaxs="i",las=1,tck=.02,main="Autoregressive Parameter")
lines(slope.upper.band, col="steelblue4")
lines(slope.lower.band, col="steelblue4")
abline(h=0,lty=2)


CUMSUM=NULL
for (i in 1:length(br)){
  CUMSUM=c(CUMSUM,sum(res[1:i])/sd(res))
}

lower=upper=NULL
for (i in 1:length(br)){
  upper[i]= 0.948*(length(br)^0.5)+2*(i-1)*(length(br)^-0.5)
  lower[i]=-0.948*(length(br)^0.5)-2*(i-1)*(length(br)^-0.5)
}

plot(CUMSUM, type="l", xlim=c(0,150),ylim=c(-40,50),las=1,xaxs="i",xlab="",ylab="",tck=0.02,main="The CUMSUM Test")
lines(upper, col="steelblue4",lty=2)
lines(lower, col="steelblue4",lty=2)
abline(h=0,lty=2)



### COMBINING FORECASTS
data$spread
fore = NULL
space=51
### ONE STEP AHEAD OUT-OF-SAMPLE FORECAST (last on is out of analysed sample)
for (i in 1:space) {
  sample = data$spread[1:(length(data$spread)-space+i)]
  
  spec.ar7 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE))
  fit.ar7 = arfimafit(spec=spec.ar7,data=sample,solver="gosolnp")
  fore.ar7 = arfimaforecast(fit.ar7,n.ahead=1)@forecast$seriesFor
  
  spec.ar6 = arfimaspec(mean.model=list(armaOrder=c(6,0),include.mean=TRUE))
  fit.ar6 = arfimafit(spec=spec.ar6,data=sample,solver="gosolnp")
  fore.ar6 = arfimaforecast(fit.ar6,n.ahead=1)@forecast$seriesFor
  
  spec.ar2 = arfimaspec(mean.model=list(armaOrder=c(2,0),include.mean=TRUE))
  fit.ar2 = arfimafit(spec=spec.ar2,data=sample,solver="gosolnp")
  fore.ar2 = arfimaforecast(fit.ar2,n.ahead=1)@forecast$seriesFor
  
  spec.ar127 = arfimaspec(mean.model=list(armaOrder=c(7,0),include.mean=TRUE),
                          fixed.pars=list(ar3=0,ar4=0,ar5=0,ar6=0))
  fit.ar127 = arfimafit(spec=spec.ar127,data=sample,solver="gosolnp")
  fore.ar127 = arfimaforecast(fit.ar127,n.ahead=1)@forecast$seriesFor
  
  spec.arma11 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE))
  fit.arma11 = arfimafit(spec=spec.arma11,data=sample,solver="gosolnp")
  fore.arma11 = arfimaforecast(fit.arma11,n.ahead=1)@forecast$seriesFor
  
  spec.arma21 = arfimaspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE))
  fit.arma21 = arfimafit(spec=spec.arma21,data=sample,solver="gosolnp")
  fore.arma21 = arfimaforecast(fit.arma21,n.ahead=1)@forecast$seriesFor
  
  spec.arma217 = arfimaspec(mean.model=list(armaOrder=c(2,7),include.mean=TRUE),
                            fixed.pars=list(ma2=0,ma3=0,ma4=0,ma5=0,ma6=0))
  fit.arma217 = arfimafit(spec=spec.arma217,data=sample,solver="gosolnp")
  fore.arma217 = arfimaforecast(fit.arma217,n.ahead=1)@forecast$seriesFor
  
  fore = cbind(fore,c(fore.ar7,fore.ar6,fore.ar2,fore.ar127,fore.arma11,fore.arma21,fore.arma217))
}

### EQUALLY WEIGHTED FORECAST
sum((1/nrow(fore))*fore[,51])

actual = data$spread[(length(data$spread)-49):length(data$spread)]
fore.error = t(fore[,-ncol(fore)])-actual


### OPTIMAL WEIGHTED FORECAST
weight%*%fore[,51]

plot(c(actual,NA),type="l",xlab="",ylab="",main="",las=1,xaxs="i",tck=0.02,ylim=c(min(fore),max(fore)),lty=2)
for (i in 1:nrow(fore)) {
  lines(fore[i,],col=paste0("grey",round(100/10*i)))
}
lines(t(fore)%*%weight,col="steelblue4",lwd=2)

lm1 = summary(lm(actual~t(fore[,-ncol(fore)])+0))
lm1
ind1 = which(lm1$coefficients[,1]<0)
lm2 = summary(lm(actual~t(fore[-ind1,-ncol(fore)])+0))
lm2
sum(lm2$coefficients[,1]*t(fore[-ind,ncol(fore)]))

forecast.error.variance.opt = apply(fore.error[,-ind],2,var)
weight.opt = forecast.error.variance.opt/sum(forecast.error.variance.opt)
weight.opt # optimal weights

opt.fore = weight.opt%*%fore[-ind,]
lines(c(opt.fore),col="steelblue1",lwd=2,lty=2)

### END
