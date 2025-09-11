
#################################################
#################################################
###### 5. MULTIEQUATION TIME-SERIES MODELS ######
#################################################
#################################################
# Load pacman
library(pacman)

# Use p_load to install (if necessary) and load the required packages
p_load(vars, mFilter, tseries, TSstudio, forecast, tidyverse)

### PAGE 134
library("gdata")
library(readxl)
terrorism = read_excel("data/terrorism.xlsx")

# Generate the date sequence from 1970 Q1 to 2010 Q4
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2010-12-31")
date_seq <- seq(from = start_date, to = end_date, by = "quarter")

# Add the date sequence to the data frame
terrorism$Date <- date_seq

# Reorder columns to place Date first
ts_terrorism <- terrorism[, c("Date", "Domestic", "Transnational")]




### FIGURE 5.1
library(xts)

## Convert to ts_terrorism to xts
library(xts)

# Convert the data frame to an xts object
terrorism_xts <- xts(ts_terrorism[, -1], order.by = ts_terrorism$Date)

# View the xts object
head(terrorism_xts)

# Plot the xts object
par(mfrow = c(2, 1))  # Set up the plot area to have 2 rows and 1 column
plot(terrorism_xts$Domestic, main = "Domestic Time Series", col = "steelblue4", lwd = 2)
plot(terrorism_xts$Transnational, main = "Transnational Time Series", col = "darkred", lwd = 2)
## Plot Domestic and Transnational

autoplot(terrorism_xts$Domestic)+ theme_minimal()

autoplot(terrorism_xts$Transnational) + theme_minimal()

# Plot settings
par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))

# Plot Domestic
plot(index(terrorism_xts), terrorism_xts$Domestic, type = "l", las = 1, xaxs = "i", yaxs = "i", xlab = "", ylab = "", tck = .02, main = "Domestic", col = "steelblue4", ylim = c(0, 400))

# Plot Transnational
plot(index(terrorism_xts), terrorism_xts$Transnational, type = "l", las = 1, xaxs = "i", yaxs = "i", xlab = "", ylab = "", tck = .02, main = "Transnational", col = "steelblue4", ylim = c(0, 400))

# Index for a specific date (e.g., 1973 Q2)
ind <- which(index(terrorism_xts) == as.Date("1973-04-01"))

# Create indicators for pure.jump, pulse, grad.change, and prol.pulse
terrorism_xts$pure.jump <- 1
terrorism_xts$pure.jump[1:ind] <- 0
terrorism_xts$pulse <- 0
terrorism_xts$pulse[ind] <- 1
terrorism_xts$grad.change <- 0
terrorism_xts$grad.change[ind:(ind+3)] <- seq(0.25, 1, by = 0.25)
terrorism_xts$prol.pulse <- 0
terrorism_xts$prol.pulse[ind:(ind+3)] <- seq(1, 0.25, by = -0.25)

# Plot the indicators
space <- 20
par(mfrow = c(2, 2))
plot(index(terrorism_xts)[1:space], terrorism_xts$pure.jump[1:space], type = "h", las = 1, xaxs = "i", xlab = "", ylab = "", tck = .02, main = "Pure Jump", col = "steelblue4", ylim = c(0, 1))
plot(index(terrorism_xts)[1:space], terrorism_xts$pulse[1:space], type = "h", las = 1, xaxs = "i", xlab = "", ylab = "", tck = .02, main = "Pulse", col = "steelblue4", ylim = c(0, 1))
plot(index(terrorism_xts)[1:space], terrorism_xts$grad.change[1:space], type = "h", las = 1, xaxs = "i", xlab = "", ylab = "", tck = .02, main = "Gradually Changing", col = "steelblue4", ylim = c(0, 1))
plot(index(terrorism_xts)[1:space], terrorism_xts$prol.pulse[1:space], type = "h", las = 1, xaxs = "i", xlab = "", ylab = "", tck = .02, main = "Prolonged Pulse", col = "steelblue4", ylim = c(0, 1))

### INTERPRETATION
library("forecast")
auto.arima(terrorism$Transnational,xreg=terrorism$pure.jump,ic="bic")
auto.arima(terrorism$Transnational,xreg=terrorism$pulse,ic="bic")
auto.arima(terrorism$Transnational,xreg=terrorism$grad.change,ic="bic")
auto.arima(terrorism$Transnational,xreg=terrorism$prol.pulse,ic="bic")

auto.arima(terrorism$Domestic,xreg=terrorism$pure.jump,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$pulse,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$grad.change,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$prol.pulse,ic="bic")

auto.arima(terrorism,xreg=terrorism$pure.jump,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$pulse,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$grad.change,ic="bic")
auto.arima(terrorism$Domestic,xreg=terrorism$prol.pulse,ic="bic")


### PAGE 278
terrorism = read.xls("/Users/user/Google Drive/Website/Book/Enders/italy.xls")
terrorism$ENTRY = seq(1971.25,by=0.25,length.out=nrow(terrorism))

ind1 = which(terrorism$ENTRY=="1971.25")
ind2 = which(terrorism$ENTRY=="1989")

par(mfrow=c(2,1))
plot(terrorism$ENTRY,terrorism$Attkit,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="",col="steelblue4",yaxs="i")
abline(h=0)
acf1 = acf(terrorism$Attkit[ind1:ind2])
acf1

library("MTS")
ccor = ccm(terrorism[,-1],level=TRUE)
ccor$ccm[3,]

plag = 3
X = embed(terrorism$Attkit,plag+1)
summary(lm(terrorism$Slitaly[-c(1:plag)]~X))
summary(lm(terrorism$Slitaly[-c(1:plag)]~X[,-ncol(X)]))
summary(lm(terrorism$Slitaly[-c(1:plag)]~X[,-c(1,ncol(X))]))
summary(lm(terrorism$Slitaly[-c(1:plag)]~X[,3]))
summary(lm(terrorism$Slitaly[-c(1:plag)]~X[,2]))

lm1 = summary(lm(terrorism$Slitaly[-c(1:plag)]~0+X[,-c(1,ncol(X))]))
adl.res=lm1$residuals
plot(terrorism$ENTRY[-c(1:3)],adl.res,type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,main="",col="steelblue4",yaxs="i",ylim=c(-.3,.3))
abline(h=0,lty=2)
acf.res = acf(adl.res)
acf.res

plot(terrorism$Slitaly,type="l")


### PAGE 310
library("vars")
terrorism = read.xls("/Users/user/Google Drive/Website/Book/Enders/TERRORISM.xls")
terrorism$ENTRY = seq(1970,by=0.25,length.out=nrow(terrorism))

library("urca")
colnames(terrorism)
adf.dom = ur.df(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),2],type="drift",lag=2)
adf.tra = ur.df(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),3],type="drift",lag=1)
adf.dom@cval
adf.dom
adf.tra

ers.dom = ur.ers(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),2],model="constant",lag=2)
ers.tra = ur.ers(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),3],model="constant",lag=1)
ers.dom@cval
ers.dom
ers.tra

VARselect(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),-1],type="const") # AIC suggests 3 lags
var.terror = VAR(terrorism[-c(1:which(terrorism$ENTRY=="1979.25")),-1],p=3)
summary(var.terror)

fevd.terror = fevd(var.terror,n.ahead=12)
fevd.terror
plot(fevd.terror)

par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(irf(var.terror),col=1,las=1,xaxs="i",xlab="",ylab="")

k=ncol(terrorism[,-1])
amat = diag(k)
diag(amat) = NA
amat[1,2] = NA
amat # the coefficient b[2,1] is set equal to zero
svar.terror = SVAR(var.terror, estmethod="direct", Amat = amat) 
svar.terror
summary(svar.terror)
plot(irf(svar.terror),col=1,las=1,xaxs="i",xlab="",ylab="")


### PAGE 325
terrorism = read.xls("/Users/user/Google Drive/Website/Book/Enders/Enders_Holt.xls")
terrorism$ENTRY = seq(1974.25,by=0.25,length.out=nrow(terrorism))
k = ncol(terrorism[,-1])

par(mfcol = c(2,2), oma = c(0,0,1,0) + 0.2, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
for (i in 1:k) {
   plot(terrorism$ENTRY,terrorism[,i+1],type="l",las=1,xaxs="i",xlab="",ylab="",tck=.02,col="steelblue4",yaxs="i",main=colnames(terrorism)[i+1])
   abline(h=0,lty=2)
}
var.enders = VAR(na.omit(terrorism[,-1]),p=11)
summary(var.enders)

amat = diag(k)
diag(amat) = NA
amat[2,1] = NA
amat[4, ] = NA
amat
svar.enders = SVAR(var.enders, estmethod="direct", Amat = amat) 
svar.enders
plot(irf(svar.enders,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")


### PAGE 331
terrorism = read.xls("/Users/user/Google Drive/Website/Book/Enders/Exrates.xls")
head(terrorism)

terrorism$loge_ca = log(terrorism$e_uk)
terrorism$logr_ca = terrorism$loge_ca-log(terrorism$p_uk)+log(terrorism$p_us)
dloge_ca = c(diff(terrorism$loge_ca))
dlogr_ca = c(diff(terrorism$logr_ca))

terrorism$date = as.yearqtr(terrorism$DESCRIPTOR)

X1 = embed(dloge_ca,2)
X2 = embed(terrorism$loge_ca,2)
summary(lm(X1[,1]~X2[-1,2]+X1[,-1]))

df1 = ur.df(dloge_ca,type="drift")
df1@testreg

df = terrorism.frame(r_uk=logr_ca,e_uk=loge_ca)
VAR.PPP = VAR(df,p=3)
summary(VAR.PPP)
fevd(VAR.PPP)

k = ncol(df)
amat = diag(k)
diag(amat) = NA
amat[2,1] = NA
amat
SVAR.PPP = SVAR(VAR.PPP, estmethod = "scoring", Amat = amat, Bmat = NULL,
                max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
fevd(SVAR.PPP)

plot(irf(SVAR.PPP,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")

BQ.PPP = BQ(VAR.PPP)
fevd(BQ.PPP)
plot(irf(BQ.PPP,boot=FALSE),col=1,las=1,xaxs="i",xlab="",ylab="")



### PAGE 340
terrorism = read.xls("/Users/user/Google Drive/Website/Book/Enders/QUARTERLY.xls")
dinfl = diff(log(terrorism$CPI))
dlip = diff(log(terrorism$IndProd))
Y = cbind(dlip,dinfl)
VAR.PPP = VAR(Y,p=3)
summary(VAR.PPP)
fevd(VAR.PPP)

SBQ.PPP = BQ(VAR.PPP)
IRF.PPP = irf(SBQ.PPP,boot=TRUE,cumulative=T,n.ahead=25,ortho=T,las=1)

par(mfcol = c(2,1), oma = c(0,0,1,0) + 1, mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(IRF.PPP$irf$dinfl[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-1,6),yaxs="i")
lines(IRF.PPP$Lower$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dinfl[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dinfl[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dinfl[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

plot(IRF.PPP$irf$dlip[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-6,6),yaxs="i")
lines(IRF.PPP$Lower$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dlip[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dlip[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dlip[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)


k = ncol(Y)
amat = diag(k)
diag(amat) = NA
amat[1,2] = NA
amat # inflation is not influencing GDP
SVAR.PPP = SVAR(VAR.PPP, estmethod = "direct", Amat = amat, Bmat = NULL,
                max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
IRF.PPP = irf(SVAR.PPP,boot=TRUE,cumulative=T,n.ahead=25,ortho=T,las=1)
plot(IRF.PPP$irf$dinfl[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(-4,8),yaxs="i")
lines(IRF.PPP$Lower$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dinfl[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dinfl[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dinfl[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dinfl[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

plot(IRF.PPP$irf$dlip[,1]/0.01,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue4",ylim=c(0,4),yaxs="i")
lines(IRF.PPP$Lower$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$Upper$dlip[,1]/0.01,col="steelblue4",lty=2)
lines(IRF.PPP$irf$dlip[,2]/0.005,type="l",las=1,xaxs="i",xlab="",ylab="",tck=0.02,col="steelblue1")
lines(IRF.PPP$Lower$dlip[,2]/0.005,col="steelblue1",lty=2)
lines(IRF.PPP$Upper$dlip[,2]/0.005,col="steelblue1",lty=2)
abline(h=0)

### END
