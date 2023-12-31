##========================================================#

##https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/
  # Quantitative ALM, Financial Econometrics & Derivatives 
  # ML/DL using R, Python, Tensorflow by Sang-Heon Lee 
  #
  # https://kiandlee.blogspot.com
  #——————————————————-#
  # Vector Autoregressive Model
  #========================================================#
  
graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

library(urca) # ca.jo, denmark
library(vars) # vec2var
library(tidyverse) # data manipulation
library(tsDyn)
library(forecast)
library(xts)
#========================================================
# Data
#========================================================

# forecasting horizon
nhor <-  12 

#——————————————-
# quarterly data related with money demand
#——————————————-
# LRM : logarithm of real money M2 (LRM)
# LRY : logarithm of real income (LRY)
# LPY : logarithm of price deflator (LPY)
# IBO : bond rate (IBO)
# IDE : bank deposit rate (IDE)
# the period 1974:Q1 - 1987:Q3
#——————————————-

# selected variables
data(denmark)
denmark |> dim()
denmark |> colnames()
df.lev <- denmark |> select(LRM, LRY, IBO, IDE)
#df.lev <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
m.lev  <-  as.matrix(df.lev)
nr_lev <-  nrow(df.lev)
nr_lev
# quarterly centered dummy variables
dum_season <-  data.frame(yyyymm = denmark$ENTRY)

substr.q   <-  as.numeric(substring(denmark$ENTRY, 6,7))

substr.q |> head()
# Extract Q2 from substr.q

dum_season$Q2 <-  (substr.q==2)-1/4
dum_season$Q3 <-  (substr.q==3)-1/4
dum_season$Q4 <-  (substr.q==4)-1/4
dum_season    <-   dum_season[,-1]

# Draw Graph for each of the series in four panels


str.main <- c("LRM=ln(real money M2)", "LRY=ln(real income)", "IBO=bond rate", "IDE=bank deposit rate")

x11(width=12, height = 6); 
par(mfrow=c(2,2), mar=c(5,3,3,3))
for(i in 1:4) {
  matplot(m.lev[,i], axes=FALSE,
          type=c("l"), col = c("blue"), 
          main = str.main[i])
  
  axis(2) # show y axis
  
  # show x axis and replace it with 
  # an user defined sting vector
  axis(1, at=seq_along(1:nrow(df.lev)),
       labels=denmark$ENTRY, las=2)
}

#========================================================
# VAR model in level
#========================================================

# lag length
#df.lev <- df.lev[, sapply(df.lev, is.numeric)]
df.lev <- ts(as.matrix(df.lev))
VARselect(df.lev, lag.max = 4,
          type = "const", season = 4)
# estimation
var.model_lev <-  VAR(df.lev, p = 2, 
                     type = "const", season = 4)

df.lev |> head()
summary(var.model_lev)
# forecast of lev data
var.pred <-   predict(var.model_lev, n.ahead = nhor)
x11(); par(mai=rep(0.4, 4)); plot(var.pred)
x11(); par(mai=rep(0.4, 4)); fanchart(var.pred)


#========================================================
# VAR model in difference using vars
#========================================================

# 1st differenced data
df.diff <-  diff(as.matrix(df.lev), lag = 1)
colnames(df.diff) <-  c("dLRM", "dLRY", "dIBO", "dIDE")
m.diff <- as.matrix(df.diff)

# lag length
VARselect(df.diff, lag.max = 4,
          type = "const", season = 4)

# estimation
vare_diff <-  VAR(df.diff, p = 1, 
                 type = "const", season = 4)

# forecast of differenced data
varf_diff <- predict(vare_diff, n.ahead = nhor)
x11(); par(mai=rep(0.4,4)); plot(varf_diff)
x11(); par(mai=rep(0.4,4)); fanchart(varf_diff)

# recover lev forecast
m.varf_lev_ft <- rbind(m.lev, matrix(NA, nhor, 4))
m.ft_df <- do.call(cbind,lapply(varf_diff$fcst, 
                                function(x) x[,"fcst"]))

# growth to level
for(h in (nr_lev+1):(nr_lev+nhor)) {
  hf <- h - nr_lev
  m.varf_lev_ft[h,] <- m.varf_lev_ft[h-1,] + m.ft_df[hf,]
}

# Draw Graph
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))

for(i in 1:4) {
  df <- m.varf_lev_ft[,i]
  matplot(df, type=c("l"), col = c("blue"), 
          main = str.main[i]) 
  abline(v=nr_lev, col="blue")
}


#========================================================
# VAR model in difference using tsDyn
#========================================================

linevare_diff <- lineVar(data = df.lev, lag = 1, include = "const",
                         model = "VAR", I = "diff", beta = NULL, exogen = dum_season)

# check if both models (vars & tsDyn) yield same coefficients
linevare_diff 
do.call(rbind,lapply(vare_diff$varresult, 
                     function(x) x$coefficients))

# quarterly centered dummy variables for forecast
dumf_season <- rbind(tail(dum_season,4),
                     tail(dum_season,4),
                     tail(dum_season,4))
# forecast
linevarf_diff <- predict(linevare_diff, n.ahead = nhor, 
                         exoPred = dumf_season) 
# Draw Graph
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))

df <- rbind(df.lev, linevarf_diff)
df |> head()



for(i in 1:4) {
  matplot(df[,i], type=c("l"), col = c("blue"), 
          main = str.main[i]) 
  abline(v=nr_lev, col="blue")
}

