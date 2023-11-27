library(tidyverse)
library(dynlm)
library(haven)
library(fpp3)
library(forecast)

cons_inc<-read_dta("docs/data/Consumption_Income_Pakistan.dta")
library(tseries)
cons_inc |> head()
cons_inc |> tail()
# cons_inc$year <- as_date(cons_inc$year, format="%Y")

#cons_inc$year<-as_date(cons_inc$year)
cons_inc<-cons_inc %>% mutate(Trend=1:52)


cons_inc<-ts(cons_inc,start = 1967, end=2018)

#cons_inc<-as_tibble(con_inc)
ggplot(cons_inc)+aes(x=year)+geom_line(aes(y = C), color = "darkred",size=0.8) + 
  geom_line(aes(y = Y), color="steelblue", linetype="twodash",size=0.8)+labs(x="Income per capita",y="Consumption per captia",title = "Consumption Income pattern over time for Pakistan")+theme_minimal()

library(broom)
library(moderndive)
con_mod<-lm(C~Y,data = cons_inc)
regression_points <- get_regression_points(con_mod)
regression_points
library(ggfortify)
library(modelsummary)
autoplot(con_mod)

cons_inc<-cons_inc|>mutate(resid=residuals(con_mod))
tidy(con_mod)
modelsummary(con_mod,fmt = 2,
  estimate  = c(
    "{estimate} {stars}"))
#get_regression_table(con_mod)
#library(modelsummary)
#modelsummary(con_mod)

acf1 <- ggAcf(residuals(con_mod))
pacf1 <- ggPacf(residuals(con_mod))
library(patchwork)
acf1+pacf1
#pacf(resid)
#adf.test(resid)
ggplot(cons_inc)+aes(x=year, y=resid)+geom_col()

library(ggResidpanel)
resid_panel(con_mod,plots = 'default',smoother = TRUE)


library(patchwork)
g11<-ggplot(cons_inc, aes(x = Trend, y = C)) +
  geom_point() +
  labs(x = "Time trend", y = "Consumption",
       title = "Relationship between 
       Consumption with time trend") +  
  geom_smooth(method = "lm", se = FALSE)
tren_mod<-lm(C~Trend,data = cons_inc)
cons_inc<-cons_inc|>mutate(trend_resid=tren_mod$residuals)
g12<-ggplot(cons_inc)+aes(x=year, y=trend_resid)+geom_col()
g11+g12

g2s <-
  lm(C ~ Y + lag(Y, 1) + lag(C, 1) + lag(Y, 2) + lag(C, 2) + lag(Y, 3) + lag(C, 3),
     data = cons_inc)

modelsummary(g2s,estimate = c("{estimate} {stars}"))


g2s1 <- lm(C ~ Y + lag(C, 1) + lag(Y, 2), data = cons_inc)


ggAcf(residuals(g2s1))

ggPacf(residuals(g2s1))

dyn_inc<-cons_inc|>filter(year>1968)|>mutate(g2s1res=g2s1$residuals)|>na.omit()
dyn_inc|>glimpse()
g13<-ggplot(dyn_inc)+aes(x=year, y=g2s1res)+geom_col()

modelsummary(g2s1,estimate = c("{estimate} {stars}"))


g13

resid_panel(g2s1,plots = 'default',smoother = TRUE)

library(corrplot)
cons_inc<-cons_inc|>mutate(lagY1=lag(Y,1),lagY2=lag(Y,2),lagY3=lag(Y,3),lagC1=lag(C,1),lagC2=lag(C,2),lagC3=lag(C,3))
M<-cons_inc|>select(C,Y,lagY1,lagY2,lagY3,lagC1,lagC2,lagC3)|>na.omit()
M1<-cor(M)
round(M1,3)

g2s2<-lm(C~Y+lag(C,1)+lag(Y,1),data=cons_inc)

modelsummary(g2s2,estimate = c("{estimate} {stars}"))

resid_panel(g2s2,plots = 'default',smoother = TRUE)
