library(tidyverse)
library(dynlm)
library(haven)
library(fpp3)


cons_inc<-read_dta("data/Consumption_Income_Pakistan.dta")
library(tseries)
cons_inc$year<-as_date(cons_inc$year)
cons_inc<-cons_inc %>% mutate(Trend=1:52)


#cons_inc<-ts(con_inc,start = 1960, end=2019)

#cons_inc<-as_tibble(con_inc)
ggplot(cons_inc)+aes(x=year)+geom_line(aes(y = C), color = "darkred",size=0.8) + 
  geom_line(aes(y = Y), color="steelblue", linetype="twodash",size=0.8)+labs(x="Income per capita",y="Consumption per captia",title = "Consumption Income pattern over time for Pakistan")+theme_minimal()

#| echo: false
#| warning: false
#| message: false
library(kableExtra)
library(stargazer)
library(broom)
library(moderndive)
con_mod<-lm(C~Y,data = cons_inc)
regression_points <- get_regression_points(con_mod)
regression_points |> slice_head(n=5)

library(ggfortify)
library(modelsummary)

cons_inc<-cons_inc|>mutate(resid=residuals(con_mod))

modelsummary(con_mod, stars = TRUE)
#get_regression_table(con_mod)
#library(modelsummary)
#modelsummary(con_mod)

ggplot(cons_inc)+aes(x=year, y=resid)+geom_col()

library(ggResidpanel)
resid_panel(con_mod,plots = 'default',smoother = TRUE)


library(gridExtra)
g11<-ggplot(cons_inc, aes(x = Trend, y = C)) +
  geom_point() +
  labs(x = "Time trend", y = "Consumption",
       title = "Relationship between Consumption with time trend") +  
  geom_smooth(method = "lm", se = FALSE)
tren_mod<-lm(C~Trend,data = cons_inc)
cons_inc<-cons_inc|>mutate(trend_resid=tren_mod$residuals)
g12<-ggplot(cons_inc)+aes(x=year, y=trend_resid)+geom_col()
library(patchwork)
g11+g12

g2s<-lm(C~Y+lag(Y,1)+lag(C,1)+lag(Y,2)+lag(C,2)+lag(Y,3)+lag(C,3),data=cons_inc)

modelsummary(g2s,estimate = c("{estimate} ({std.error}){stars}"))

g2s1<-lm(C~Y+lag(C,1)+lag(Y,2),data=cons_inc)
dyn_inc<-cons_inc|>filter(year>1968)|>mutate(g2s1res=g2s1$residuals)|>na.omit()
dyn_inc|>glimpse()


modelsummary(g2s1,estimate = c("{estimate} ({std.error}){stars}"))


g13<-ggplot(dyn_inc)+aes(x=year, y=g2s1res)+geom_col()+labs(title="Residuals of Dynamic Consumption Function",x="Year",y="Residuals")
g13

resid_panel(g2s1,plots = 'default',smoother = TRUE)

library(corrplot)
cons_inc<-cons_inc|>mutate(lagY1=lag(Y,1),lagY2=lag(Y,2),lagY3=lag(Y,3),lagC1=lag(C,1),lagC2=lag(C,2),lagC3=lag(C,3))
M<-cons_inc|>select(C,Y,lagY1,lagY2,lagY3,lagC1,lagC2,lagC3)|>na.omit()
M1<-cor(M)
round(M1,3)

g2s2<-lm(C~Y+lag(C,1)+lag(Y,1),data=cons_inc)

modelsummary(g2s2,estimate = c("{estimate} ({std.error}){stars}"))

resid_panel(g2s2,plots = 'default',smoother = TRUE)
