

library(tidyverse)

library(readxl)

con_inc <- read_excel("docs/data/SgpCon2Models.xlsx")

con_inc |> glimpse()

#con_inc<-con_inc %>% mutate(date=Year)

con_inc <- con_inc  |>  mutate(Trend = 1:60)

con_inc<-ts(con_inc,start = 1960, end=2019)

con_inc <- as_tibble(con_inc)

ggplot(con_inc)+aes(x=Year)+geom_line(aes(y = SgpGDP), color = "darkred") + 
  geom_line(aes(y = SgpCon), color="steelblue", linetype="twodash")

library(broom)

library(moderndive)

con_mod <- lm(SgpCon ~ SgpGDP, data = con_inc)

regression_points <- get_regression_points(con_mod)
regression_points

library(ggfortify)
autoplot(con_mod)

barplot(con_mod$residuals)
#get_regression_table(con_mod)

library(modelsummary)
#modelsummary(con_mod)

library(ggResidpanel)

resid_panel(con_mod,plots = 'default',smoother = TRUE)


g11<-ggplot(con_inc, aes(x = Trend, y = SgpCon)) +
  geom_point() +
  labs(x = "Time trend", y = "Singapore Consumption",
       title = "Relationship between Singapore consumption with time trend") +  
  geom_smooth(method = "lm", se = FALSE)

tren_mod<-lm(SgpCon~Trend,data = con_inc)

g12<-barplot(tren_mod$residuals)


con_inc<-con_inc %>% mutate(con_gr=(SgpCon-lag(SgpCon,1))/lag(SgpCon,1)*100,gdp_gr=(SgpGDP-lag(SgpGDP,1))/lag(SgpGDP,1)*100,gdp_gr_Saf=(SAfGDP-lag(SAfGDP,1))/lag(SAfGDP,1)*100)
ggplot(con_inc)+aes(x=Year,y=con_gr)+geom_line()+ggtitle("Growth rate in consumption of Singapore")
ggplot(con_inc)+aes(x=Year,y=con_gr)+geom_line()+ggtitle("Growth rate in GDP of Singapore")

growth_mod<-lm(con_gr~gdp_gr,data=con_inc)

modelsummary(growth_mod)

barplot(growth_mod$residuals)
library(ggResidpanel)
resid_panel(growth_mod,plots = 'default',smoother = TRUE)

library(huxtable)
growth_mod1<-lm(con_gr~gdp_gr_Saf,data=con_inc)
huxreg(growth_mod1)

modelsummary(growth_mod1,estimate = "{estimate}{stars}", output="huxtable")
barplot(growth_mod1$residuals)
library(ggResidpanel)
resid_panel(growth_mod1,plots = 'default',smoother = TRUE)


growth_mod2<-lm(con_gr~gdp_gr_Saf+gdp_gr,data=con_inc)
huxreg(growth_mod2)


barplot(growth_mod2$residuals)
library(ggResidpanel)
resid_panel(growth_mod2,plots = 'default',smoother = TRUE)

