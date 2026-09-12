## ----include=FALSE----------------
library(tidyverse)
library(httr)
clrs <- MetBrewer::met.brewer(name = "Java")
clrs_lt <- colorspace::lighten(clrs, 0.9)
knitr::opts_chunk$set(fig.retina = 3, collapse = TRUE)
options(digits = 3, width = 75)


## ---------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(collapse)
library(kableExtra)
library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(xts)
library(urca)
library(tsibble)
library(fpp2)
library(fpp3)
library(ggthemes)
library(DT)
library(fable)
library(fabletools)


## ---------------------------------
library(readr) # To load data from csv file, if one needs to load data from excel, then use excel file
cement<-read_csv("cement.csv")
glimpse(cement)


## ---------------------------------
cement<-cement %>% rename(Category=`Series Name` )

#cement<-ts(cement, frequency = 12, start=c(1991,7))
#cement
#cement<-cement %>% mutate(Month=yearmonth(Date)) %>% as_tsibble(index = Month)

cement<-cement %>% filter(Category=="Total Cement Sales") # Select only Total Cement Sales

cement$Date<-my(cement$Date) # Formating required
cement$Date<-as_date(cement$Date,format="%Y-%m")

#cement<-tsibble(cement)
cement
cement$year<-year(cement$Date)
cement
cement_yrly<-cement %>% group_by(year) %>% summarise(prod_yr=sum(Output))
cement_yrly<-cement_yrly %>% filter(year<=2021)
#cement_yrly$year<-make_date(cement_yrly$year)
#cement_yrly<-tsibble(cement_yrly)
cement_yrly
#cement<-tsibble(cement)


## ---------------------------------
p1<-ggplot(cement_yrly)+aes(x=year,y=prod_yr)+geom_line()
p1
forecast::naive(cement_yrly$prod_yr, h=4) %>% autoplot() 


## ---------------------------------
pm<-ggplot(cement)+aes(x=Date,y=Output)+geom_line()
pm


## ---------------------------------
p1+labs(x="Date",y="output (Thousand metric tons)", title = "Year Production from 1991-2022", caption = "By Zahid Asghar, Source:APCMA, Pakistan")





## ---------------------------------
pm+labs(x="Date",y="Production (Thousand metric tons)", title = "Year Production from 1991-2022", caption = "By Zahid Asghar, Source:APCMA, Pakistan")



## ---------------------------------
cement_2010<-cement %>% filter(Date>="2010-6-30")
cement_2010


## ---------------------------------
p11<-ggplot(cement_2010)+aes(x=Date,y=Output)+geom_line()
p11+labs(x="Date",y="Output (Thousand metric tons", title = "Monthly Cement Output from 2010-2022", caption = "By Zahid Asghar, Source:APCMA, Pakistan")


## ---------------------------------

forecast::snaive((cement$Output), h = 24) %>% autoplot()



## ---- echo=FALSE------------------
library(kableExtra)
fit_arima<-auto.arima(cement$Output, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)



## ---------------------------------
cement<-cement %>% mutate(Month=yearmonth(Date)) %>% as_tsibble(index=Month)

cement %>%
  gg_season(Output,labels = "both")+
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")





## ---------------------------------
cement %>%
  gg_subseries(Output,labels = "both")+
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")




## ---------------------------------

cement %>%
  gg_lag(Output, geom = "point") +
  labs(x = "lag(Output, k)")



## ---------------------------------
library(seasonalview)
library(shiny)
#cement_prod<-ts(data = cement$Output,frequency = 12, start=c(1991,7))
#view(seas(cement_prod))


