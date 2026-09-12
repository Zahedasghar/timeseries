
library(tidyverse)
library(lubridate)

library(forecast)
library(xts)
library(tsibble)
library(fpp2)
library(fpp3)
library(ggthemes)
library(ggfortify)

## To get R data loaded, run the following

load("tax.RData")

## To view data

View(tax)

## To have an overview of data and its variables

glimpse(tax)


## To have date variable in appropriate format if needed
tax$Date<-lubridate::ym(tax$TAX_PERIOD)
tax$SCODE<-as.character(tax$SCODE)

tax$Month<-tax$TAX_PERIOD-round(tax$TAX_PERIOD,-2)
tax$Month<-month.abb[tax$Month]

### Put a code of your service instead of 98450000 and name data as per your
## requirement and proceed as follows

mydf<-tax %>% filter(SCODE=="9805.6") %>% group_by(Date) %>% 
  summarise(tax_val=sum(services_dum))
glimpse(mydf)
mydf$year<-year(mydf$Date)
mydf_yr<-mydf %>% group_by(year) %>% 
  summarise(tax_yr=sum(tax_val))

ggplot(mydf_yr)+aes(year, tax_yr)+geom_line()

chart<-ggplot(mydf)+aes(Date,tax_val)+geom_line()+
  labs(title=" Service tax collected from name of your service sector", 
       caption ="By your name: Source: KPRA" )
chart
ggsave("chart.pdf")
ggsave("chartsw.png",width = 25, height = 25, units = "cm")

mydf_ts<-ts(mydf$tax_val,start=c(2014,7),end=c(2021,6),frequency = 12)

### Naive Forecasting 
fcsnaive<-snaive(mydf_ts,h=12)
autoplot(fcsnaive)
fcsnaive



### Simple exponential smoothing
fcses<-ses(mydf_ts,h=12)


fcses


### ARIMA Forecast
fit<-auto.arima(mydf_ts)
arforecast<-forecast(fit,h=12)
autoplot(arforecast)




### Accuracy of three methods
accuracy(arforecast)
accuracy(fcses)
accuracy(fcsnaive)
autplot(fcsnaive)

### Advanced methods
mydf_ts<-as_tsibble(mydf_ts)

mydf_ts %>%
  model(ARIMA(value)) %>%
  forecast(h=12) %>%
  autoplot(mydf_ts) +
  labs(title = "Tax collected",
       y = "Rs ('000)")
fit <- mydf_ts %>%
  model(
    additive = ETS(value ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(value ~ error("M") + trend("A") +
                           season("M"))
  )
fit<-mydf_ts %>% model(ETS(value))
fc <- fit %>% forecast(h = 12)
accuracy(fc)
fc %>%
  autoplot() +
  labs(title="My service analysis",
       y="tax in Rs (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

library(seasonal)
library(seasonalview)
library("shiny")
view(seas(mydf_ts))


pak_fit <- global_economy %>%
  filter(Code == "PAK") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

pak_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")
glance(pak_fit) %>% arrange(AICc) %>% select(.model:BIC)

augment(pak_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)
pak_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)
