
library(tidyverse)
library(lubridate)
library(readxl)
library(collapse)
library(dynlm)
library(forecast)
library(scales)
library(xts)
library(urca)
library(tsibble)
library(fpp2)
library(fpp3)
library(ggthemes)
library(DT)
library(ggfortify)
library(ggthemes)
#tax<-read_excel("services_data.xlsx")
#save(tax, file="tax.Rdata")
#save(kprev, file = "kprev.RData")
load("tax.RData")
#load("D:/RepTemplates/Tax_data/Data.RData") ## Data are loaded
View(tax)
str(tax)
tax$Date<-lubridate::ym(tax$TAX_PERIOD)
tax$SCODE<-as.character(tax$SCODE)
glimpse(tax)
#tax_df<-as.data.frame(tax)
#tax_df<-tax_df %>% mutate(Year = str_replace_all(Year, pattern = ",", replacement = " "))
##tax_df$Date<-as.Date(tax_df$Date)
#tax_df$Date<-as.Date(tax_df$Date)
tax$Month<-tax$TAX_PERIOD-round(tax$TAX_PERIOD,-2)
tax$Month<-month.abb[tax$Month]


## We are not selecting varaibles as we have already deleted some variables to have some ammendments in data
#tax_df<-tax %>% select('SCODE','Date','TAX_PERIOD','Year', 'Month',
#                        'services_dum', 'Rate')


total_tax<-tax %>% group_by(Date,SCODE) %>% 
  summarise(tax_val=sum(services_dum)) %>% mutate(tax_rs=tax_val/1000000)
glimpse(total_tax)
View(total_tax)
## Extract year variable from Date
total_tax$year<-year(total_tax$Date)

total_tax
yearly_tax<-total_tax %>% group_by(year) %>% 
  summarise(tax_by_serv=sum(tax_val))
glimpse(yearly_tax)

ggplot(yearly_tax)+aes(year, tax_by_serv)+geom_line()+labs(
  title = "Total tax collected from services sector over 2013-2021",
  x = "Fiscal Year",
  y = "Rs in 0000000 ..."
)+theme_minimal()

## group by service and yearwise





## Monthly tax summary
ggplot(total_tax, aes(x = Date, y = tax_rs)) +
  geom_line() +
  labs(
    title = "Tax collected from services sector ",
    x = "Fiscal Year",
    y = "Rs "
  )+theme_economist()


SNAIVE<-snaive(total_tax$tax_rs, h=12) 
SES<-ses(total_tax$tax_rs,h=12)
autoplot(SNAIVE)
autoplot(SES)
arfit<-auto.arima(total_tax$tax_rs)
arforecast<-arfit %>% forecast(h=12)

arforecast


autoplot(arforecast)
accuracy(arforecast)
accuracy(SNAIVE)
accuracy(SES)

### ARIMA models

fit_arima<-auto.arima(yearly_tax$tax_by_serv, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)
fit_arima %>% forecast(h=1)


### Aritificial Nueral Networks

NN <- nnetar(total_tax$tax_rs, lambda="auto")
fcast2 <- forecast(NN, h=12, PI=TRUE, npaths=100)
autoplot(fcast2, include = 2)
fcast2
accuracy(NN)

# Seasonal Naive Forecasting
fit_SN <- snaive(total_tax$tax_rs)

forecast::snaive((total_tax$tax_rs), h = 24)
fdf<-as.data.frame(forecast::snaive((total_tax$tax_rs), h = 24))
fdf<-forecast(snaive((total_tax$tax_rs), h = 24))
fdf$`Point Forecast`
forecast::snaive((total_tax$tax_rs), h = 24) %>% autoplot()


### Autoregressive Integrated Moving Average Models

fit_ARIMA <- auto.arima(total_tax$tax_rs, stepwise = FALSE, approximation = FALSE)
summary(fit_ARIMA)
fit_ARIMA %>% forecast(h=24) %>% autoplot()
auto.arima(total_tax$tax_rs) %>% forecast(h=24)



### Artificial Nueral Network Forecasting

SNN <- nnetar(total_tax$tax_rs, lambda="auto")
fcast2m <- forecast(SNN, h=24, PI=TRUE, npaths=100)
autoplot(fcast2m, include = 36)
fcast2m
library(kableExtra)
accuracy(SNN)

## Service sector share in total tax by service category

# Top 10 sectors tax share is almost .....

grp<-tax_df %>% group_by(SCODE) %>% 
  summarise(total=sum(services_dum)) %>% mutate(tax_Rs=round(total/1000000,2)) %>% 
  arrange(desc(tax_Rs))
grp
total_output_tax<-tax_df %>% summarise(sum(services_dum)/1000000)
round(total_output_tax,2)
grp1<-grp %>% select( SCODE, tax_Rs) %>% mutate(percent_share=round(tax_Rs/916400.9*100,2))
datatable(grp1, caption ="Table 1.1 The total tax for each service for the 2013 to 2022 fiscal years." )
sum(grp1[1:10,]$percent_share)
#kable(grp1, col.names = c("SCODE", "Service","Output_tax_rsionRs"), align = "cc", caption = "Table 1.1 The total tax for each service for the 2013 to 2022 fiscal years.")

## 

tax_2020<-tax_df %>% filter(Year==2018|Year==2019|Year==2020) %>%  group_by(SCODE) %>% 
  summarise(total=sum(services_dum)) %>% mutate(tax_Rs=round(total/1000000,2)) %>% 
  arrange(desc(tax_Rs))

total_output_tax_2020<-tax_df %>%filter(Year==2018|Year==2019|Year==2020) %>% summarise(sum(services_dum)/1000000)
round(total_output_tax_2020,2)
tax_20_grp<-tax_2020 %>% select( SCODE, tax_Rs) %>% mutate(percent_share=round(tax_Rs/366567.6	*100,2))
datatable(tax_20_grp, caption ="Table 1.2 The total tax for each service for the 2020 to 2022 fiscal years." )



## Top 15 sector during FY 2020-22 as per Tax_output

top<- tax_20_grp %>% arrange(desc(percent_share)) 
top10<-top[1:10,] 
datatable(top10, caption="Top 10 services by tax output")
ggplot(top10, aes( x=SCODE,y=percent_share))+
  geom_bar(stat='identity')+coord_flip()
sum(top10$percent_share)


## Forecasting for Service having Tax rate 16%


tax_at_16<-tax %>% filter( Rate==16) %>% 
  summarise(total=sum(services_dum)) 
tax_at_16 %>% summarise(sum(tax_at_16$total))
tax_at_all<-tax  %>% 
  summarise(total=sum(services_dum)) 
tax_at_all 

### Tax forecast for services having 16% tax-rate

#Top 10 sectors with maximum tax collection with 16% rates applied are as follows.


tax %>% group_by(SCODE)%>% filter( Rate==16) %>% 
  summarise(total=sum(services_dum)) %>% arrange(desc(total)) %>% top_n(10)

## 

tax_rate_yr<-tax %>% group_by(Date) %>% filter(Rate==16) %>% 
  summarise(tax_val=sum(services_dum)) 
## Extract year variable from Date
tax_rate_yr$year<-year(tax_rate_yr$Date)


tax_rate_yr<-tax_rate_yr%>% group_by(year) %>% 
  summarise(tax_by_serv=sum(tax_val))

tax_rate_yr<-tax_rate_yr %>% filter(year!=2021)
#tax_year1<-ts(tax_year$tax_by_serv,  frequency = 1,start = c(2013,1))
#plot(tax_year1)
ggplot(tax_rate_yr,aes(year, tax_by_serv))+geom_line()+labs(
  title = "Total tax collected from services sector over 2013-2021",
  x = "Fiscal Year",
  y = "Rs"
)+theme_economist()


auto.arima(tax_rate_yr$tax_by_serv, stepwise = FALSE, approximation = FALSE) %>% forecast(h=1)

#So **Rs.** billion rupees tax will be collected from sectors where 16% tax rate is appplied. If tax rate is reduced by 1%, it will reduce tax collection by **Rs. 378** million in a year. Now we calculate impact of loss in revenue if tax is reduced by 1% in top 10 sectors having tax rate of 16%.


top10_tax<-total_tax %>% 
  filter( SCODE %in% c("9813.4","9801.2","9809","9815.5","9824","9805.6","9999",
                       "9875","9818.1","9813"))


ggplot(top10_tax)+aes(Date, tax_val,color=SCODE)+geom_line()+
  labs(title = "Tax by top 10 services from 2012-2021", caption = "By Zahid Asghar, Source: Dummy")

top_10<-  top10_tax %>%  filter(Rate==16) %>% 
  summarise(total=sum(services_dum)) 
t
top_10_yr<-top10_tax%>% group_by(Year) %>% 
  summarise(tax_by_serv=sum(services_dum))
## Total tax by top 10 companies at 16% rate

top_10_yr %>% summarise(sum(tax_by_serv))
tax_rate_yr<-tax_rate_yr %>% filter(year!=2021)
ggplot(top_10_yr,aes(Year, tax_by_serv))+geom_line()+labs(
  title = "Total tax collected from top 10 services sector over 2013-2021",
  x = "Fiscal Year",
  y = "Rs "
)+theme_economist()
auto.arima(top_10_yr$tax_by_serv, stepwise = FALSE, approximation = FALSE) %>% forecast(h=1)



