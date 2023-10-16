library(fpp2)
library(fpp3)
library(tidyverse)
library(forecast)
library(zoo)
library(ggfortify)
library(readxl)

kse30 <- read_csv("docs/data/kse_30_index.csv")


kse30$date <- mdy(kse30$Date)

kse30 |> select(Price, date) |> glimpse()


ggplot(kse30)+aes(x=date,y=Price)+
  geom_line()

library(dygraphs)

kse30 |> select(date, Price) |> 
  dygraph(, main="KSE-30 price index") |> 
  dyAxis("y", label = "Price") |> 
  dyAxis("x", label = "date") 

 
kse30 |> select(date, Price) |> 
  dygraph(, main="KSE-30 price index") |> 
  dyAxis("y", label = "Price") |> 
  dyAxis("x", label = "date") |> 
  dyOptions(drawRug = TRUE)

