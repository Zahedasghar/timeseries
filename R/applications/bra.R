library(readxl)
library(tidyverse)
BRA <- read_excel("docs/data/BRA.xlsx")
BRA
BRA |> glimpse()
library(janitor)
BRA |> clean_names() ->BRA
BRA |> glimpse()

BRA |> mutate_at(c('sales_tax_on_services', 'total'), as.numeric) -> BRA

View(BRA)

BRA |> group_by(sector) |> 
  summarise(tot_rev=sum(sales_tax_on_services)) |> top_n(n=5) |> arrange(desc(tot_rev))


BRA |> group_by(year) 

BRA |> group_by(year) |>  
  summarise(tot_rev=sum(sales_tax_on_services)) |> na.omit()

BRA |> filter(sales_tax_on_services!="NA") |> 
  summarise(tot_rev=sum(sales_tax_on_services),.by=year)

BRA |> filter(sales_tax_on_services!="NA") |> 
  summarise(tot_rev=sum(sales_tax_on_services),.by=year)
            
            
            

            