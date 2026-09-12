
# Time-series-VAR-models --------------------------------------------------

library(tidyverse)
library(fpp3)
library(fable)
library(readxl)
library(janitor)
library(xts)
library(purrr)
library(broom)
library(dplyr)
library(tidyr)
# load-data ---------------------------------------------------------------
stock <- read_excel("data/Data2.xlsx", sheet="Prices 10y")
cds <- read_xlsx('data/Data1.xlsx',sheet="10y of 5Y CDS") # read sheet 1 

quick <- read_excel("data/Data2.xlsx", sheet="QUICK RATIO 10Y")
leverage <- read_excel("data/Data2.xlsx", sheet="Financial Leverage 10y")
market_cap <- read_excel("data/Data2.xlsx", sheet="Market cap 10y")
vix <- read_excel("data/Data1.xlsx", sheet="VIX over 10y")
vix_var <- read_excel("data/Data1.xlsx", sheet="VIX var over 10y")
spx <- read_excel("data/Data1.xlsx", sheet="SPX 10y")
fedf <- read_excel("data/Data1.xlsx", sheet="FED FUND 10Y")
upgrade <- read_excel("data/Data1.xlsx", sheet="UPGRADE")
downgrade <- read_excel("data/Data1.xlsx", sheet="DOWNGRADE")


 

date_column <- as.Date(quick$Date, origin="1899-12-30")
quick$Date <- date_column
quick
dates <- as.Date(unlist(quick[,1]))

quick1 <- xts(quick[, -1], order.by=dates)
quick1 <- na.locf(quick1)

quick2 <- bind_cols(quick$Date,quick1)
quick2 <- quick2 |> rename(Date=`...1`)
date_column <- as.Date(leverage$Date, origin="1899-12-30")
leverage[[date_colname]] <- date_column
dates <- as.Date(unlist(leverage[,1]))
leverage1 <- xts(leverage[, -1], order.by=dates)
leverage1 <- na.locf(leverage1)

leverage2 <- bind_cols(leverage$Date,leverage1)

leverage2 <- leverage2 |> rename(Date=`...1`)

# Specify the desired start and end dates
start_date <- as.Date("5/12/2013", format = "%d/%m/%Y")
end_date <- as.Date("22/06/2023", format = "%d/%m/%Y")

# Filter the data frame to include only rows within the specified date range
cds <- cds %>%
  filter(Date>= start_date & Date <= end_date) |> unique()  

cds1 <- cds |> pivot_longer(cols=!Date,
                    names_to = 'company',
                    values_to = 'CDS')


stock1 <- stock |> 
  filter(Date>= start_date & Date <= end_date) |> unique()  |> pivot_longer(cols=!Date,
                                names_to = 'company',
                                values_to = 'S') |> 
  mutate(logS=log(S)) |> group_by(company) |> mutate(Lag_S=lag(logS))

leverage3 <- leverage2 |> 
  filter(Date>= start_date & Date <= end_date) |> unique()  |> pivot_longer(cols=!Date,
                                                                            names_to = 'company',
                                                                     values_to = 'L')

quick2 |> dim()
quick3 <- quick2 |> 
  filter(Date>= start_date & Date <= end_date) |> unique()  |>
  pivot_longer(cols=!Date,names_to = 'company',values_to = 'Q')
market_cap1 <- market_cap |> 
  filter(Date>= start_date & Date <= end_date) |>
  unique()  |> 
  pivot_longer(cols=!Date,names_to = 'company',values_to = 'MC') |> 
  mutate(logMC=log(MC))

market_cap1
spx1 <- spx|> 
  filter(Date>= start_date & Date <= end_date) |>
  unique() |> mutate(SPY=log(`SPX Index`)) 

vix1<- vix|> 
  filter(Date>= start_date & Date <= end_date) |>
  unique() |> rename(VIX=`VIX Index`)

vix_var1<- vix_var |>  
  filter(Date>= start_date & Date <= end_date) |>
  unique() |> rename(VIX_var=`VIX Index`)

fedf1 <- fedf |>  
  filter(Date>= start_date & Date <= end_date) |>
  unique()

upgrade1 <- upgrade |> 
  filter(Date>= start_date & Date <= end_date) |>
  unique()  |> 
  pivot_longer(cols=!Date,names_to = 'company',values_to = 'U') 



downgrade1 <- downgrade |> 
  filter(Date>= start_date & Date <= end_date) |>
  unique()  |> 
  pivot_longer(cols=!Date,names_to = 'company',values_to = 'D') 




inner_join(cds1, stock1, by = c("Date", "company"))


# Merge data1 and data2 based on Date and ID
merged_data <- stock1 %>%
  inner_join(cds1, by = c("Date", "company")) %>%
  inner_join(leverage3, by = c("Date", "company")) |> 
  inner_join(quick3, by = c("Date", "company"))

merged_data1 <- merged_data |> 
  inner_join(fedf1, by = "Date") %>%
  inner_join(vix1, by = "Date") |> 
  inner_join(vix_var1, by = "Date") |> 
  inner_join(spx1,by="Date")
merged_data2 <- merged_data1 |> 
  inner_join(market_cap1,by=c("Date","company")) |> 
  inner_join(upgrade1,by=c("Date","company"))|> 
  inner_join(downgrade1,by=c("Date","company"))


merged_data2 |> colnames()
  
fedf1

vix
spx1
merged_data1 |> select(Q)

merged_data2 |> names()
merged_data2 |> 
  group_by(company) |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(CDS,1)+`SPX Index`+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs) 


# Lag-1 CDS as dependent variable -----------------------------------------


eq2 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(CDS,1)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

eq2 <- as_data_frame(eq2)


CDS_eq <- pivot_wider(eq2,id_cols = 'company',names_from = 'term',
            values_from = c('estimate','std.error','statistic','p.value')) 

CDS_eq <- as_data_frame(CDS_eq) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(CDS_eq,'data/CDS_eq.csv')


# 2-lags ------------------------------------------------------------------

eq3 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(logS,2)+lag(CDS,1)+lag(CDS,2)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

eq3 <- as_data_frame(eq3)


CDS_eq_lag2 <- pivot_wider(eq3,id_cols = 'company',names_from = 'term',
                      values_from = c('estimate','std.error','statistic','p.value')) 

CDS_eq_lag2 <- as_data_frame(CDS_eq_lag2) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(CDS_eq_lag2,'data/CDS_eq_lag2.csv')


# 3-lags ------------------------------------------------------------------

eq4 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(CDS,1)+lag(CDS,2)+lag(CDS,3)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

eq4 <- as_data_frame(eq4)


CDS_eq_lag3 <- pivot_wider(eq4,id_cols = 'company',names_from = 'term',
                           values_from = c('estimate','std.error','statistic','p.value')) 

CDS_eq_lag3 <- as_data_frame(CDS_eq_lag3) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(CDS_eq_lag3,'data/CDS_eq_lag3.csv')




# 4-lags ------------------------------------------------------------------

eq5 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(logS,4)+lag(CDS,1)+lag(CDS,2)+
                                lag(CDS,3)+lag(CDS,4)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

eq5 <- as_data_frame(eq5)


CDS_eq_lag4 <- pivot_wider(eq5,id_cols = 'company',names_from = 'term',
                           values_from = c('estimate','std.error','statistic','p.value')) 

CDS_eq_lag4 <- as_data_frame(CDS_eq_lag4) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(CDS_eq_lag4,'data/CDS_eq_lag4.csv')




# 5-lags ------------------------------------------------------------------

eq6 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(CDS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(logS,4)+lag(logS,5)+lag(CDS,1)+lag(CDS,2)+
                                lag(CDS,3)+lag(CDS,4)+lag(CDS,5)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

eq6 <- as_data_frame(eq6)


CDS_eq_lag5 <- pivot_wider(eq6,id_cols = 'company',names_from = 'term',
                           values_from = c('estimate','std.error','statistic','p.value')) 

CDS_eq_lag5 <- as_data_frame(CDS_eq_lag5) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(CDS_eq_lag5,'data/CDS_eq_lag5.csv')
















# Stock-lag1 --------------------------------------------------------------




seq1 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(logS ~ lag(logS,1)+lag(CDS,1)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

stock_eq <- pivot_wider(seq1,id_cols = 'company',names_from = 'term',
            values_from = c('estimate','std.error','statistic','p.value')) 

stock_eq <- as_data_frame(stock_eq) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(stock_eq,'data/stock_eq.csv')




# Slag-2 -------------------------------------------------------------------

seq2 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(logS ~ lag(logS,1)+lag(logS,2)+lag(CDS,1)+lag(CDS,2)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

stock_eq2 <- pivot_wider(seq2,id_cols = 'company',names_from = 'term',
                        values_from = c('estimate','std.error','statistic','p.value')) 

stock_eq2 <- as_data_frame(stock_eq2) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(stock_eq2,'data/stock_eq2.csv')



# Slag-3 -------------------------------------------------------------------

seq3 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(logS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(CDS,1)+lag(CDS,2)+lag(CDS,3)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

stock_eq3 <- pivot_wider(seq3,id_cols = 'company',names_from = 'term',
                         values_from = c('estimate','std.error','statistic','p.value')) 

stock_eq3 <- as_data_frame(stock_eq3) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(stock_eq3,'data/stock_eq3.csv')



# Slag-4 -------------------------------------------------------------------

seq4 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(logS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(logS,4)+lag(CDS,1)+lag(CDS,2)+lag(CDS,3)+lag(CDS,4)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

stock_eq4 <- pivot_wider(seq4,id_cols = 'company',names_from = 'term',
                         values_from = c('estimate','std.error','statistic','p.value')) 

stock_eq4 <- as_data_frame(stock_eq4) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(stock_eq4,'data/stock_eq4.csv')


# Slag-5 -------------------------------------------------------------------

seq5 <- merged_data2 |> 
  group_by(company) |> na.omit() |> 
  nest() |> 
  mutate(fit = map(data, ~ lm(logS ~ lag(logS,1)+lag(logS,2)+lag(logS,3)+lag(logS,4)+lag(logS,5)+lag(CDS,1)+lag(CDS,2)+lag(CDS,3)+lag(CDS,4)+
                                lag(CDS,5)+SPY+L+Q+VIX+VIX_var+MC+U+D, data = .)),
         coefs = map(fit, tidy)) |> 
  unnest(coefs)

stock_eq5 <- pivot_wider(seq5,id_cols = 'company',names_from = 'term',
                         values_from = c('estimate','std.error','statistic','p.value')) 

stock_eq5 <- as_data_frame(stock_eq5) |> mutate(across(where(is.numeric), ~ round(., 3)))


write_csv(stock_eq5,'data/stock_eq5.csv')















stock1 |> group_by(company) %>%
  mutate(LaggedVariable = lag(S))

left_join(stock,cds,by="Date") |> colnames()

quick2 |> filter(Date>= start_date & Date <= end_date) |> unique()  |> 
  dim()
# Print the resulting data frame
print(filtered_df)



intersect(names(stock), names(quick))


# Market-cap --------------------------------------------------------------

date_column <- as.Date(market_cap$Date, origin="1899-12-30")
market_cap[[date_colname]] <- date_column
dates <- as.Date(unlist(market_cap[,1]))
market_cap <- xts(market_cap[, -1], order.by=dates)

market_cap |> glimpse()






# Vix level-------------------------------------------------------------------------

# Load the vars package
library(vars)

# Sample data (replace with your own data)
data <- data.frame(
  Y1 = c(10, 15, 20, 25, 30),
  Y2 = c(5, 8, 12, 15, 18),
  X1 = c(1, 2, 3, 4, 5),
  X2 = c(0.5, 1.0, 1.5, 2.0, 2.5)
)

#Specify the exogenous variables for each equation
exogen_matrix <- matrix(c(data$X1, data$X2), ncol = 2)

# Estimate the VAR model with 1 lag for Y1 and Y2
var_model <- VAR(data = data[, c("Y1", "Y2")], p = 1, exogen = list(exogen_matrix))

# Summary of the VAR model
summary(var_model)

library(vars)

# Sample data (replace with your own data)
data <- data.frame(
  Y1 = c(10, 15, 20, 25, 30),
  Y2 = c(5, 8, 12, 15, 18)
)

# Estimate the VAR model with 1 lag for Y1 and Y2
var_model <- VAR(data , p = 1)

# Summary of the VAR model
summary(var_model)



