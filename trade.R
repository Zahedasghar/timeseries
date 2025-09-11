library(tidyverse)
library(WDI)
# import the data
trade <- read_csv("data/TradeData_all-world-16-20.csv")

# select the required columns
trade <- trade |> 
  select(RefYear, ReporterDesc, FlowDesc, PartnerDesc, PrimaryValue) |> 
  rename(year = RefYear, reporter = ReporterDesc, trade_direction = FlowDesc, 
         partner = PartnerDesc, trade_value_usd = PrimaryValue)

# rename the columns
# trade <- trade %>% 
#   rename(year = RefYear, reporter = ReporterDesc, trade_direction = FlowDesc, 
#          partner = PartnerDesc, trade_value_usd = PrimaryValue)

trade

TDI <-  trade %>% 
  # keep trade on Indonesia only and select columns of interest
  filter(reporter == "Pakistan") %>% 
  select(year, trade_direction, trade_value_usd)


# reshape the dataset
TDI <- spread(TDI, trade_direction, trade_value_usd)

# make a call to WDI database to obtain GDP data on Pakistan over 2016-2020 period
gdp <- WDI(indicator= c("GDP" = "NY.GDP.MKTP.CD"), country="PAK", start=1990, end=2020)

# merge the additional dataset with TDI data frame and deselect redundant columns
TDI <- merge(TDI, gdp, by = "year") %>% select(year, Export, Import, GDP)

TDI

TDI |> mutate((Export+Import)/GDP) |> View()


# calculate TDI for Pakistan in each examined year
TDI$tdi <- (TDI$Export + TDI$Import)/TDI$GDP*100 

# only keep values of the TDI index 
TDI <- TDI %>% select(year, tdi)

# round the TDI values
TDI$tdi <- round(TDI$tdi, 0)

TDI


# create a vector with names of selected South Asian economies
SouthAsia <- c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal", 
               "Bhutan", "Maldives")

x <- trade %>% 
  # filter the dataset to only keep data on trade of selected countries in 2020
  filter(reporter %in% SouthAsia & year == 2020) %>% 
  # keep only columns of interest
  select(reporter, trade_direction, trade_value_usd)

# reshape the data
x <- spread(x, trade_direction, trade_value_usd)
x
# make a call to WB database to obtain GDP data on six examined economies
gdp <- WDI(indicator = c("GDP" = "NY.GDP.MKTP.CD"), 
           country = c("IND", "PAK", "BGD", "LKA", "NPL", "BTN"), 
           start = 2020, end = 2020)

# merge the additional dataset with x data frame and deselect redundant columns
x <- merge(x, gdp, by.x = "reporter", by.y = "country") %>% 
  select(reporter, Export, Import, GDP)

x

# calculate TDI indices for the six economies
x$tdi <- (x$Export + x$Import)/x$GDP*100

# round the values
x$tdi <- round(x$tdi, 0)

x[, c("reporter", "tdi")]

# create a bar chart by selecting your variables and reordering the economies by values of TDI
TDI_plot <- ggplot(x, aes(x = tdi, y = reorder(reporter, desc(tdi)))) +
  
  # adding the bar plot to the chart area, and adjusting the width of the bars, and
  # and inactivating the legend
  # setting bar color based on value of TDI
  geom_col(aes(fill = tdi), width = 0.6, show.legend = FALSE) +
  
  # adding the chart title and removing the axis labels
  labs(title = "Trade Dependence Index for South Asian Economies (2020)", x = NULL, y = NULL) +
  
  # applying the minimal theme
  theme_minimal()

TDI_plot



#  Import Penetration 

# create a vector with names of selected South Asian economies

IP <- trade %>% 
  # keep trade data on the Pakistan and select columns of interest
  filter(reporter == "Pakistan") %>% 
  select(year, trade_direction, trade_value_usd) %>%
  # reshape the data to have separate columns for exports and imports
  spread(trade_direction, trade_value_usd)

# obtain GDP data for the Pakistan over 2016-2020
gdp <- WDI(indicator= c("GDP" = "NY.GDP.MKTP.CD"), country="PAK", start=2016, end=2020)

# merge the new dataset with IP data frame
IP <- merge(IP, gdp, by = "year") %>% select(year, Export, Import, GDP)

# calculate the value for domestic demand
IP$Domestic_Demand <- IP$GDP - IP$Export + IP$Import

# calculate the IP index
IP$ip <- (IP$Import / IP$Domestic_Demand) * 100

# only keep values of the IP index, and round the values
IP <- IP %>% select(year, ip) %>% mutate(ip = round(ip, 0))

IP


# remove TDI index values from the data frame
x <- x %>% select(-tdi)

# calculate domestic demand for the 6 economies
x$Domestic_Demand <- x$GDP - x$Export + x$Import

# calculate IP indices for the 6 economies, and round the values
x$ip <- (x$Import / x$Domestic_Demand) * 100
x$ip <- round(x$ip, 0)

x[, c("reporter", "ip")]


# create a bar chart by selecting your variables, and reordering economies by values of IP
IP_plot <- ggplot(x, aes(x = ip, y = reorder(reporter, desc(ip)))) +
  # adding the bar plot to the chart area, adjusting bar width, and removing the legend
  # setting bar color based on value of IP
  geom_col(aes(fill = ip), width = 0.6, show.legend = FALSE) +
  # adding the chart title, and removing axis labels
  labs(title = "Import Penetration Index for SouthAsian-4 Economies (2020)", x = NULL, y = NULL) +
  # applying minimal theme to the chart
  theme_minimal()

IP_plot


# extract data on Pakistan exports, and keep only columns of interest
# rename the column
EP <- trade %>% 
  filter(reporter == "Pakistan" & trade_direction == "Export") %>% 
  select(year, trade_value_usd) %>%
  rename(Export=trade_value_usd)


# obtain GDP data for Pakistan over 2016-2020
gdp <- WDI(indicator= c("GDP" = "NY.GDP.MKTP.CD"), country="PAK", start=2016, end=2020)

# merge the two datasets and keep only columns of interest
EP <- merge(EP, gdp, by = "year")  %>% select(year, Export, GDP)

# calculate and round the EP indices
EP$ep <- (EP$Export / EP$GDP) *100
EP$ep <- round(EP$ep, 0)

EP %>% select(year, ep)


# clean x data frame
x <- x %>% select(reporter, Export, GDP)

# calculate EP index for each economy in 2020, and round the values
x$ep <- (x$Export / x$GDP) * 100
x$ep <- round(x$ep, 0)

x %>% select(reporter, ep)



# create a bar chart by selecting your variables, and reordering economies by value of EP
EP_plot <- ggplot(x, aes(x = ep, y = reorder(reporter, desc(ep)))) +
  # adding the bar plot to the chart area, adjusting bar width, and removing the legend
  # setting bar color based on value of EP
  geom_col(aes(fill = ep), width = 0.6, show.legend = FALSE) +
  # adding the chart title, and removing axis labels
  labs(title = "Export Propensity Index for SouthAsian Economies (2020)", x = NULL, y = NULL) + 
  # applying minimal theme to the chart
  theme_minimal()

EP_plot


# Marginal Propensity to Import

# extract the data on Pakistan's imports
# select and rename columns as necessary
PKM <- trade %>% 
  filter(reporter == "Pakistan" & trade_direction == "Import") %>% 
  select(year, trade_value_usd) %>%
  rename(Import=trade_value_usd)


# merge the two data frames
PKM <- merge(PKM, gdp, by = "year")  %>% select(year, Import, GDP)

PKM

# calculate change in total imports each year, and store values in dImport column
# Note `NA` in dImport cell for 2016:
# this is because there is no previous year to compare with
PKM$dImport <- PKM$Import - lag(PKM$Import)

# calculate change in GDP each year
PKM$dGDP <- PKM$GDP - lag(PKM$GDP)

# calculate and round the index values
PKM$PKM <- PKM$dImport / PKM$dGDP
PKM$PKM <- round(PKM$PKM, 2)

PKM %>% select(year, PKM)


## South Asia

# retrieve Import data for SA economies
# select columns of interest
x <- trade %>% 
  filter(reporter %in% SouthAsia & trade_direction == "Import") %>% 
  select(reporter, year, trade_value_usd)


gdp <- WDI(indicator = c("GDP" = "NY.GDP.MKTP.CD"), 
           country = c("IND", "PAK", "BGD", "LKA", "NPL", "BTN"), 
           start = 2020, end = 2020)

gdp$country <- ifelse(gdp$country == "Pakistan", "Pakistan", gdp$country)

# merge the two datasets
x <- merge(x, gdp, by.x = c("year", "reporter"), 
           by.y = c("year", "country")) %>% 
  # select columns of interest
  select(year, reporter, trade_value_usd, GDP)


x <- x %>% 
  # group data by reporter
  group_by(reporter) %>%
  # calculate change in total imports each year for each economy
  mutate(dImports = trade_value_usd - lag(trade_value_usd, n = 1, default = NA)) %>% 
  # calculate change in GDP each year for each economy
  mutate(dGDP = GDP - lag(GDP, n = 1, default = NA)) %>%
  # calculate PKM indices for each economy for each year
  mutate(PKM = dImports / dGDP) %>% 
  ungroup() 

x <- x %>% 
  # remove redundant columns
  select(year, reporter, PKM) %>%
  # reshape the data
  spread(reporter, PKM)

# drop the first row currently containing NAs
x <- x[-1,]

x

# pivot the dataset to long format
x <- x %>% 
  pivot_longer(cols = India:`Sri Lanka`, names_to = "country", values_to = "PKM")

# create a time-series chart by selecting your variables, and by specifying the color coding rule
PKM_plot <-  ggplot(x, aes(x = year, y = PKM, color = country)) + 
  # adding the line charts
  geom_line() + 
  # specifying the color palette
  scale_color_brewer(palette = "Set1") +
  # providing the chart title, deactivating axis labels, and creating a color coding legend
  labs(title = "PKM for SAsian economies (2016-2020)", x = NULL, y = NULL, color = "Economies") +
  # applying the minimal theme
  theme_minimal()

PKM_plot

