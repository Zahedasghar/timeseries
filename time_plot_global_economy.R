library(fpp3)

global_economy |>
  filter(Country == "Pakistan") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Pakistan exports")

  global_economy |>
    filter(Country == "Pakistan"|Country=="Bangladesh"| Country=="India") |>
    autoplot(Exports) +
    labs(y = "% of GDP", title = "Pakistan exports")
  
  
pk_exports <- global_economy |>
    filter(Country == "Pakistan") |>
    mutate(
      `5-MA` = slider::slide_dbl(Exports, mean,
                                 .before = 2, .after = 2, .complete = TRUE)
    )

pk_exports |> 
  autoplot(Exports, linewidth=1) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Pakistani Exports")+theme_minimal()


# Employment in US retail sector ------------------------------------------

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)




us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


us_retail_employment |>
  model(classical_decomposition(Employed, type = "additive")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")



us_retail_employment |>
  model(classical_decomposition(Employed, type = "multiplicative")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of total US retail employment")

us_retail_employment |>
  model(classical_decomposition(Employed, type = "additive")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total 
          US retail employment")
# Install and load the WDI package
# 
# library(WDI)
#   
# # Define the indicators
#   indicators <- c("NY.GDP.MKTP.CD",     # Gross domestic product (current US$)
#                   "NY.GDP.MKTP.KD.ZG",  # GDP growth (annual %)
#                   "FP.CPI.TOTL",        # Consumer price index (2010 = 100)
#                   "NE.IMP.GNFS.ZS",     # Imports of goods and services (% of GDP)
#                   "NE.EXP.GNFS.ZS",     # Exports of goods and services (% of GDP)
#                   "SP.POP.TOTL")        # Total population
#   
# # Retrieve data for the desired indicators from 1960 to 2023
# globe_econ <- WDI(country = "all", indicator = indicators, start = 1960, end = 2023)
# 
# save(globe_econ, file = "data/globe_econ.RData")

load("data/globe_econ.RData")

# View the first few rows of the retrieved data
head(globe_econ)

colnames(globe_econ) <- c("country","iso2c","iso3c", "year", "GDP", "GDP_growth", "CPI", "imports", "exports", "population")

## Convert to tsibble

# Convert data to tsibble
wdi_data <- as_tsibble(globe_econ, key = country, index = year)

save(wdi_data, file = "data/wdi_data.RData")

load("data/wdi_data.RData")

wdi_data |> filter(country == "Pakistan") |> 
  autoplot(exports) +
  labs(y = "% of GDP", title = "Pakistan exports as % of GDP", caption = "By Zahid, source:WDI")+
  theme_minimal()


wdi_data |> filter(country %in% c("Pakistan","Japan", "China")) |> 
  autoplot(exports, linewidth=1) +
  labs(y = "% of GDP", title = "Bangladesh, India and Pakistan exports as % of GDP", caption = "By Zahid, source:WDI")+
  theme_minimal()+ theme(legend.position = "top")

## Revision of base year in 2000-01 led to increase in GDP but While preparing
## lecture , just worked with wdi data  and found that after revision of base
## year in 2000-01, Pakistan export/GDP ratio has declined significantly. Once I
## observed same patterns in tax to GDP. So we can have natural growth rate for
## increase in GDP but unless we increase our share in export/capita (in which
## we are among the bottom 10 countries), it simply means we are not
## competitive in the world.
#' Without increasing our share in export, its not possible to meet external debt obligations which 
#' have to be paid in foreign currency. 
#' 



wdi_data |> filter(country %in% c("Pakistan","India", "Bangladesh")) |> 
  autoplot(imports, linewidth=1) +
  labs(y = "% of GDP", title = "Bangladesh, India and Pakistan imports as % of GDP", caption = "By Zahid, source:WDI")+
  theme_minimal()+ theme(legend.position = "top")

