library(readxl)
library(tidyverse)
library(xts)
library(dygraphs)

usd_pkr <- read_excel("docs/data/usd2pkr_asadzaman.xlsx", sheet="Data", skip=1)

usd_pkr |> head()

library(janitor)

names(usd_pkr) <- c("date", "usd2pkr", "annual_growth_rate","years")

usd_pkr <- usd_pkr |> dplyr::select(1:4)

## Convert date to month, day, year format

usd_pkr$date <- as.Date(usd_pkr$date, format = "%m/%d/%Y")

## Convert all three columns to xts format

usd_pkr_xts <- xts(usd_pkr[, -1,-4], order.by = usd_pkr$date)

usd_pkr_xts <- usd_pkr_xts[,-3]

usd_pkr_xts |> head()
## Plot both series on the same graph

plot(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "USD to PKR", xlab = "Year")


## Dyrange select graphs 

dygraph(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "USD to PKR", xlab = "Year") %>% 
  dyRangeSelector() |> 
  dyShading(from = "1972-1-1", to = "1978-04-1",color = "#FFE6E6") %>%
  dyShading(from = "1978-1-1", to = "1985-1-1",color = "#CCEBD6") 

# Assuming usd_pkr is an xts object with a column named "Date" and another column named "Regime"
# Make sure the "Date" column is in the correct format

# Example data
usd_pkr_data <- data.frame(
  Date = as.Date(c("1972-01-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01", "1977-01-01",
                   "1978-01-01", "1979-01-01", "1980-01-01", "1981-01-01", "1982-01-01", "1983-01-01",
                   "1984-01-01", "1985-01-01", "1986-01-01", "1987-01-01", "1988-01-01", "1989-01-01",
                   "1990-01-01", "1991-01-01", "1992-01-01", "1993-01-01", "1994-01-01", "1995-01-01",
                   "1996-01-01", "1997-01-01", "1998-01-01", "1999-01-01", "2000-01-01", "2001-01-01",
                   "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01", "2007-01-01",
                   "2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01",
                   "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01",
                   "2020-01-01", "2021-01-01", "2022-01-01")),
  Regime = c("PPPP", "PPPP", "PPPP", "PPPP", "PPPP", "PPPP", "PPPP", "Zia", "Zia", "Zia", "Zia", "Zia",
             "Zia", "Zia", "PML", "PML", "PML", "PML", "PPPP", "PPPP", "PPPP", "PML-N", "PML-N", "PPPP",
             "PPPP", "PPPP", "PML-N", "PML-N", "PML-N", "Musharaf", "Musharaf", "Musharaf", "PMLQ&Musharaf",
             "PMLQ&Musharaf", "PMLQ&Musharaf", "PMLQ&Musharaf", "PMLQ&Musharaf", "PPPP", "PPPP", "PPPP",
             "PPPP", "PPPP", "PMLN", "PMLN", "PMLN", "PMLN", "PMLN", "PTI", "PTI", "PTI", "PTI", "PDM")
)

# Convert the "Date" column to a Date object
usd_pkr_data$Date <- as.Date(usd_pkr_data$Date)

# Create an xts object
library(xts)
usd_pkr_xts <- xts(usd_pkr_data$Regime, order.by = usd_pkr_data$Date)



## 
# Define the political regimes in Pakistan from 1947 onward
pakistan_regimes <- c(
  Independence = c("1947-08-15", "1958-10-07"),
  Martial_Law_Ayub_Khan = c("1958-10-07", "1969-03-25"),
  Civilian_Government_Yahya_Khan = c("1969-03-25", "1971-12-20"),
  Military_Rule_Bangladesh_War = c("1971-12-20", "1972-12-20"),
  PPP_Bhutto = c("1972-12-20", "1977-07-05"),
  Martial_Law_Zia_ul_Haq = c("1977-07-05", "1988-08-17"),
  Civilian_Government_Benazir_Bhutto = c("1988-08-17", "1990-08-06"),
  Martial_Law_1990 = c("1990-08-06", "1993-04-26"),
  Civilian_Government_Nawaz_Sharif = c("1993-04-26", "1996-11-05"),
  Martial_Law_1996 = c("1996-11-05", "1997-02-17"),
  Civilian_Government_Nawaz_Sharif_2 = c("1997-02-17", "1999-10-12"),
  Martial_Law_Musharraf = c("1999-10-12", "2008-08-18"),
  PPP_Zardari = c("2008-08-18", "2013-06-05"),
  PMLN_Nawaz_Sharif_3 = c("2013-06-05", "2017-07-28"),
  PMLN_Shahid_Khaqan_Abbasi = c("2017-07-28", "2018-05-31"),
  PTI_Imran_Khan = c("2018-05-31", "2023-01-10"),
  Ongoing_PDM = c("2023-01-10", Sys.Date())  # Assuming the present date for ongoing regime
)

## Insert all these regimes in this graph with their names as well

dygraph(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "Regime", xlab = "Year") %>%
  dyRangeSelector() %>%
  dyShading(from = "1947-01-01", to = "1958-10-07", color = "#FFE6E6") %>%
  dyShading(from = "1958-10-07", to = "1969-03-25", color = "#CCEBD6") %>%
  dyShading(from = "1969-03-25", to = "1971-12-20", color = "#FFE6E6") %>%
  dyShading(from = "1971-12-20", to = "1972-12-20", color = "#CCEBD6") %>%
  dyShading(from = "1972-12-20", to = "1977-07-05", color = "#FFE6E6") %>%
  dyShading(from = "1977-07-05", to = "1988-08-17", color = "#CCEBD6") %>%
  dyShading(from = "1988-08-17", to = "1990-08-06", color = "#FFE6E6") %>%
  dyShading(from = "1990-08-06", to = "1993-04-26", color = "#CCEBD6") %>%
  dyShading(from = "1993-04-26", to = "1996-11-05", color = "#FFE6E6") %>%
  dyShading(from = "1996-11-05", to = "1997-02-17", color = "#CCEBD6") %>%
  dyShading(from = "1997-02-17", to = "1999-10-12", color = "#FFE6E6") %>%
  dyShading(from = "1999-10-12", to = "2008-08-18", color = "#CCEBD6") %>%
  dyShading(from = "2008-08-18", to = "2013-06-05", color = "#FFE6E6") %>%
  dyShading(from = "2013-06-05", to = "2017-07-28", color = "#CCEBD6") %>%
  dyShading(from = "2017-07-28", to = "2018-05-31", color = "#FFE6E6") %>%
  dyShading(from = "2018-05-31", to = "2022-04-10", color = "#CCEBD6") %>%
  dyShading(from = "2022-04-10", to = "2023-09-10", color = "#FFE6E6") %>%
  dyLegend(labelsSeparateLines = TRUE, show = "always") 

## Save this as html file
library(htmlwidgets)
saveWidget(dygraph(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "Regime", xlab = "Year") %>%
  dyRangeSelector() %>%
  dyShading(from = "1947-01-01", to = "1958-10-07", color = "#FFE6E6") %>%
  dyShading(from = "1958-10-07", to = "1969-03-25", color = "#CCEBD6") %>%
  dyShading(from = "1969-03-25", to = "1971-12-20", color = "#FFE6E6") %>%
  dyShading(from = "1971-12-20", to = "1972-12-20", color = "#CCEBD6") %>%
  dyShading(from = "1972-12-20", to = "1977-07-05", color = "#FFE6E6") %>%
  dyShading(from = "1977-07-05", to = "1988-08-17", color = "#CCEBD6") %>%
  dyShading(from = "1988-08-17", to = "1990-08-06", color = "#FFE6E6") %>%
  dyShading(from = "1990-08-06", to = "1993-04-26", color = "#CCEBD6") %>%
  dyShading(from = "1993-04-26", to = "1996-11-05", color = "#FFE6E6") %>%
  dyShading(from = "1996-11-05", to = "1997-02-17", color = "#CCEBD6") %>%
  dyShading(from = "1997-02-17", to = "1999-10-12", color = "#FFE6E6") %>%
  dyShading(from = "1999-10-12", to = "2008-08-18", color = "#CCEBD6") %>%
  dyShading(from = "2008-08-18", to = "2013-06-05", color = "#FFE6E6") %>%
  dyShading(from = "2013-06-05", to = "2017-07-28", color = "#CCEBD6") %>%
  dyShading(from = "2017-07-28", to = "2018-05-31", color = "#FFE6E6") %>%
  dyShading(from = "2018-05-31", to = "2022-04-10", color = "#CCEBD6") %>%
  dyShading(from = "2022-04-10", to = "2023-09-10", color = "#FFE6E6") %>%
  dyLegend(labelsSeparateLines = TRUE, show = "always"), "usd_pkr.html")

library(dygraphs)

# Assuming usd_pkr_xts is your xts object
dygraph(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "Regime", xlab = "Year") %>%
  dyRangeSelector() %>%
  dyShading(from = "1947-01-01", to = "1958-10-07", color = "#FFE6E6") %>%
  dyShading(from = "1958-10-07", to = "1969-03-25", color = "#CCEBD6") %>%
  dyShading(from = "1969-03-25", to = "1971-12-20", color = "#FFE6E6") %>%
  dyShading(from = "1971-12-20", to = "1972-12-20", color = "#CCEBD6") %>%
  dyShading(from = "1972-12-20", to = "1977-07-05", color = "#FFE6E6") %>%
  dyShading(from = "1977-07-05", to = "1988-08-17", color = "#CCEBD6") %>%
  dyShading(from = "1988-08-17", to = "1990-08-06", color = "#FFE6E6") %>%
  dyShading(from = "1990-08-06", to = "1993-04-26", color = "#CCEBD6") %>%
  dyShading(from = "1993-04-26", to = "1996-11-05", color = "#FFE6E6") %>%
  dyShading(from = "1996-11-05", to = "1997-02-17", color = "#CCEBD6") %>%
  dyShading(from = "1997-02-17", to = "1999-10-12", color = "#FFE6E6") %>%
  dyShading(from = "1999-10-12", to = "2008-08-18", color = "#CCEBD6") %>%
  dyShading(from = "2008-08-18", to = "2013-06-05", color = "#FFE6E6") %>%
  dyShading(from = "2013-06-05", to = "2017-07-28", color = "#CCEBD6") %>%
  dyShading(from = "2017-07-28", to = "2018-05-31", color = "#FFE6E6") %>%
  dyShading(from = "2018-05-31", to = "2022-04-10", color = "#CCEBD6") %>%
  dyShading(from = "2022-04-10", to = "2023-09-10", color = "#FFE6E6") %>%
  dyLegend(labelsSeparateLines = TRUE, show = "always")







# Define the political regimes in Pakistan from 1947 onward
pakistan_regimes <- c(
  Independence = c("1947-08-15", "1958-10-07"),
  Martial_Law_Ayub_Khan = c("1958-10-07", "1969-03-25"),
  Civilian_Government_Yahya_Khan = c("1969-03-25", "1971-12-20"),
  Military_Rule_Bangladesh_War = c("1971-12-20", "1972-12-20"),
  PPP_Bhutto = c("1972-12-20", "1977-07-05"),
  Martial_Law_Zia_ul_Haq = c("1977-07-05", "1988-08-17"),
  Civilian_Government_Benazir_Bhutto = c("1988-08-17", "1990-08-06"),
  Martial_Law_1990 = c("1990-08-06", "1993-04-26"),
  Civilian_Government_Nawaz_Sharif = c("1993-04-26", "1996-11-05"),
  Martial_Law_1996 = c("1996-11-05", "1997-02-17"),
  Civilian_Government_Nawaz_Sharif_2 = c("1997-02-17", "1999-10-12"),
  Martial_Law_Musharraf = c("1999-10-12", "2008-08-18"),
  PPP_Zardari = c("2008-08-18", "2013-06-05"),
  PMLN_Nawaz_Sharif_3 = c("2013-06-05", "2017-07-28"),
  PMLN_Shahid_Khaqan_Abbasi = c("2017-07-28", "2018-05-31"),
  PTI_Imran_Khan = c("2018-05-31", "2023-01-10"),
  Ongoing_PDM = c("2023-01-10", Sys.Date())  # Assuming the present date for ongoing regime
)

# Function to add regime annotations
# Function to add regime annotations
regimeAnnotation <- function(dygraph, from, to, text, color) {
  dygraph %>%
    dyShading(from = from, to = to, color = color) %>%
    dyAnnotation(from, text, attachAtBottom = TRUE, width = 60)
}


## Take data from 2000 onward and have ggplot of with annotations and shades of different regimes

pk_usd_2000 <- usd_pkr_xts["2000/"]

# Create a data frame with the date and the regime


library(ggplot2)

# Plot using ggplot2
ggplot(pk_usd_2000_df, aes(x = Date)) +
  geom_line(aes(y = usd2pkr, color = "USD to PKR"),linewidth=1.5, col="red") +
  geom_line(aes(y = annual_growth_rate, color = "black"), linewidth=1.3) +
  labs(title = "USD to PKR Exchange Rate and Annual Growth Rate (2000 Onward)",
       y = "Values") +
  scale_color_manual(name = "Series", values = c("USD to PKR" = "blue", "Annual Growth Rate" = "red")) +
  theme_minimal() +
  geom_rect(data = NULL, aes(
    xmin = as.Date("1999-10-12"),
    xmax = as.Date("2008-08-18"),
    ymin = -Inf,
    ymax = Inf
  ), fill = "#CCEBD6", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE) +
  geom_rect(data = NULL, aes(
    xmin = as.Date("2008-08-18"),
    xmax = as.Date("2013-06-05"),
    ymin = -Inf,
    ymax = Inf
  ), fill = "#c5e2e6", alpha = 0.02, inherit.aes = FALSE, show.legend = FALSE) +
  # Add more geom_rect layers for other regimes as needed
  # Remember to adjust colors, alpha, and date ranges accordingly
  geom_rect(data = NULL, aes(
    xmin = as.Date("2013-06-05"),
    xmax = as.Date("2017-07-28"),
    ymin = -Inf,
    ymax = Inf
  ), fill = "#CCEBD6", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE)+
  geom_rect(data = NULL, aes(
    xmin = as.Date("2017-07-28"),
    xmax = as.Date("2018-05-31"),
    ymin = -Inf,
    ymax = Inf
  ), fill = "#d1c5e6", alpha = 0.02, inherit.aes = FALSE, show.legend = FALSE)+
  geom_rect(data = NULL, aes(
    xmin = as.Date("2018-05-31"),
    xmax = as.Date("2022-04-10"),
    ymin = -Inf,
    ymax = Inf
  ), fill = "#CCEBD6", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE) +
  geom_rect(data = NULL, aes(
    xmin = as.Date("2022-04-10"),
    xmax = as.Date("2023-09-10"),

# Create a data frame for annotations
annotations_df <- data.frame(
  x = as.Date(c("2004-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2022-04-10")),
  y = c(75, 75, 75, 75,75),
  label = c("Musharraf", "PPPP", "PMLN", "PTI", "PDM")
)

# Plot using ggplot2
plt <- ggplot(pk_usd_2000_df, aes(x = Date)) +
  geom_line(aes(y = usd2pkr, color = "USD to PKR"), linewidth = 1.5, col = "red") +
  geom_line(aes(y = annual_growth_rate, color = "black"), linewidth = 1.3) +
  labs(
    title = "USD to PKR Exchange Rate and Annual Growth Rate (2000 Onward)",
    y = "Values"
  ) +
  scale_color_manual(
    name = "Series",
    values = c("USD to PKR" = "blue", "Annual Growth Rate" = "red")
  ) +
  theme_minimal() +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("1999-10-12"),
      xmax = as.Date("2008-08-18"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2008-08-18"),
      xmax = as.Date("2013-06-05"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#c5e2e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2013-06-05"),
      xmax = as.Date("2017-07-28"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2017-07-28"),
      xmax = as.Date("2018-05-31"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d1c5e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2018-05-31"),
      xmax = as.Date("2022-04-10"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2022-04-10"),
      xmax = as.Date("2023-09-10"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d1c5e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = annotations_df,
    aes(x = x, y = y, label = label),
    vjust = 0,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  theme(legend.position = "bottom")  # Adjust legend position as needed

## Save as ragg file
library(ragg)
ragg::agg_png("usd2pkr.png", width = 16, height = 9, units = "in", res = 300)
plt
dev.off()

exchange_rate <- ggplot(pk_usd_2000_df, aes(x = Date)) +
  geom_line(aes(y = usd2pkr, color = "USD to PKR"), linewidth = 1.5, col = "red") +
  geom_line(aes(y = annual_growth_rate, color = "black"), linewidth = 1.3) +
  labs(
    title = "USD to PKR Exchange Rate and Annual Growth Rate (2000 Onward)",
    y = "Values"
  ) +
  scale_color_manual(
    name = "Series",
    values = c("USD to PKR" = "blue", "Annual Growth Rate" = "red")
  ) +
  theme_minimal() +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("1999-10-12"),
      xmax = as.Date("2008-08-18"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2008-08-18"),
      xmax = as.Date("2013-06-05"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#c5e2e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2013-06-05"),
      xmax = as.Date("2017-07-28"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2017-07-28"),
      xmax = as.Date("2018-05-31"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d1c5e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2018-05-31"),
      xmax = as.Date("2022-04-10"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#CCEBD6",
    alpha = 0.01,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_rect(
    data = NULL,
    aes(
      xmin = as.Date("2022-04-10"),
      xmax = as.Date("2023-09-10"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d1c5e6",
    alpha = 0.02,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = annotations_df,
    aes(x = x, y = y, label = label),
    vjust = 0,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  theme(legend.position = "bottom")  # Adjust legend position as needed

library(plotly)

ggplotly(exchange_rate)
