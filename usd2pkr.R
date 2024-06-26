library(readxl)
library(tidyverse)
library(xts)
library(dygraphs)
library(htmlwidgets)
usd_pkr <- read_excel("docs/data/usd2pkr_asadzaman.xlsx", sheet="Data", skip=1)

library(janitor)

names(usd_pkr) <- c("date", "usd2pkr", "annual_growth_rate","years")

# Remove rows with missing values
usd_pkr <- usd_pkr %>% drop_na(annual_growth_rate, usd2pkr)




usd_pkr <- usd_pkr |> dplyr::select(1:4)

## Convert date to month, day, year format

usd_pkr$date <- as.Date(usd_pkr$date, format = "%m/%d/%Y")

## Convert all three columns to xts format

usd_pkr_xts <- xts(usd_pkr[, -1,-4], order.by = usd_pkr$date)

usd_pkr_xts <- usd_pkr_xts[,-3]

# ## Calculate variable relative difference of usd2pkr to annual growth rate (usd2pkr-annual_growth_rate)/annual_growth_rate*100
# 
# usd_pkr_xts$relative_diff <- (usd_pkr_xts$usd2pkr - usd_pkr_xts$annual_growth_rate)/usd_pkr_xts$annual_growth_rate*100



## Plot both series on the same graph

plot(usd_pkr_xts, main = "USD to PKR Exchange Rate", ylab = "USD to PKR", xlab = "Year")




#plot(usd_pkr_xts$relative_diff, main = "Relative Difference of USD to PKR Exchange Rate", ylab = "Relative Difference", xlab = "Year")

# dygraph(usd_pkr_xts$relative_diff, main = "Relative Difference of USD to PKR Exchange Rate", ylab = "Relative Difference", xlab = "Year") |> 
#   dyRangeSelector() 

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


## Take data from 2000 onward till 2024 

pk_usd_2000 <- usd_pkr_xts["2000/2024"]

# Create a data frame with the date and the regime
pk_usd_2000 <- data.frame(Date = index(pk_usd_2000), pk_usd_2000)

library(ggplot2)

# Plot using ggplot2
ggplot(pk_usd_2000, aes(x = Date)) +
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
    ymin = -Inf,
    ymax = Inf), fill = "#d1c5e6", alpha = 0.02, inherit.aes = FALSE, show.legend = FALSE) 
# Create a data frame for annotations
annotations_df <- data.frame(
  x = as.Date(c("2004-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2022-04-10")),
  y = c(75, 75, 75, 75,75),
  label = c("Musharraf", "PPPP", "PMLN", "PTI", "PDM")
)


# Define color palette
regime_colors <- c("#CCEBD6", "#c5e2e6", "#CCEBD6", "#d1c5e6", "#CCEBD6", "#d1c5e6")

# Define regime boundaries
regime_boundaries <- c(
  as.Date("1999-10-12"),
  as.Date("2008-08-18"),
  as.Date("2013-06-05"),
  as.Date("2017-07-28"),
  as.Date("2018-05-31"),
  as.Date("2022-04-10"),
  as.Date("2023-09-10")
)








pk_long <- pk_usd_2000 %>%
  pivot_longer(cols = c("usd2pkr", "annual_growth_rate"), names_to = "series", values_to = "value")

## Line plot

pk_long |> 
ggplot()+aes(x=Date,y=value, color=series)+
  geom_line(linewidth=1)+theme_minimal()+
  labs(x="",y="Values",labs="USD to PKR Exchange Rate and Annual Growth Rate (2000 Onward)")+
  theme(legend.position = "bottom")+ 
  annotate(geom="point", x=as.Date("2004-10-01"), y=155, size=15, shape=21, fill="transparent") 
  

# Define the data frame for annotations
annotations_df <- data.frame(
  x = as.Date(c("2004-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2022-04-10")),
  y = c(75, 75, 75, 75, 75),
  label = c("Musharraf/PMLQ", "PPPP", "PMLN", "PTI", "PDM")
)

# Create the plot
pk_long |> 
  ggplot() +
  aes(x = Date, y = value, color = series) +
  geom_line(linewidth = 1) + 
  theme_minimal() +
  geom_rect(
    aes(xmin = as.Date("1999-10-12"), xmax = as.Date("2008-08-18"), ymin = -Inf, ymax = Inf),
    fill = "lightblue", alpha = 0.011, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2008-08-18"), xmax = as.Date("2013-06-05"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2013-06-05"), xmax = as.Date("2018-05-31"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2018-05-31"), xmax = as.Date("2022-04-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2022-04-10"), xmax = as.Date("2023-09-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    data = annotations_df,
    aes(x = x, y = y, label = label),
    vjust = 0,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  labs(x = "", y = "", title = "PKR to USD Exchange Rate") +
  scale_color_manual(values = c("usd2pkr" = "red", "annual_growth_rate" = "blue")) +
  theme(
    legend.position = c(0.2, 0.8),  # Adjust legend position
    legend.background = element_rect(fill = "white", size = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8)
  )+ labs(caption="exchangerates.org.uk")


plot <- pk_long |> 
  ggplot() +
  aes(x = Date, y = value, color = series) +
  geom_line(linewidth = 1) + 
  theme_minimal() +
  geom_rect(
    aes(xmin = as.Date("1999-10-12"), xmax = as.Date("2008-08-18"), ymin = -Inf, ymax = Inf),
    fill = "lightblue", alpha = 0.011, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2008-08-18"), xmax = as.Date("2013-06-05"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2013-06-05"), xmax = as.Date("2018-05-31"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2018-05-31"), xmax = as.Date("2022-04-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2022-04-10"), xmax = as.Date("2023-09-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    data = annotations_df,
    aes(x = x, y = y, label = label),
    vjust = 0,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  labs(x = "", y = "", title = "Politics and PKR to USD Exchange Rate") +
  scale_color_manual(values = c("usd2pkr" = "red", "annual_growth_rate" = "blue")) +
  theme(
    legend.position = c(0.2, 0.8),  # Adjust legend position
    legend.background = element_rect(fill = "white", size = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8)
  )+ labs(caption="exchangerates.org.uk")


library(ragg)
ragg::agg_png("exchange_ragg_14x10.png", width = 14, height = 10, units = "in", res = 250, scaling = 2)
plot
dev.off()


+
  annotate(geom="text", x=as.Date("2008-01-01"), y=100, 
           label="Lawyer's \n Movement", size=2)+
  annotate(geom="point", x=as.Date("2007-12-01"), y=100, size=15, shape=21, fill="transparent")+
annotate(geom="text", x=as.Date("2014-01-01"), y=130, 
         label="PTI Dharna", size=2) + 
  annotate(geom="point", x=as.Date("2014-10-01"), y=130, size=15, shape=21, fill="transparent")+
    annotate(geom="text", x=as.Date("2017-12-01"), y=140, 
           label="Panama",size=2) + 
  annotate(geom="point", x=as.Date("2017-12-01"), y=140, size=15, shape=21, fill="transparent")+
  annotate(geom="point", x=as.Date("2020-09-01"), y=160, size=15, shape=21, fill="transparent") +
  annotate(geom="text", x=as.Date("2020-09-01"), y=160, 
           label="Covid-19", size=2) + 
  annotate(geom="point", x=as.Date("2022-04-01"), y=200, size=15, shape=21, fill="transparent") +
  annotate(geom="text", x=as.Date("2022-04-01"), y=200, 
           label="VNC", size=2) 





library(ggplot2)

# Assuming you have a Date column in your data
# If not, replace 'Date' with the actual column name

## Data in descending date order

pk_long <- pk_long[order(pk_long$Date), ]




pk_long
pk_long %>%
  ggplot() +
  aes(x = Date, y = value, color = series) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  scale_x_date(limits = rev(range(pk_long$Date)))
+
  geom_rect(
    aes(xmin = as.Date("1999-10-12"), xmax = as.Date("2008-08-18"), ymin = -Inf, ymax = Inf),
    fill = "lightblue", alpha = 0.011, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2008-08-18"), xmax = as.Date("2013-06-05"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2013-06-05"), xmax = as.Date("2018-05-31"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2018-05-31"), xmax = as.Date("2022-04-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgreen", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_rect(
    aes(xmin = as.Date("2022-04-10"), xmax = as.Date("2023-09-10"), ymin = -Inf, ymax = Inf),
    fill = "lightgrey", alpha = 0.01, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_text(
    data = annotations_df,
    aes(x = x, y = y, label = label),
    vjust = 0,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  labs(x = "", y = "", title = "Politics and PKR to USD Exchange Rate") +
  scale_color_manual(values = c("usd2pkr" = "red", "annual_growth_rate" = "blue")) +
  theme(
    legend.position = c(0.2, 0.8),  # Adjust legend position
    legend.background = element_rect(fill = "white", size = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8)
  )+ labs(caption="exchangerates.org.uk")