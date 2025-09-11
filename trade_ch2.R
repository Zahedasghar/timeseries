## https://r.tiid.org/Trade_indicators/trade-performance.html

library(tidyverse) # for manipulating data
library(readxl) # for reading in data files in a clean format
options(scipen = 100) # turn off scientific notation

trade <- read_csv("data/TradeData_all-world-16-20.csv")

trade1 <- read_csv("data/TradeData_all-world-11-15.csv")


# import 2016-2020 data

# select and rename columns
data <- trade %>% 
  select(RefYear, ReporterDesc, FlowDesc, PartnerDesc, PrimaryValue) %>% 
  rename(year = RefYear, reporter = ReporterDesc, trade_direction = FlowDesc, 
         partner = PartnerDesc, trade_value_usd = PrimaryValue)


# select and rename columns
data_prev <- trade1 %>% 
  select(RefYear, ReporterDesc, FlowDesc, PartnerDesc, PrimaryValue) %>% 
  rename(year = RefYear, reporter = ReporterDesc, trade_direction = FlowDesc, 
         partner = PartnerDesc, trade_value_usd = PrimaryValue)


trade2 <- read_csv("data/TradeData_all-world-21-24.csv") |> 
  select(refYear, reporterDesc, flowDesc, partnerDesc, primaryValue) |>
  rename(year = refYear, reporter = reporterDesc, trade_direction = flowDesc, 
         partner = partnerDesc, trade_value_usd = primaryValue)

# combine three  datasets



data <- rbind(data_prev, data)

data <- rbind(data, trade2)
rm(data_prev)
rm(trade2)
# view the dataset
data

# 
# Growth Rate of Exports 

GRX <- data %>% 
  # keep data on exports from China and India, select columns of interest
  filter(reporter %in% c("China") & trade_direction == "Export") %>% 
  select(reporter, year, trade_value_usd)

GRX <- GRX %>% 
  # arrange the data set by year in descending order
  arrange(desc(year))

GRX

## Section 2.2 is omitted 



# Normalised trad balance -------------------------------------------------

# create a vector with names of selected economies
group <- c("Bangladesh", "China", "India", "Indonesia", "Malaysia", 
         "Pakistan" , "New Zealand", "Philippines", "Rep. of Korea", "Singapore", 
           "Sri Lanka", "Thailand", "Viet Nam")

# filter to only keep data on export and import flows of the selected economies in 2022
NTB <- data %>% 
  filter(reporter %in% group & year == 2022) %>% 
  select(reporter, trade_direction, trade_value_usd)

# reshape the dataset to keep export and import data in separate columns
NTB <- NTB %>% spread(trade_direction, trade_value_usd)

# calculate NTB value, and round it 
NTB$Normalized_Trade_Balance <- (NTB$Export - NTB$Import) / (NTB$Export + NTB$Import)
NTB$Normalized_Trade_Balance <- round(NTB$Normalized_Trade_Balance, 2)

NTB


# create a bar chart by 
NTB_plot <- NTB %>% 
  # selecting your variables, and reordering reporters alphabetically
  ggplot(aes(x = Normalized_Trade_Balance, y = reorder(reporter, desc(reporter)))) +
  # adding a bar chart to the chart area, specifying the bar width and removing the legend
  geom_bar(aes(fill = Normalized_Trade_Balance), stat = 'identity', width = 0.6, show.legend = F) +
  # adding the chart title and removing axis labels
  labs(title = "Normalized Trade Balance for Selected Economies (2022)", x = NULL, y = NULL) +
  # adding a vertical line indicating location of 0 on x axis  
  geom_vline(xintercept = 0) +
  # applying the minimal theme
  theme_minimal()

NTB_plot



# Export Import Coverage --------------------------------------------------

# get the dataset from previous section
XMC <- NTB[,-4]

# calculate export/import coverage for selected economies in 2022
XMC$XM_Coverage <- XMC$Export / XMC$Import
XMC$XM_Coverage <- round(XMC$XM_Coverage, 2)

XMC

# create a bar chart by

# create a bar chart by selecting your variables, and reordering reporters alphabetically
XMC_plot <- ggplot(XMC, aes(x = XM_Coverage, y = reorder(reporter, desc(reporter)))) +
  # adding a bar chart to the chart area, specifying the bar width and removing the legend
  geom_bar(fill = 'dodgerblue4', stat='identity', width = 0.6, show.legend = FALSE) +
  # adding the chart title and removing axis labels
  labs(title = "Export/Import Coverage for Selected Economies (2022)", x = NULL, y = NULL) +
  # adding a vertical line indicating location of 1 on x axis  
  geom_vline(xintercept = 1, color = 'red', linewidth = 1) +
  # applying the minimal theme
  theme_minimal()

XMC_plot

## Make Pakistan , Bangladesh and India red 

# Define the countries to be highlighted in red
highlight_countries <- c("Pakistan")

# Create a bar chart with conditional coloring
XMC_plot <- ggplot(XMC, aes(x = XM_Coverage, y = reorder(reporter, desc(reporter)))) +
  # Adding a bar chart with conditional coloring
  geom_bar(aes(fill = ifelse(reporter %in% highlight_countries, "Highlighted", "Normal")),
           stat = 'identity', width = 0.6, show.legend = FALSE) +
  # Adding the chart title and removing axis labels
  labs(title = "Export/Import Coverage for Selected Economies (2022)", x = NULL, y = NULL) +
  # Adding a vertical line indicating location of 1 on x axis  
  geom_vline(xintercept = 1, color = 'red', linewidth = 1) +
  # Applying the minimal theme
  theme_minimal() +
  # Customizing fill colors
  scale_fill_manual(values = c("Highlighted" = "red", "Normal" = "dodgerblue4"))+ theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5), axis.text.y = element_text(margin = margin(r = -10))) +
  labs(subtitle = "Pakistan is the only country with having less than 50% import coverage from exports")+
  labs(caption="Source: UN Comtrade") 

XMC_plot

highlight_countries <- c("Pakistan", "Bangladesh")

XMC_plot <- ggplot(XMC, aes(x = XM_Coverage, y = reorder(reporter, desc(reporter)))) +
  # Adding a bar chart with conditional coloring
  geom_bar(aes(fill = ifelse(reporter %in% highlight_countries, "Highlighted", "Normal")),
           stat = 'identity', width = 0.6, show.legend = FALSE) +
  # Adding the chart title, subtitle, and caption
  labs(
    title = "Export/Import Coverage for Selected Economies (2022)",
    subtitle = "Pakistan is the only country with having less than 50% import coverage from exports. This is only for merchandise trade , services are not covered for all countries",
    caption = "By: Zahid Asghar, Source: UN Comtrade",
    x = NULL,
    y = NULL
  ) +
  # Adding a vertical line indicating location of 1 on x axis  
  geom_vline(xintercept = 1, color = 'red', linewidth = 1) +
  # Customizing fill colors
  scale_fill_manual(values = c("Highlighted" = "red", "Normal" = "dodgerblue4")) +
  # Applying the minimal theme with adjustments
  theme_minimal(base_size = 14) +  # Increase base size for better visibility
  theme(
    plot.margin = margin(10, 10, 10, 10),  # Adjust margins for better spacing
    axis.text.y = element_text(margin = margin(r = -10)),  # Reduce gap between labels and bars
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Center-align title
    plot.subtitle = element_text(size = 12, hjust = 0, face = "italic", margin = margin(b = 10)),  # Left-align subtitle
    plot.caption = element_text(size = 10, hjust = 0)  # Left-align caption
  )

# Display the plot
XMC_plot

# Save the plot with high resolution
ggsave("export_import_plot.png", plot = XMC_plot, width = 12, height = 8, dpi = 300,bg="white")
# Save the plot with high resolution
ggsave("export_import_plot.pdf", plot = XMC_plot, width = 12, height = 8, dpi = 300,bg="white")
