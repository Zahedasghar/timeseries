library(tidyverse)
library(tidymodels)
library(tidyquant)
library(timetk)
library(modeltime)
library(lubridate)
library(zoo)
library(boot)

# Load monthly export data from CSV, skipping the first row
df_exports_raw <- read_csv('data/total_export_receipts_krndaz.csv', skip = 1)
print("Raw Exports Data:")
print(head(df_exports_raw))

# Clean and parse the data
df_exports <- df_exports_raw %>%
  rename(date = Monthly, exports = `US$  (Millions)`) %>%
  mutate(date = as.yearmon(date, format = "%b %Y")) %>%
  mutate(exports = str_remove_all(exports, "[^0-9\\.]+")) %>%
  mutate(exports = as.numeric(exports)) %>%
  drop_na() %>%
  mutate(exports = exports / lag(exports) - 1) %>%
  drop_na()

# Print to verify the cleaned data
print("Cleaned Exports Data:")
print(head(df_exports))

# Monthly change (%) of USD/PKR exchange rates
# https://finance.yahoo.com/quote/PKR%3DX/
df_usdpkr <- 
  tq_get("PKR=x", to = "2024-07-01") %>% 
  tq_transmute(select = "close",
               mutate_fun = to.monthly) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "usd_pkr")

# Print to verify the data
print("USD/PKR Data:")
print(head(df_usdpkr))

# Monthly change (%) of KSE-100 index
# https://finance.yahoo.com/quote/%5EKSE/
df_kse100 <- 
  tq_get("^KSE", to = "2024-07-01") %>% 
  tq_transmute(select = "close",
               mutate_fun = to.monthly) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "kse100")

# Print to verify the data
print("KSE-100 Data:")
print(head(df_kse100))

# Merging series
df_merged <- 
  df_exports %>% 
  left_join(df_usdpkr, by = "date") %>% 
  left_join(df_kse100, by = "date") %>% 
  drop_na()

# Print to verify the merged data
print("Merged Data:")
print(head(df_merged))

# Change of monthly %
df_merged %>% 
  pivot_longer(-date, names_to = "vars") %>% 
  filter(date >= as.yearmon("2021-01")) %>% 
  mutate(type = case_when(
    vars == "exports" ~ "Exports in Pakistan",
    vars == "usd_pkr" ~ "USD/PKR",
    vars == "kse100" ~ "KSE-100",
    TRUE ~ vars
  )) %>% 
  ggplot(aes(date, value, color = vars)) +
  geom_line(linewidth = 1.25) +
  geom_text(
    data = . %>%  slice_tail(n = 1, by = type),
    aes(label = type),
    hjust = 0,
    vjust = 0,
    nudge_x = 0.05,
    size = 5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_yearmon(format = "%Y-%m",
                  expand = expansion(mult = c(0, .3))) +
  labs(x = "", 
       y = "",
       subtitle = "Change of monthly %") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())



# Filter data for the last 10 years
start_date <- as.yearmon(Sys.Date()) - 10
df_merged_filtered <- df_merged %>% filter(date >= start_date)

# Print to verify the filtered data
print("Filtered Merged Data:")
print(head(df_merged_filtered))

# Determine the last date in the dataset
end_date <- max(df_merged_filtered$date)


# Change of monthly %
df_merged_filtered %>% 
  pivot_longer(-date, names_to = "vars") %>% 
  filter(date >= start_date) %>% 
  mutate(type = case_when(
    vars == "exports" ~ "Exports in Pakistan",
    vars == "usd_pkr" ~ "USD/PKR",
    vars == "kse100" ~ "KSE-100",
    TRUE ~ vars
  )) %>% 
  ggplot(aes(date, value, color = type)) +
  geom_line(linewidth = 1.25) +
  geom_text(
    data = . %>% group_by(type) %>% filter(date == max(date)),
    aes(label = type),
    hjust = -0.1,
    vjust = -0.5,
    size = 5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_yearmon(limits = c(start_date, end_date), format = "%Y-%m",
                  expand = expansion(mult = c(0, .1))) +
  labs(x = "", 
       y = "",
       subtitle = "Change of monthly %") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())



# Bootstrap confidence intervals using the `boot` package
set.seed(12345)

# Define the regression function
regression_function <- function(data, indices) {
  d <- data[indices, ]  # allows boot to select sample
  fit <- glm(exports ~ usd_pkr, data = d)
  return(coef(fit))
}

# Bootstrapping
boot_results <- boot(data = df_merged_filtered, statistic = regression_function, R = 1000)

# Extract bootstrap estimates
boot_estimates <- boot_results$t

# Create a data frame for plotting
boot_df <- as.data.frame(boot_estimates)
colnames(boot_df) <- names(coef(glm(exports ~ usd_pkr, data = df_merged_filtered)))

# Plotting the bootstrap confidence intervals
boot_df %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "estimate") %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  labs(x = "", 
       y = "",
       subtitle = "The distribution includes zero which means there is no significant effect",
       title = "The estimated effect of USD/PKR rates on exports in Pakistan") +
  theme_minimal(base_size = 16) +
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 16))
