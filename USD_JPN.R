library(tidyverse)
library(tidymodels)
library(tidyquant)
library(timetk)
library(modeltime)

#Quarterly change (%) of USD/JPY exchange rates
#https://finance.yahoo.com/quote/JPY%3DX/
df_usdjpy <- 
  tq_get("JPY=x", to = "2024-07-01") %>% 
  tq_transmute(select = "close",
               mutate_fun = to.quarterly) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "quarterly",
               col_rename = "usd_jpy")



#Quarterly change (%) of exports of goods and services in Japan
#https://fred.stlouisfed.org/series/JPNEXPORTQDSNAQ
df_exports <- 
  tq_get("JPNEXPORTQDSNAQ", 
         get = "economic.data") %>% 
  select(date, exports = price) %>% 
  mutate(date = as.yearqtr(date),
         exports = exports / lag(exports) - 1) %>% 
  drop_na()


#Quarterly change (%) of Nikkei 225
#https://finance.yahoo.com/quote/%5EN225/
df_nikkei <- 
  tq_get("^N225", to = "2024-07-01") %>% 
  tq_transmute(select = "close",
               mutate_fun = to.quarterly) %>% 
  tq_transmute(mutate_fun = periodReturn,
               col_rename = "nikkei225")


#Merging series
df_merged <- 
  df_exports %>% 
  left_join(df_usdjpy) %>% 
  left_join(df_nikkei) 


#Change of quarterly %
df_merged %>% 
  pivot_longer(-date, names_to = "vars") %>% 
  filter(date >= 2021) %>% 
  mutate(type = case_when(
    vars == "exports" ~ "Exports in Japan",
    vars == "usd_jpy" ~ "USD/JPY",
    vars == "nikkei225" ~ "Nikkei 225",
    TRUE ~ vars
  )) %>% 
  ggplot(aes(date, value, color = vars)) +
  geom_line(linewidth = 1.25) +
  geom_text(
    data = . %>%  slice_tail(n = 1, by = type),
    aes(label = type),
    hjust = 0,
    vjust = 0,
    family = "Bricolage Grotesque",
    nudge_x = 0.05,
    size = 5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_yearqtr(format = "%Y Q%q",
                  expand = expansion(mult = c(0, .3))) +
  labs(x = "", 
       y = "",
       subtitle = "Change of quarterly %") +
  theme_minimal(base_family = "Bricolage Grotesque",
                base_size = 16) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
