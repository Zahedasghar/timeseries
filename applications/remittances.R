# ---------------------------------------------------------------
# Remittances to Pakistan from the SBP EasyData API
# Author: Zahid Asghar
# ---------------------------------------------------------------
# Before running: put your key in ~/.Renviron as
#   SBP_EASYDATA_KEY=your40characterkey
# (name once, then the key; end the file with a blank line)
# then restart R and check:  nchar(Sys.getenv("SBP_EASYDATA_KEY"))  == 40
# ---------------------------------------------------------------

library(tidyverse)
library(httr2)
library(fpp3)     # brings in tsibble, feasts, fable
library(plotly)

theme_set(theme_minimal(base_size = 12))


# ---------------------------------------------------------------
# 1. One function to download one series
# ---------------------------------------------------------------
# SBP's firewall rejects R's default user agent, so we pretend to be
# a browser. The gsub() fixes the bare carriage returns SBP sends.

get_sbp_series <- function(series_id,
                           start_date = "2022-07-01",
                           end_date   = Sys.Date()) {

  key <- Sys.getenv("SBP_EASYDATA_KEY")
  if (key == "") stop("SBP_EASYDATA_KEY not found in .Renviron")

  browser_agent <- paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
  )

  text <- request("https://easydata.sbp.org.pk/api/v1/series") |>
    req_url_path_append(series_id, "data") |>
    req_url_query(
      api_key    = key,
      start_date = as.character(start_date),
      end_date   = as.character(end_date),
      format     = "csv"
    ) |>
    req_user_agent(browser_agent) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_string()

  text <- gsub("\r\n?", "\n", text)

  read_csv(I(text), show_col_types = FALSE)
}

# Quick test - should print 6 rows for "Other Countries"
test <- get_sbp_series("TS_GP_BOP_WR_M.WR0320", "2024-01-01", "2024-06-30")
print(test)


# ---------------------------------------------------------------
# 2. Find the country codes (run once)
# ---------------------------------------------------------------
# Every country has a code like WR0010, WR0020, ... We ask for one
# month of each and keep the ones that answer. Takes a few minutes.

lookup_file <- "data/sbp_wr_series.csv"
dir.create("data", showWarnings = FALSE)

if (!file.exists(lookup_file)) {

  codes  <- sprintf("WR%04d", seq(10, 600, by = 10))
  found  <- list()

  for (code in codes) {
    id <- paste0("TS_GP_BOP_WR_M.", code)

    one <- tryCatch(
      get_sbp_series(id, "2024-06-01", "2024-06-30"),
      error = function(e) NULL
    )

    if (!is.null(one) && nrow(one) > 0) {
      found[[code]] <- tibble(
        series_id   = id,
        series_name = one$`Series Name`[1]
      )
      cat(code, "->", one$`Series Name`[1], "\n")
    }

    Sys.sleep(0.3)   # be polite to the server
  }

  series_lookup <- bind_rows(found)
  write_csv(series_lookup, lookup_file)
}

series_lookup <- read_csv(lookup_file, show_col_types = FALSE)
print(series_lookup, n = Inf)


# ---------------------------------------------------------------
# 3. Shorten the labels into country names
# ---------------------------------------------------------------
# SBP labels look like "Workers' remittances received from Saudi Arabia"

series_lookup <- series_lookup |>
  mutate(
    country = str_remove(series_name,
                         "^Workers' remittances received from "),
    country = str_squish(country)
  )

# Keep only the countries you want, or leave as is to take them all
# series_lookup <- series_lookup |>
#   filter(country %in% c("Saudi Arabia", "U.A.E.", "U.K.", "U.S.A."))


# ---------------------------------------------------------------
# 4. Download everything (cached for a week)
# ---------------------------------------------------------------

cache_file <- "data/remittances_sbp.rds"

if (file.exists(cache_file) &&
    difftime(Sys.time(), file.mtime(cache_file), units = "days") < 7) {

  rmt <- read_rds(cache_file)
  message("Using cached data from ", file.mtime(cache_file))

} else {

  all_rows <- list()

  for (i in seq_len(nrow(series_lookup))) {
    id      <- series_lookup$series_id[i]
    country <- series_lookup$country[i]

    cat("Downloading", country, "\n")

    one <- get_sbp_series(id)

    all_rows[[i]] <- tibble(
      country     = country,
      date        = as.Date(one$`Observation Date`),
      remittances = as.numeric(one$`Observation Value`)
    )

    Sys.sleep(0.2)
  }

  rmt <- bind_rows(all_rows) |>
    filter(!is.na(date), !is.na(remittances)) |>
    arrange(country, date)

  write_rds(rmt, cache_file)
}

glimpse(rmt)
range(rmt$date)

# To force a fresh download:  file.remove(cache_file)


# ---------------------------------------------------------------
# 5. Wide version, if you prefer the old layout
# ---------------------------------------------------------------

rmt_wide <- rmt |>
  pivot_wider(names_from = country, values_from = remittances) |>
  arrange(date)

head(rmt_wide)


# ---------------------------------------------------------------
# 6. Which countries matter most?
# ---------------------------------------------------------------
# "Total" is one of the series, so keep it separate from the countries.

total_name <- series_lookup$country[str_detect(series_lookup$country,
                                               regex("total", ignore_case = TRUE))]

countries_only <- rmt |> filter(!country %in% total_name)

top6 <- countries_only |>
  group_by(country) |>
  summarise(average = mean(remittances)) |>
  arrange(desc(average)) |>
  slice(1:6) |>
  pull(country)

top6


# ---------------------------------------------------------------
# 7. Plot the trends
# ---------------------------------------------------------------

p_trend <- countries_only |>
  filter(country %in% top6) |>
  ggplot(aes(date, remittances, colour = country)) +
  geom_line() +
  labs(
    title   = "Workers' remittances to Pakistan by source country",
    x       = NULL,
    y       = "US$ million",
    colour  = NULL,
    caption = "Source: State Bank of Pakistan, EasyData"
  )

p_trend
ggplotly(p_trend)


# ---------------------------------------------------------------
# 8. Country shares of the total
# ---------------------------------------------------------------

if (length(total_name) > 0) {
  totals <- rmt |>
    filter(country %in% total_name) |>
    select(date, total = remittances)
} else {
  totals <- countries_only |>
    group_by(date) |>
    summarise(total = sum(remittances))
}

shares <- countries_only |>
  left_join(totals, by = "date") |>
  mutate(share = remittances / total)

# Pick countries by their AVERAGE share, not month by month,
# otherwise the lines come out broken.
big_share <- shares |>
  group_by(country) |>
  summarise(average_share = mean(share, na.rm = TRUE)) |>
  filter(average_share > 0.05) |>
  pull(country)

p_share <- shares |>
  filter(country %in% big_share) |>
  ggplot(aes(date, share, colour = country)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title  = "Share of total remittances by source country",
    x      = NULL,
    y      = "Share of total",
    colour = NULL
  )

p_share


# ---------------------------------------------------------------
# 9. Turn into a tsibble for forecasting
# ---------------------------------------------------------------
# Observation dates are month-end, so yearmonth() lines them up.

ts_data <- rmt |>
  mutate(month = yearmonth(date)) |>
  select(country, month, remittances) |>
  as_tsibble(key = country, index = month) |>
  fill_gaps()     # countries start at different dates

ts_data


# ---------------------------------------------------------------
# 10. Seasonality for one country
# ---------------------------------------------------------------

focus <- top6[1]

ts_data |>
  filter(country == focus) |>
  model(STL(remittances)) |>
  components() |>
  autoplot() +
  labs(title = paste("STL decomposition:", focus))

ts_data |>
  filter(country == focus) |>
  gg_subseries(remittances) +
  labs(title = paste("Seasonal pattern:", focus), y = "US$ million")


# ---------------------------------------------------------------
# 11. Compare three models on a holdout
# ---------------------------------------------------------------
# Fit on everything except the last 24 months, then check which
# model did best on those 24 months.

model_data <- ts_data |> filter(country %in% top6)

train <- model_data |> filter(month < max(month) - 23)

fits <- train |>
  model(
    snaive = SNAIVE(remittances),
    ets    = ETS(remittances),
    arima  = ARIMA(remittances)
  )

test_fc <- fits |> forecast(h = "24 months")

test_fc |>
  accuracy(model_data) |>
  select(country, .model, RMSE, MAE, MAPE) |>
  arrange(country, RMSE) |>
  print(n = Inf)


# ---------------------------------------------------------------
# 12. Forecast 12 months ahead using all the data
# ---------------------------------------------------------------

final_fits <- model_data |>
  model(
    snaive = SNAIVE(remittances),
    ets    = ETS(remittances),
    arima  = ARIMA(remittances)
  )

fc <- final_fits |> forecast(h = "12 months")

recent <- model_data |> filter(month >= max(month) - 60)

fc |>
  autoplot(recent, level = 80) +
  facet_wrap(~ country, scales = "free_y", ncol = 2) +
  labs(title = "12-month forecasts by model", x = NULL, y = "US$ million")


# ---------------------------------------------------------------
# 13. Forecast the total
# ---------------------------------------------------------------

if (length(total_name) > 0) {
  total_ts <- ts_data |> filter(country %in% total_name)
} else {
  total_ts <- rmt |>
    mutate(month = yearmonth(date)) |>
    group_by(month) |>
    summarise(remittances = sum(remittances)) |>
    as_tsibble(index = month)
}

total_ts |>
  model(
    ets   = ETS(remittances),
    arima = ARIMA(remittances)
  ) |>
  forecast(h = "12 months") |>
  autoplot(total_ts |> filter(month >= max(month) - 60), level = 80) +
  labs(title = "Total remittances: 12-month forecast",
       x = NULL, y = "US$ million")


# ---------------------------------------------------------------
# 14. Are the residuals clean?
# ---------------------------------------------------------------

final_fits |>
  filter(country == focus) |>
  select(arima) |>
  gg_tsresiduals() +
  labs(title = paste("ARIMA residuals:", focus))
