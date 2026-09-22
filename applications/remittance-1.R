# ===============================================================
# Workers' remittances to Pakistan
# Download from SBP EasyData, seasonally adjust, forecast
# Author: Zahid Asghar
# ===============================================================
# Setup, once:
#   install.packages(c("tidyverse", "httr2", "fpp3", "seasonal", "plotly"))
#   usethis::edit_r_environ()   ->  SBP_EASYDATA_KEY=your40characterkey
#   restart R, then check:  nchar(Sys.getenv("SBP_EASYDATA_KEY")) == 40
# ===============================================================

library(tidyverse)
library(httr2)
library(fpp3)        # tsibble + feasts + fable
library(seasonal)    # X-13ARIMA-SEATS
library(plotly)

theme_set(theme_minimal(base_size = 12))
dir.create("data", showWarnings = FALSE)


# ===============================================================
# PART 1  DOWNLOAD
# ===============================================================

# ---------------------------------------------------------------
# 1.1 One function to fetch one series
# ---------------------------------------------------------------
# Two quirks handled here:
#  - SBP's firewall rejects R's default user agent, so we look like
#    a browser
#  - the CSV uses bare carriage returns, which readr mis-parses

get_sbp_series <- function(series_id,
                           start_date = "2000-01-01",
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

# Quick test
test <- get_sbp_series("TS_GP_BOP_WR_M.WR0320", "2024-01-01", "2024-06-30")
print(test)


# ---------------------------------------------------------------
# 1.2 Discover the series codes (runs once, then reads the file)
# ---------------------------------------------------------------

lookup_file <- "data/sbp_wr_series.csv"

if (!file.exists(lookup_file)) {

  codes <- sprintf("WR%04d", seq(10, 600, by = 10))
  found <- list()

  for (code in codes) {
    id  <- paste0("TS_GP_BOP_WR_M.", code)
    one <- tryCatch(get_sbp_series(id, "2024-06-01", "2024-06-30"),
                    error = function(e) NULL)

    if (!is.null(one) && nrow(one) > 0) {
      found[[code]] <- tibble(series_id   = id,
                              series_name = one$`Series Name`[1])
      cat(code, "->", one$`Series Name`[1], "\n")
    }
    Sys.sleep(0.3)
  }

  write_csv(bind_rows(found), lookup_file)
}

series_lookup <- read_csv(lookup_file, show_col_types = FALSE) |>
  distinct(series_id, .keep_all = TRUE) |>                  # no duplicate codes
  mutate(country = str_squish(str_remove(series_name,
                    "^Workers' remittances received from ")))

print(series_lookup, n = Inf)


# ---------------------------------------------------------------
# 1.3 Download every series (cached for a week)
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

  rmt <- bind_rows(all_rows)
  write_rds(rmt, cache_file)
}

# Clean: drop missing rows and any duplicated country-date pairs
rmt <- rmt |>
  filter(!is.na(date), !is.na(remittances)) |>
  distinct(country, date, .keep_all = TRUE) |>
  arrange(country, date)

glimpse(rmt)


# ---------------------------------------------------------------
# 1.4 Inspect coverage before doing anything statistical
# ---------------------------------------------------------------
# Watch for: n larger than n_dates (duplicates), and series that
# start much later than the rest.

coverage <- rmt |>
  group_by(country) |>
  summarise(
    n       = n(),
    n_dates = n_distinct(date),
    from    = min(date),
    to      = max(date),
    .groups = "drop"
  ) |>
  arrange(from)

print(coverage, n = Inf)


# ===============================================================
# PART 2  BUILD THE TOTAL SERIES
# ===============================================================

# ---------------------------------------------------------------
# 2.1 Separate real countries from aggregates
# ---------------------------------------------------------------
# The lookup contains rollups (Total, GCC, EU, Other Countries).
# Adding those to the country series double-counts, which is what
# produced the 40% gap.

aggregate_pattern <- regex(
  "total|all countr|gcc|g\\.c\\.c|eu countr|european union|other countr",
  ignore_case = TRUE
)

aggregate_names <- series_lookup$country[
  str_detect(series_lookup$country, aggregate_pattern)
]

cat("Treating these as aggregates:\n")
print(aggregate_names)          # EYEBALL THIS before continuing

total_name <- series_lookup$country[
  str_detect(series_lookup$country, regex("total", ignore_case = TRUE))
]

countries_only <- rmt |> filter(!country %in% aggregate_names)


# ---------------------------------------------------------------
# 2.2 Two candidate totals: SBP's own, and our own sum
# ---------------------------------------------------------------

total_published <- rmt |>
  filter(country %in% total_name) |>
  group_by(date) |>
  summarise(remittances = mean(remittances), .groups = "drop") |>
  arrange(date)

total_summed <- countries_only |>
  group_by(date) |>
  summarise(remittances = sum(remittances), .groups = "drop") |>
  arrange(date)

cat("Published total:", format(range(total_published$date)), "\n")
cat("Summed total:   ", format(range(total_summed$date)), "\n")

# How well do they agree over the overlap?
total_published |>
  rename(published = remittances) |>
  inner_join(total_summed |> rename(summed = remittances), by = "date") |>
  mutate(gap_pct = 100 * (summed - published) / published) |>
  summarise(min = min(gap_pct), median = median(gap_pct), max = max(gap_pct))

# A gap within roughly +/- 2% means the aggregate filter is right.
# A large positive gap means an aggregate is still being summed in;
# a large negative gap means countries are missing from the parts.


# ---------------------------------------------------------------
# 2.3 Choose which total to adjust
# ---------------------------------------------------------------
# Longer history makes for a much better seasonal adjustment, so
# take the summed series if it reaches back further AND agrees with
# the published one over the overlap.

use_summed <- nrow(total_summed) > nrow(total_published)

total_df <- if (use_summed) total_summed else total_published
cat("Using the", if (use_summed) "summed" else "published", "total\n")

# Final checks before building the ts object
stopifnot(nrow(total_df) == n_distinct(total_df$date))

month_index <- year(total_df$date) * 12 + month(total_df$date)
if (any(diff(month_index) != 1)) {
  warning("Gaps in the monthly sequence - inspect before adjusting")
  print(total_df$date[which(diff(month_index) != 1)])
}

range(total_df$date)
nrow(total_df)


# ---------------------------------------------------------------
# 2.4 As a ts object, and a proper dated plot
# ---------------------------------------------------------------

total_ts <- ts(total_df$remittances,
               start     = c(year(min(total_df$date)), month(min(total_df$date))),
               frequency = 12)

total_ts       # print and confirm the start and end dates are right

ggplot(total_df, aes(date, remittances)) +
  geom_line(linewidth = 0.6, colour = "grey30") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Total workers' remittances to Pakistan (raw)",
       x = NULL, y = "US$ million",
       caption = "Source: State Bank of Pakistan, EasyData")


# ===============================================================
# PART 3  SEASONAL ADJUSTMENT
# ===============================================================

# ---------------------------------------------------------------
# 3.1 Eid regressors
# ---------------------------------------------------------------
# Remittances surge in the weeks BEFORE Eid, and Eid moves about
# 11 days earlier each Gregorian year. A fixed monthly seasonal
# filter cannot see that, so we hand X-13 explicit regressors.
#
# VERIFY THESE DATES. They depend on moon sighting and Pakistan is
# often a day behind Saudi Arabia. A one-day error only matters near
# a month boundary, but 31 March 2025 is exactly such a case.

eid_fitr <- as.Date(c(
  "2010-09-10", "2011-08-31", "2012-08-20", "2013-08-09",
  "2014-07-29", "2015-07-18", "2016-07-07", "2017-06-26",
  "2018-06-16", "2019-06-05", "2020-05-24", "2021-05-13",
  "2022-05-03", "2023-04-22", "2024-04-11", "2025-03-31",
  "2026-03-20", "2027-03-10", "2028-02-27"
))

eid_adha <- as.Date(c(
  "2010-11-17", "2011-11-07", "2012-10-27", "2013-10-16",
  "2014-10-06", "2015-09-24", "2016-09-13", "2017-09-02",
  "2018-08-22", "2019-08-12", "2020-08-01", "2021-07-21",
  "2022-07-10", "2023-06-29", "2024-06-17", "2025-06-07",
  "2026-05-27", "2027-05-17", "2028-05-05"
))

# start = -20, end = -1: effect runs from 20 days before Eid to the
# day before it. center = "calendar" strips the regressor's own
# seasonal mean so it does not fight the seasonal factors.
# Build the regressors over the full Eid date range first
reg_fitr <- genhol(eid_fitr, start = -20, end = -1, center = "calendar")
reg_adha <- genhol(eid_adha, start = -15, end = -1, center = "calendar")

# X-13 forecasts 3 years ahead, so the regressors must cover that too
end_needed <- c(end(total_ts)[1] + 3, end(total_ts)[2])

reg_fitr <- window(reg_fitr, start = start(total_ts), end = end_needed,
                   extend = TRUE)
reg_adha <- window(reg_adha, start = start(total_ts), end = end_needed,
                   extend = TRUE)

reg_fitr[is.na(reg_fitr)] <- 0
reg_adha[is.na(reg_adha)] <- 0

eid_xreg <- ts.union(reg_fitr, reg_adha)

# Confirm the coverage before running
cat("Data ends:      ", end(total_ts), "\n")
cat("Regressors end: ", end(eid_xreg), "\n")

# ---------------------------------------------------------------
# 3.2 Level shift, only if it falls inside the sample
# ---------------------------------------------------------------
# SBP changed the compilation basis in July 2019. Include a level
# shift only if the data actually spans that date.

sample_start <- min(total_df$date)
reg_vars <- "td"

if (sample_start < as.Date("2019-07-01")) {
  reg_vars <- c("td", "ls2019.jul")
  message("Including the July 2019 level shift")
} else {
  message("Sample starts after July 2019 - no level shift needed")
}


# ---------------------------------------------------------------
# 3.3 Run X-13ARIMA-SEATS
# ---------------------------------------------------------------

m <- seas(
  total_ts,
  transform.function   = "log",     # seasonal swing grows with the level
  xreg                 = eid_xreg,
  regression.usertype  = "holiday",
  regression.variables = reg_vars,
  outlier.types        = "all"
)

summary(m)

# If the run fails, try stripping it back and adding pieces one at a
# time. This minimal version almost always runs:
#   m <- seas(total_ts, transform.function = "log")


# ---------------------------------------------------------------
# 3.4 Diagnostics - check before trusting the output
# ---------------------------------------------------------------

# (a) Are the Eid regressors significant? Look for reg_fitr and
#     reg_adha in summary(m) with |t| above 2.

# (b) Residual seasonality. p > 0.05 means none is left, which is
#     what you want.
qs(m)

# (c) Residual autocorrelation
Box.test(resid(m), lag = 24, type = "Ljung-Box")

# (d) Outliers X-13 found on its own
outlier(m)

# (e) Pictures
plot(m)          # original vs adjusted
monthplot(m)     # seasonal factors by month, look for stability


# ---------------------------------------------------------------
# 3.5 Components into a tidy frame
# ---------------------------------------------------------------

sa_df <- tibble(
  date      = total_df$date,
  original  = as.numeric(original(m)),
  adjusted  = as.numeric(final(m)),
  trend     = as.numeric(trend(m)),
  irregular = as.numeric(irregular(m))
) |>
  mutate(seas_factor = original / adjusted)

head(sa_df)

head(sa_df)
write_csv(sa_df, "data/remittances_total_seasonally_adjusted.csv")


# ---------------------------------------------------------------
# 3.6 Plots
# ---------------------------------------------------------------

sa_df |>
  select(date, original, adjusted, trend) |>
  pivot_longer(-date, names_to = "series", values_to = "value") |>
  ggplot(aes(date, value, colour = series)) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = c(original = "grey70",
                                 adjusted = "#1f77b4",
                                 trend    = "#d62728")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Total remittances: raw, seasonally adjusted, trend",
       subtitle = "X-13ARIMA-SEATS with Eid al-Fitr and Eid al-Adha regressors",
       x = NULL, y = "US$ million", colour = NULL,
       caption = "Source: State Bank of Pakistan, EasyData. Own calculation.")

sa_df |>
  ggplot(aes(date, seas_factor)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Estimated seasonal factor",
       subtitle = "Above 1 means the raw month is seasonally high",
       x = NULL, y = "Factor")

# Month-on-month growth only means something on adjusted data
sa_df |>
  mutate(mom = 100 * (adjusted / lag(adjusted) - 1)) |>
  filter(!is.na(mom)) |>
  ggplot(aes(date, mom)) +
  geom_col(fill = "#1f77b4") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Month-on-month growth, seasonally adjusted",
       x = NULL, y = "%")

# Three-month annualised: the usual way to read momentum
sa_df |>
  mutate(ann3 = 100 * ((adjusted / lag(adjusted, 3))^4 - 1)) |>
  filter(!is.na(ann3)) |>
  ggplot(aes(date, ann3)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Three-month annualised growth, seasonally adjusted",
       x = NULL, y = "% annualised")


# ---------------------------------------------------------------
# 3.7 Did the Eid regressors earn their place?
# ---------------------------------------------------------------

m_plain <- seas(total_ts, transform.function = "log")

tibble(
  date        = total_df$date,
  with_eid    = as.numeric(final(m)),
  without_eid = as.numeric(final(m_plain))
) |>
  mutate(difference = with_eid - without_eid) |>
  ggplot(aes(date, difference)) +
  geom_col(fill = "#d62728") +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Effect of the Eid regressors on the adjusted series",
       subtitle = "Tall bars = months where ignoring Eid would mislead you",
       x = NULL, y = "US$ million")

cat("Residual seasonality with Eid regressors:\n");  print(qs(m))
cat("Residual seasonality without:\n");              print(qs(m_plain))


# ===============================================================
# PART 4  FORECASTING
# ===============================================================

# ---------------------------------------------------------------
# 4.1 Model the adjusted series
# ---------------------------------------------------------------
# Forecasting the adjusted series and re-seasonalising usually beats
# modelling the raw series directly.

sa_tsibble <- sa_df |>
  mutate(month = yearmonth(date)) |>
  select(month, adjusted) |>
  as_tsibble(index = month)

# Holdout check: fit without the last 12 months, see who wins
train <- sa_tsibble |> filter(month < max(month) - 11)

train |>
  model(ets = ETS(adjusted), arima = ARIMA(adjusted), naive = NAIVE(adjusted)) |>
  forecast(h = "12 months") |>
  accuracy(sa_tsibble) |>
  select(.model, RMSE, MAE, MAPE) |>
  arrange(RMSE)

# Full-sample fit and 12-month forecast
sa_fc <- sa_tsibble |>
  model(ets = ETS(adjusted), arima = ARIMA(adjusted)) |>
  forecast(h = "12 months")

sa_fc |>
  autoplot(sa_tsibble, level = 80) +
  labs(title = "Seasonally adjusted total remittances: 12-month forecast",
       x = NULL, y = "US$ million")


# ---------------------------------------------------------------
# 4.2 Country detail
# ---------------------------------------------------------------

top6 <- countries_only |>
  group_by(country) |>
  summarise(average = mean(remittances), .groups = "drop") |>
  arrange(desc(average)) |>
  slice(1:6) |>
  pull(country)

top6

p_trend <- countries_only |>
  filter(country %in% top6) |>
  ggplot(aes(date, remittances, colour = country)) +
  geom_line(linewidth = 0.6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Workers' remittances by source country",
       x = NULL, y = "US$ million", colour = NULL,
       caption = "Source: State Bank of Pakistan, EasyData")

p_trend
ggplotly(p_trend)

# Shares of the published total
shares <- countries_only |>
  left_join(total_published |> rename(total = remittances), by = "date") |>
  mutate(share = remittances / total) |>
  filter(is.finite(share))

big_share <- shares |>
  group_by(country) |>
  summarise(average_share = mean(share), .groups = "drop") |>
  filter(average_share > 0.05) |>
  pull(country)

shares |>
  filter(country %in% big_share) |>
  ggplot(aes(date, share, colour = country)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Share of total remittances by source country",
       x = NULL, y = "Share", colour = NULL)
