# Attach packages ==============================================================

library(readr)
library(here)
library(dplyr)
library(purrr)
library(quarto)
library(fs)
library(fable) # for forecasting

library(fpp3) # for forecasting

library(tsibble) # for time series data manipulation

library(dplyr) # for data manipulation

library(ggplot2) # for data visualization

library(xts) # for time series data manipulation

library(lubridate) # for date manipulation

library(tidyr) # for data manipulation

library(forecast) # for forecasting

# Load data ====================================================================

rmt_long <- readRDS("D:/RepTemplates/timeseries/docs/data/rmt_long.RDS")

glimpse(rmt_long)

# Create dataframe to iterate over =============================================

# HTML reports
rmt_reports_pdf <- rmt_long |>
  distinct(country) |>
  mutate(
    output_format = "pdf",
    output_file = paste(
      tolower(country),
      "report.pdf",
      sep = "-"
    ),
    execute_params = map2(
      country,
      country,  # Corrected: using the same variable as the vector
      ~ list(country = .x)  # Using tidyverse-style lambda function
    )
  )


# PDF reports
# 
# rmt_reports_pdf <- rmt_reports_html |>
#   mutate(
#     output_file = gsub("html", "pdf", output_file),
#     output_format = gsub("html", "pdf", output_format)
#   )
# 
# # Bind HTML and PDF report dataframes together
# 
# rmt_reports <- rbind(rmt_reports_html, rmt_reports_pdf)

# Subset to first 2 cat/dog breeds =============================================

rmt_reports_subset1 <- rmt_reports_pdf |>
  slice_head(n = 2, by = c(country, output_format)) |>
  select(output_file, output_format, execute_params)

# Map over each row ============================================================

library(here)
library(purrr)

# Set working directory using here()


# Render the file using pwalk
purrr::pwalk(
  rmt_reports_subset1,
  quarto_render,
  input = here::here("docs/remittances.qmd"),  # Corrected file path
  .progress = TRUE
)


# Move reports to separate folder ==============================================

output_dir <- "reports"

# List files that contain ".html" or ".pdf".
files <- dir_ls(here(), regexp = ".pdf$")

# Create directory if needed
dir_create(output_dir)

# Move the files
file_move(files, output_dir)
