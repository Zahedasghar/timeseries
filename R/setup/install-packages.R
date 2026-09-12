# ---------------------------------------------------------------------------
# install-packages.R
# Installs every package the EC614 course documents need. Run this once before
# the first `quarto render`. It only installs what is actually missing, so it is
# safe to re-run.
#
#   source("R/setup/install-packages.R")
# ---------------------------------------------------------------------------

cran <- c(
  # core
  "tidyverse", "readr", "readxl", "haven", "janitor", "lubridate", "stringr",
  "tidyr", "dplyr", "purrr", "data.table", "collapse", "scales", "broom",
  # time series and forecasting
  "fpp2", "fpp3", "fable", "fabletools", "feasts", "tsibble", "forecast",
  "tseries", "urca", "zoo", "xts", "slider", "dynlm", "lmtest", "sandwich",
  "ARDL", "dynamac", "plm", "prophet", "seasonal", "seasonalview",
  # tables and reporting
  "knitr", "kableExtra", "gt", "gtExtras", "DT", "modelsummary", "stargazer",
  "huxtable",
  # graphics
  "ggplot2", "ggrepel", "ggfortify", "ggthemes", "ggResidpanel", "patchwork",
  "gridExtra", "viridis", "colorspace", "MetBrewer", "hrbrthemes", "corrplot",
  "gganimate", "gifski", "transformr", "plotly", "dygraphs", "leaflet",
  # data access
  "WDI", "fredr", "OECD", "httr",
  # teaching datasets and misc
  "gapminder", "moderndive", "fontawesome", "shiny", "tidymodels", "remotes",
  "renderthis"
)

missing <- setdiff(cran, rownames(installed.packages()))

if (length(missing) == 0) {
  message("All CRAN packages already installed.")
} else {
  message("Installing ", length(missing), " package(s): ",
          paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
}

# ---------------------------------------------------------------------------
# Not on CRAN. Only needed by illustrative chunks that are set `eval: false`,
# so the site renders without them. Install only if you want those to run.
# ---------------------------------------------------------------------------
# remotes::install_github("mingjerli/IMFData")

# ---------------------------------------------------------------------------
# Notes
# ---------------------------------------------------------------------------
# * fredr needs a free API key: fredr::fredr_set_key("...") in ~/.Renviron.
#   Only used in an eval:false chunk in lectures/week-01-course-overview.qmd.
# * prophet pulls in a Stan toolchain and is the slowest install here.
#   It is used by labs/lab-06-prophet-forecasting.qmd.
# * No Python or reticulate is required. The Python in lecture 1 is shown as
#   static code, not executed.

still <- setdiff(cran, rownames(installed.packages()))
if (length(still)) {
  warning("Still missing after install: ", paste(still, collapse = ", "))
} else {
  message("Ready. Now run:  quarto render")
}
