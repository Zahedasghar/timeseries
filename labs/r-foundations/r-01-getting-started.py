# =============================================================================
# NC bike crash data: R/tidyverse script translated to Python/polars
# Install once:
#   pip install polars pandas plotnine pyarrow met-brewer colorspace
# =============================================================================

import polars as pl
from plotnine import *

# ---- Colour setup (MetBrewer + colorspace both have Python ports) -----------
from met_brewer import met_brew
from colorspace import lighten

clrs = met_brew(name="Java")
clrs_lt = lighten(clrs, 0.9)

pl.Config.set_tbl_rows(15)          # how many rows to print
pl.Config.set_fmt_float("mixed")    # rough analogue of options(digits = 3)

# NOTE: knitr::opts_chunk$set(...) has no in-code equivalent.
# In Quarto, put it in the YAML header instead:
#   execute:
#     warning: false
#   fig-dpi: 300

# ---- Read: read_delim(delim = ";", na = c("NA", "", ".", "#NULL!")) ---------
bike = pl.read_csv(
    "data/nc_bike_crash.csv",
    separator=";",
    null_values=["NA", "", ".", "#NULL!"],
    infer_schema_length=10_000,
    ignore_errors=True,
)

# ---- janitor::clean_names() -------------------------------------------------
import re

def clean_names(df: pl.DataFrame) -> pl.DataFrame:
    new = {
        c: re.sub(r"_+", "_",
                  re.sub(r"[^\w]+", "_",
                         re.sub(r"(?<=[a-z0-9])(?=[A-Z])", "_", c))
                 ).strip("_").lower()
        for c in df.columns
    }
    return df.rename(new)

bike = clean_names(bike)

# ---- rename() ---------------------------------------------------------------
bike = bike.rename({
    "ambulance_r": "ambulance_req",
    "crash_loc": "crash_location",
    "bike_age_grp": "bike_age_group",
    "drvr_age_grp": "drvr_age_group",
    "objectid": "object_id",
})

# ---- mutate(bike_age = parse_number(bike_age)) -------------------------------
# Pull the first number out of the string; "Unknown" and friends become null.
bike = bike.with_columns(
    pl.col("bike_age").cast(pl.Utf8)
      .str.extract(r"(\d+\.?\d*)", 1)
      .cast(pl.Float64)
      .alias("bike_age")
)

# ---- names(bike) and str(bike) ----------------------------------------------
print(bike.columns)     # names()
print(bike.schema)      # column types
print(bike.head())      # a look at the values
print(bike.describe())  # summary() for every column at once

# ---- A pandas copy purely for plotting --------------------------------------
bike_pd = bike.to_pandas()

# =============================================================================
# PLOTS  (plotnine = ggplot2 grammar; aesthetics are quoted strings)
# =============================================================================

p1 = (ggplot(bike_pd, aes(x="crash_hour", y="bike_age"))
      + geom_point())
print(p1)

p2 = (ggplot(bike_pd, aes(x="crash_hour", y="bike_age"))
      + geom_point(alpha=0.5, color="blue"))
print(p2)

p3 = (ggplot(bike_pd, aes(x="crash_hour", y="bike_age", color="ambulance_req"))
      + geom_point(alpha=0.5)
      + facet_grid(cols="bike_sex"))      # older plotnine: facet_grid(". ~ bike_sex")
print(p3)

p4 = (ggplot(bike_pd, aes(x="bike_age"))
      + geom_histogram(binwidth=5))
print(p4)

p5 = (ggplot(bike_pd, aes(y="bike_age", x="bike_sex"))
      + geom_boxplot())
print(p5)

p6 = (ggplot(bike_pd, aes(x="bike_injury"))
      + geom_bar())
print(p6)

p7 = (ggplot(bike_pd, aes(x="crash_location", fill="bike_injury"))
      + geom_bar())
print(p7)

p8 = (ggplot(bike_pd, aes(x="crash_location", fill="bike_injury"))
      + geom_bar(position="fill"))
print(p8)

# =============================================================================
# DATA VERBS
# =============================================================================

# ---- filter(county == "Durham") ---------------------------------------------
bike.filter(pl.col("county") == "Durham")

# ---- filter(county == "Durham", bike_age < 10) ------------------------------
# Comma-separated conditions work exactly as in dplyr (they are AND-ed).
bike.filter(pl.col("county") == "Durham", pl.col("bike_age") < 10)

# ---- group_by(bike_age_group) |> summarise(crash_count = n()) ---------------
(bike
 .group_by("bike_age_group")
 .agg(pl.len().alias("crash_count")))

# ---- str_replace() on the mangled Excel dates -------------------------------
bike = bike.with_columns(
    pl.col("bike_age_group")
      .str.replace("10-Jun", "6-10", literal=True)
      .str.replace("15-Nov", "11-15", literal=True)
)

(bike
 .group_by("bike_age_group")
 .agg(pl.len().alias("count")))

# ---- slice(1:5) and the last five rows --------------------------------------
bike.head(5)          # or bike.slice(0, 5)
bike.tail(5)          # no nrow() arithmetic needed

# ---- select(crash_location, hit_run) |> table() -----------------------------
# Polars has no table(); build it with group_by + pivot.
(bike
 .group_by(["crash_location", "hit_run"])
 .agg(pl.len().alias("n"))
 .pivot(on="hit_run", index="crash_location", values="n")
 .fill_null(0)
 .sort("crash_location"))

# ---- select(-object_id) -----------------------------------------------------
bike.drop("object_id")

# ---- rename(speed_limit = Speed_Limi) — already named speed_limit -----------
# bike = bike.rename({"speed_limi": "speed_limit"})
print(bike.columns)

# ---- arrange(crash_count) / arrange(desc(crash_count)) ----------------------
(bike
 .group_by("bike_age_group")
 .agg(pl.len().alias("crash_count"))
 .sort("crash_count"))

(bike
 .group_by("bike_age_group")
 .agg(pl.len().alias("crash_count"))
 .sort("crash_count", descending=True))

# ---- sample_n(5) and sample_frac(0.2) ---------------------------------------
bike_n5 = bike.sample(n=5, with_replacement=False, shuffle=True, seed=1234)
print(bike_n5.shape)          # dim()

bike_perc20 = bike.sample(fraction=0.2, with_replacement=False, seed=1234)
print(bike_perc20.shape)

# ---- bike[1, 5]: row 1, column 5 in R = row 0, column 4 in Python ------------
bike.item(0, 4)
