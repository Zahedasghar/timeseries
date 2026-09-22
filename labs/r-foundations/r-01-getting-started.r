library(tidyverse)
library(httr)
clrs <- MetBrewer::met.brewer(name = "Java")
clrs_lt <- colorspace::lighten(clrs, 0.9)
knitr::opts_chunk$set(fig.retina = 3, collapse = TRUE)
options(digits = 3, width = 75)

# install.packages("ggplot2")
# install.packages("dplyr")

library(ggplot2)
library(dplyr)
#library(tidyverse) this package contains a multiple packages 

library(readr)

# Local copy of the NC bike crash data (semicolon-delimited).
# Source mirror: https://raw.githubusercontent.com/rstudio-education/dsbox/main/data-raw/ncbikecrash/ncbikecrash.csv
bike <- read_delim("data/nc_bike_crash.csv",
                   delim = ";", na = c("NA", "", ".", "#NULL!"))

# Standardize CamelCase column names to snake_case to match the code below
bike <- bike |>
  janitor::clean_names() |>
  rename(ambulance_req = ambulance_r, crash_location = crash_loc,
         bike_age_group = bike_age_grp, drvr_age_group = drvr_age_grp,
         object_id = objectid) |>
  # Extract numeric age; "Unknown" etc. become NA
  mutate(bike_age = parse_number(bike_age))

names(bike)

str(bike)

ggplot(data = bike, aes(x = crash_hour, y = bike_age)) +
  geom_point()

ggplot(data = bike, aes(x = crash_hour, y = bike_age)) +
  geom_point()

ggplot(data = bike, aes(x = crash_hour, y = bike_age)) +
  geom_point(alpha = 0.5, color = "blue")

ggplot(data = bike, aes(x = crash_hour, y = bike_age)) +
  geom_point(alpha = 0.5, color = "blue")

ggplot(data = bike, aes(x = crash_hour, y = bike_age, color = ambulance_req)) +
  geom_point(alpha = 0.5) +
  facet_grid(. ~ bike_sex)

ggplot(data = bike, aes(x = crash_hour, y = bike_age, color = ambulance_req)) +
  geom_point(alpha = 0.5) +
  facet_grid(. ~ bike_sex)

ggplot(data = bike, aes(x = bike_age)) +
  geom_histogram(binwidth = 5)

ggplot(data = bike, aes(y = bike_age, x = bike_sex)) +
  geom_boxplot()

ggplot(data = bike, aes(x = bike_injury)) +
  geom_bar()

ggplot(data = bike, aes(x = crash_location, fill = bike_injury)) +
  geom_bar()

ggplot(data = bike, aes(x = crash_location, fill = bike_injury)) +
  geom_bar(position="fill")

bike %>%
  filter(county == "Durham")

bike %>%
  filter(county == "Durham", bike_age < 10)

bike %>%
  group_by(bike_age_group) %>%
  summarise(crash_count = n())

# #install.packages("stringr")

library(stringr)

bike <- bike %>%
  mutate(bike_age_group = str_replace(bike_age_group, "10-Jun", "6-10")) %>%
  mutate(bike_age_group = str_replace(bike_age_group, "15-Nov", "11-15"))

bike %>%
  group_by(bike_age_group) %>%
  summarise(count = n())

bike %>%
  slice(1:5)

last_row <- nrow(bike)
bike %>%
  slice((last_row-4):last_row)

bike %>%
  select(crash_location, hit_run) %>%
  table()

bike %>%
  select(-object_id)

names(bike)

#bike <- bike %>%
#  rename(speed_limit = Speed_Limi)## Its already in speed_limit

names(bike)

bike %>%
  group_by(bike_age_group) %>%
  summarise(crash_count = n()) %>%
  arrange(crash_count)

bike %>%
  group_by(bike_age_group) %>%
  summarise(crash_count = n()) %>%
  arrange(desc(crash_count))

bike_n5 <- bike %>%
  sample_n(5, replace = FALSE)
dim(bike_n5)

bike_perc20 <-bike %>%
  sample_frac(0.2, replace = FALSE)
dim(bike_perc20)

bike[1,5] # row 1, column 5
