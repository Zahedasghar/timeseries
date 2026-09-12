
# Always-a project --------------------------------------------------------




# Data types --------------------------------------------------------------

#' csv, xlsx, .dta,.sav, .sas,...


# Use-copilot -------------------------------------------------------------

#' Use copilot to write code
# Data-structure ----------------------------------------------------------

#' data.frame, tibble, matrix, array, list, vector, factor, date, time, datetime, logical, integer, double, character, complex, raw, NULL, NA, NaN, Inf, -Inf, ...
#' Reading Data
#' Using Required package 
#' Loading Data
#' Inspecting Data
#' Selecting Data
#' Slicing Data
#' Filtering Data
#' Sorting Data
#' Mutating Data
#' Grouping Data
#' Summarizing Data
#' Joining Data
#' Exporting Data
#' Data Wrangling
#' Data Visualization
#' Data Analysis
#' Data Modeling
#' Data Reporting
#' Data Presentation
#' Data Communication
#' Data Science
#' Data Engineering
#' Data Mining
#' Data Cleaning
#' Data Transformation
#' Data Manipulation
#' Data Exploration
#' Data Preprocessing
#' 
#' head
#' tail
#' glimpse
#' str
#' summary
#' View
#' names
#' colnames
#' rownames
#' dim
#' nrow
#' ncol
#' length
#' class
#' typeof
#' is.na
#' 
#' dplyr verbs
#' arrange
#' filter
#' group_by
#' mutate
#' select
#' slice
#' summarise
#' 
#' 
#' 
#' 10 most powerful tools in Rstudio
#' 
#' 



library(gtExtras)







# Load-packages -----------------------------------------------------------

library(tidyverse)

# Load-data ---------------------------------------------------------------

piedf <- read_csv("data/education.csv")

saveRDS(piedf, "data/piedf.rds")
piedf |> View()

education <- readRDS("data/education.rds")

# Inspect-data ------------------------------------------------------------

piedf |> dim()  # To see number of rows and columns


piedf |> colnames() #colnames, 'names' command can also be used for the purpose


piedf |> glimpse()    # To see nature of variables 


# drop_columns_having_no_data ---------------------------------------------

piedf |>
  select(-where(~all(is.na(.)))) |> dim()


piedf |>
  select(-where(~all(is.na(.)))) |> 
  dim() 

piedf |> distinct(dataset)

piedf |> View()


# only-pslm-data ----------------------------------------------------------

piedf |> filter(dataset=='pslm') |> distinct(year)



# Select_data ---------------------------------------------------------------

piedf |> filter(dataset=='pslm' & year==2014) |> 
  select(district, contains('in_school_wq'))

piedf |> filter(dataset=='aser') |> distinct(year)


piedf |> filter(dataset=='aser' & year==2019) |> select(district, contains('in_school_wq'))


piedf |> filter(dataset=='mics') |> distinct(year)


# Slice,columns -----------------------------------------------------------

piedf |> slice(1:10) |> select(contains('in_school_wq'))

piedf |> select(1:99) |> 
  pivot_longer(cols=!(1:9),
               names_to = c('in','school','wq',"lage",'uage','se'),
               names_sep = '_',
               values_to = 'count') |> View()







piedf |> select(contains('in_school')) |> 
  pivot_longer(cols=!(1:9),
               names_to = c('in','school','wq',"age"),
               names_sep = '_',
               values_to = 'count') |> View()



