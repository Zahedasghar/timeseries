##------------------------------------------------------------------------------
#  Loading R Packages
##------------------------------------------------------------------------------
library(doParallel)
library(foreach)
Clusters <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(Clusters)
options(Ncpus = detectCores() - 1, mc.cores = detectCores() - 1, scipen = 999)
detectCores()

Packages <- 
    c(
        "fastverse"
      , "furrr"
      , "janitor"
      , "pdftools"
      , "readxl"
      , "splitstackshape"
      , "stringi"
      , "stringr"
      , "tidytable"
      , "tidyverse"
      , "writexl",
      "haven"
    )

Packages[!(Packages %in% installed.packages()[ ,"Package"])]

if (!require("pacman")) install.packages("pacman")
pacman::p_load(Packages, character.only = TRUE)
# pacman::p_up(update = TRUE, ask = FALSE)

##------------------------------------------------------------------------------
#                             The End
##------------------------------------------------------------------------------
