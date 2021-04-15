
# Project EDA -------------------------------------------------------------

# loading packages
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

# reading in data
tap_dat <- read_csv("data/tap_dat.csv") %>% 
  clean_names()

# skim full dataset
skim(tap_dat) %>% view()

# skim outcome variable
skim(tap_dat$sector_type)

