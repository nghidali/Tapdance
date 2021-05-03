
# Model-Building ----------------------------------------------------------

# loading packages
library(tidyverse)
library(tidymodels)
library(janitor)

# set seed
set.seed(3948)

# loading data
tap_dat <- read.csv("data/tap_dat.csv")

# cleaning data
tap_dat <- tap_dat %>% 
  clean_names()

# splitting data
tap_split <- initial_split(tap_dat, prop = .80, strata = sector_type)
tap_train <- training(tap_split)
tap_test <- testing(tap_split)

tap_split
dim(tap_test)
dim(tap_train)

# creating folds
tap_folds <- vfold_cv(tap_train, v = 5, repeats = 3)

# creating the recipe
tap_recipe <- recipe(sector_type ~ ., data = tap_dat) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors())
