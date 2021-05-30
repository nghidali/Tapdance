
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
tap_dat <- read_csv("data/tap_dat.csv") %>% 
  clean_names() %>%
  filter(academic_year >= 2014) %>%
  filter(tap_level_of_study != "STAP") %>% # only one observation of STAP so remove it
  mutate(
    level = factor(level),
    tap_level_of_study = factor(tap_level_of_study),
    sector_type = factor(sector_type),
    tap_sector_group = factor(tap_sector_group),
    recipient_age_group = ordered(recipient_age_group, 
                                  levels = c("under age 22",  "age 22 - 25", "age 26 - 35",  "age 36 - 50",  "over age 50"),
                                  labels = c("under_age_22",  "age_22_25", "age_26_35",  "age_36_50",  "over_age_50")),
    tap_financial_status = factor(tap_financial_status),
    tap_award_schedule = factor(tap_award_schedule),
    tap_degree_or_non_degree = factor(tap_degree_or_non_degree),
    tap_schedule_letter = factor(tap_schedule_letter),
    income = as.numeric(sub(",","",str_extract(income_by_1_000_range, "\\d+,*\\d*")))
  ) %>%
  select(-income_by_1_000_range, -income_by_5_000_range, -income_by_10_000_range) %>%
  select(-tap_sector_group)  # sector group is too similar to sector type

# splitting data
tap_split <- initial_split(tap_dat, prop = .80, strata = sector_type)
tap_train <- training(tap_split)
tap_test <- testing(tap_split)

tap_split
dim(tap_test)
dim(tap_train)

# creating folds
tap_folds <- vfold_cv(tap_train, v = 5, repeats = 3, strata = sector_type)

# tap_schedule_letter has some very uncommon factor levels, we will need to bin them in our recipe
count(tap_dat,tap_schedule_letter)

# creating the recipe
tap_recipe <- recipe(sector_type ~ ., data = tap_train) %>% 
  # remove level since it is duplicating information already contained in TAP level of study
  step_rm(level,tap_recipient_ft_es) %>%
  # Collapse uncommon factor levels, experimentally, a higher threshold works better, but this is extra necessary for tap_schedule_letter, where levels F and G have only 2 observations each
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_interact(terms = ~ income:starts_with("tap_financial_status")) %>%
  step_normalize(all_numeric_predictors()) 

# saving
save(tap_recipe, tap_folds, file = "tap_model_inputs.rda")
