
# Project EDA -------------------------------------------------------------

# loading packages
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

# reading in data
tap_data <- read_csv("data/tap_dat.csv") %>% 
  clean_names() %>%
  mutate(
    level = factor(level),
    tap_level_of_study = factor(tap_level_of_study),
    sector_type = factor(sector_type),
    tap_sector_group = factor(tap_sector_group),
    recipient_age_group = ordered(recipient_age_group, 
                                  levels = c("under age 22",  "age 22 - 25", "age 26 - 35",  "age 36 - 50",  "over age 50")),
    tap_financial_status = factor(tap_financial_status),
    tap_award_schedule = factor(tap_award_schedule),
    tap_degree_or_non_degree = factor(tap_degree_or_non_degree),
    tap_schedule_letter = factor(tap_schedule_letter),
    income = as.numeric(sub(",","",str_extract(income_by_1_000_range, "\\d+,*\\d*")))
  ) %>%
  select(-income_by_1_000_range, -income_by_5_000_range, -income_by_10_000_range)

# skim full dataset
skim(tap_data) %>% view()

# skim outcome variable
skim(tap_data$sector_type)

# view observation distributions
ggplot(tap_data, aes(level)) +
  geom_bar()

ggplot(tap_data, aes(tap_level_of_study)) +
  geom_bar()

ggplot(tap_data, aes(sector_type)) +
  geom_bar()

ggplot(tap_data, aes(tap_sector_group)) +
  geom_bar()

ggplot(tap_data, aes(recipient_age_group)) +
  geom_bar()

ggplot(tap_data, aes(tap_financial_status)) +
  geom_bar()

ggplot(tap_data, aes(tap_award_schedule)) +
  geom_bar()

ggplot(tap_data, aes(tap_degree_or_non_degree)) +
  geom_bar()
