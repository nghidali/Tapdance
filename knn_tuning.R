# KNN tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

# load required objects ----
set.seed(3948)

# Define model ----
nn_model <- nearest_neighbor(mode = "classification",
                             neighbors = tune()) %>% 
  set_engine("kknn")

# set-up tuning grid ----
nn_params <- parameters(nn_model) %>% 
  update(
    neighbors = neighbors(range = c(1, 15))
  )

# define tuning grid
nn_grid <- grid_regular(nn_params, levels = 5)

nn_grid

# workflow ----
nn_workflow <- workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(tap_recipe)

# Tuning/fitting ----
tic("KNN")

nn_tuned <- nn_workflow %>% 
  tune_grid(tap_folds, grid = nn_grid)

toc(log = TRUE)

write_rds(nn_tuned, "nn_results.rds")

# save runtime info
nearest_neighbors_run_time <- tic.log(format = TRUE)

# Write out results & workflow

