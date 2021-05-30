library(tidyverse)
library(tidymodels)
library(xgboost)
set.seed(42)

load("tap_model_inputs.rda")

# Boosted tree

rf_model <- rand_forest(
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity")

rf_workflow <- workflow() %>%
  add_recipe(tap_recipe) %>%
  add_model(rf_model)

rf_params <- parameters(rf_workflow) %>%
  update(mtry = mtry(range = c(1, 15)))

rf_grid <- grid_regular(rf_params, levels = 9)

control <- control_resamples(verbose = TRUE)

rf_tuned <- rf_workflow %>%
  tune_grid(tap_folds, rf_grid, control)

saveRDS(rf_tuned, "rf_tuned.rds")

rf_tuned <- readRDS("rf_tuned.rds")

# Pick optimal tuning params
show_best(rf_tuned, metric = "accuracy")
rf_results <- rf_workflow %>%
  finalize_workflow(select_best(rf_tuned, metric = "accuracy")) %>%
  fit(tap_train)

# Predict test set
rf_predictions <- predict(rf_results, new_data = tap_test) %>%
  bind_cols(tap_test %>% select(sector_type)) %>%
  rename(predicted = .pred_class) %>%
  mutate(predicted = factor(predicted),
         sector_type = factor(sector_type))

accuracy(rf_predictions,sector_type, predicted)
# 0.674