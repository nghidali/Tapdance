library(tidyverse)
library(tidymodels)
library(xgboost)
set.seed(42)

load("tap_model_inputs.rda")

# Boosted tree

bt_model <- boost_tree(
  mode = "classification",
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost", importance = "impurity")

bt_workflow <- workflow() %>%
  add_recipe(tap_recipe) %>%
  add_model(bt_model)

bt_params <- parameters(bt_workflow) %>%
  update(mtry = mtry(range = c(1, 15)))

bt_grid <- grid_regular(bt_params, levels = 9)

control <- control_resamples(verbose = TRUE)

bt_tuned <- bt_workflow %>%
  tune_grid(tap_folds, bt_grid, control)

saveRDS(bt_tuned, "bt_tuned.rds")

bt_tuned <- readRDS("bt_tuned.rds")

# Pick optimal tuning params
show_best(bt_tuned, metric = "accuracy")
bt_results <- bt_workflow %>%
  finalize_workflow(select_best(bt_tuned, metric = "accuracy")) %>%
  fit(tap_train)

# Predict test set
bt_predictions <- predict(bt_results, new_data = tap_test) %>%
  bind_cols(tap_test %>% select(sector_type)) %>%
  rename(predicted = .pred_class) %>%
  mutate(predicted = factor(predicted),
         sector_type = factor(sector_type))

accuracy(bt_predictions,sector_type, predicted)
# 0.636
# 0.626
# 0.621

xgb.importance(model=pull_workflow_fit(bt_results)$fit)
