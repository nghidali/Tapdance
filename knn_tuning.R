library(tidyverse)
library(tidymodels)
library(xgboost)
set.seed(42)

load("tap_model_inputs.rda")

# Define model ----
knn_model <- nearest_neighbor(mode = "classification",
                             neighbors = tune()) %>% 
  set_engine("kknn")

knn_workflow <- workflow() %>%
  add_recipe(tap_recipe) %>%
  add_model(knn_model)

# set-up tuning grid ----
knn_params <- parameters(knn_workflow) %>% 
  update(
    neighbors = neighbors(range = c(1, 15))
  )

knn_grid <- grid_regular(knn_params, levels = 9)

control <- control_resamples(verbose = TRUE)

knn_tuned <- knn_workflow %>%
  tune_grid(tap_folds, knn_grid, control)

saveRDS(knn_tuned, "knn_tuned.rds")

knn_tuned <- readRDS("knn_tuned.rds")

# Pick optimal tuning params
show_best(knn_tuned, metric = "accuracy")
knn_results <- knn_workflow %>%
  finalize_workflow(select_best(knn_tuned, metric = "accuracy")) %>%
  fit(tap_train)

# Predict test set
knn_predictions <- predict(knn_results, new_data = tap_test) %>%
  bind_cols(tap_test %>% select(sector_type)) %>%
  rename(predicted = .pred_class) %>%
  mutate(predicted = factor(predicted),
         sector_type = factor(sector_type))

accuracy(knn_predictions,sector_type, predicted)
# 0.509
