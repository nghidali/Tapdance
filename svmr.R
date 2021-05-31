library(tidyverse)
library(tidymodels)

load("tap_model_inputs.rda")

### Support vector machine (radial basis function)

svm_rbf_model <- svm_rbf(mode = "classification",
                         cost = tune(),
                         rbf_sigma = tune()) %>%
  set_engine("kernlab")

svm_rbf_workflow <- workflow() %>%
  add_recipe(tap_recipe) %>%
  add_model(svm_rbf_model)

svm_rbf_grid <-
  grid_regular(parameters(svm_rbf_workflow), levels = 3)

control <- control_resamples(verbose = TRUE)

svm_rbf_tuned <- svm_rbf_workflow %>%
  tune_grid(tap_folds, svm_rbf_grid, control)

saveRDS(svm_rbf_tuned, "svm_rbf_tuned.rds")

svm_rbf_tuned <- readRDS("svm_rbf_tuned.rds")

# Pick optimal tuning params
show_best(svm_rbf_tuned, metric = "accuracy")
svm_rbf_results <- svm_rbf_workflow %>%
  finalize_workflow(select_best(svm_rbf_tuned, metric = "accuracy")) %>%
  fit(tap_train)

saveRDS(svm_rbf_results, "svm_rbf_results.rds")

# Predict test set
svm_rbf_predictions <- predict(svm_rbf_results, new_data = tap_test) %>%
  bind_cols(tap_test %>% select(sector_type)) %>%
  rename(predicted = .pred_class) %>%
  mutate(predicted = factor(predicted),
         sector_type = factor(sector_type))

accuracy(svm_rbf_predictions,sector_type, predicted)
# accuracy = 0.636
