install.packages("ranger")
library(ranger)
library(readr)
library(dplyr)
library(tidymodels)
library(rpart)           
library(rpart.plot) 

# Problem 3 -------
# Specify the random forest
rf_mod <- 
  rand_forest(
    mode = "classification",
    engine = "ranger",
    mtry = tune(),
    trees = 100,
    min_n = tune() 
  )

# Set up the workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(spam_recipe)

# Make a search grid for the k-parameter
rf_grid <- 
  grid_latin_hypercube(
    mtry(range = c(1, length(names)/2)),
    min_n(),
    size = 10
  )

# Calculate the cross-validated AUC for all the k's in the grid
rf_tune_result <- 
  tune_grid(
    rf_workflow,
    resamples = spam_folds,
    grid = rf_grid,
    control = control_grid(save_pred = TRUE)
  )

# Put the best parameters in the workflow
rf_tuned <- 
  finalize_workflow(
    rf_workflow,
    parameters = rf_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_rf <- 
  rf_tuned %>% 
  fit(data = spam_train)

# Predict the train and test data
predictions_rf_test <- 
  fitted_rf %>% 
  predict(new_data = spam_test,
          type = "prob") %>% 
  mutate(truth = spam_test$spam) 

predictions_rf_train <- 
  fitted_rf %>% 
  predict(new_data = spam_train,
          type = "prob") %>% 
  mutate(truth = spam_train$spam) 

# Calculate the AUC
auc_rf <-
  predictions_rf_test %>% 
  roc_auc(truth, .pred_0) %>% 
  mutate(where = "test") %>% 
  bind_rows(predictions_rf_train %>% 
              roc_auc(truth, .pred_0) %>% 
              mutate(where = "train")) %>% 
  mutate(model = "random_forest")

# Problem 4--------
install.packages("xgboost")
library(xgboost)

# Specify the decistion tree
xgb_mod <- 
  boost_tree(
    trees = 100,
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

# Set up the workflow
xgb_workflow <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_recipe(spam_recipe)

# Make a search grid for the k-parameter
xgb_grid <- 
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), spam_train),
    learn_rate(),
    size = 30
  )

# Calculate the cross-validated AUC for all the k's in the grid
xgb_tune_result <- 
  tune_grid(
    xgb_workflow,
    resamples = spam_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )

# Put the best parameters in the workflow
xgb_tuned <- 
  finalize_workflow(
    xgb_workflow,
    parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_xgb <- 
  xgb_tuned %>% 
  fit(data = spam_train)

# Plot the model
install.packages("DiagrammeR")
library(DiagrammeR)
tree_plot <- xgb.plot.tree(model = fitted_xgb$fit$fit$fit, trees = 1, plot_width = 1000, 
                           plot_height = 1000, render = FALSE)

# Predict the train and test data
predictions_xgb_test <- 
  fitted_xgb %>% 
  predict(new_data = spam_test,
          type = "prob") %>% 
  mutate(truth = spam_test$spam) 

predictions_xgb_train <- 
  fitted_xgb %>% 
  predict(new_data = spam_train,
          type = "prob") %>% 
  mutate(truth = spam_train$spam) 

# Calculate the AUC
auc_xgb <-
  predictions_xgb_test %>% 
  roc_auc(truth, .pred_0) %>% 
  mutate(where = "test") %>% 
  bind_rows(predictions_xgb_train %>% 
              roc_auc(truth, .pred_0) %>% 
              mutate(where = "train")) %>% 
  mutate(model = "xgboost")
