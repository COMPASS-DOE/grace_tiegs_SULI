## v fold cross validation code that I tried in feature_importance_RF.R but
# it didn't work

# v fold cross validation
set.seed(123)
cv_fold <- vfold_cv(data_train, v = 5, strata = dep)


rf_workflow <- workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(rf_model)

rf_results <- rf_workflow %>%
  fit_resamples(
    resamples = cv_fold,
    metrics = metric_set(yardstick::rmse, rsq),
    control = control_resamples(save_pred = TRUE)
  )
collect_metrics(rf_results)