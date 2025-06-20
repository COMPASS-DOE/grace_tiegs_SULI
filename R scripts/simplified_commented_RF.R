## Working to simplify the RF for teabag study/
## make easier to modify/make function in the future
##
## Grace Tiegs 6-16-2025
## 
## https://juliasilge.com/blog/ikea-prices/ 
##
# ############## #
# ############## #


#### check that I'm not actually limiting the real model to 50
#### don't limit on VIP
#### adding in "white noise" to model
#### how well the data fits
####  the collect_predictions and feature importance
### getting higher R^2
### AI ask for improvements -- model performance and steps to make more robust
### which variables, mtry,
### leave one variable out at a random
### ! eventually will put into a function so that we can change one variable at a time
### (data input, predictor variables)

## Load in packages ############################################################
require(pacman)

p_load(
       tidyverse, # for loading in data
       readxl, # for reading in an Excel
       tidymodels, # for starting to build the model
       usemodels, # for specifying the model
       vip, # exploration of feature importance
       ranger, # random forest
       cowplot # for organizing plots
       )

## Load in data ################################################################
file_path_tea <- ("grace_tiegs_SULI/data/GT_Teabag_Types.xlsx")
teabags <- read_excel(file_path_tea)

teabags_df <- teabags %>%
  filter(!is.na(k)) %>%
  select(k, category, ecosystem_type_reported, koppen_geiger_climate_class, elevation_meters, organic_carbon_density,
         clay, nitrogen, sand, silt, mean_annual_air_temp_c, mean_precip)

## Visualize
#plot_oc <- ggplot(data = teabags_df, aes(x = mean_annual_air_temp_c, y = log_k)) +
#  geom_point()
#plot_oc

## Add white noise to the data frame
teabags_df %>% mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1))

## Build a model ###############################################################

# ensures the same random numbers are produced for data splitting reproducibility
set.seed(123)

# split into different categories. strata = k ensures that k is as 
  # evenly distributed as possible since that is what we are focused on
teabags_split <- initial_split(teabags_df, strata = k)

# assign values to the training
teabags_train <- training(teabags_split)

# assign values to the testing
teabags_test <- testing(teabags_split)

# sampling seed for the bootstrapping part
set.seed(234)

# Bootstraps used to resample data. Randomly samples observations from the data
  # with replacement. Can be done hundreds or thousands of times. Again,
  # strata = k to help evenly distribute it.
  # Returns the number/samples in each category.
teabags_fold <- bootstraps(teabags_train, strata = k)

#??? part of usemodels package. could probably get rid of this now?? or maybe i do need
  # to keep it because we are changing what the variables are ?
# Generates the template code. Aren't saving it as a variable, but just copying from 
  # the console print out.
#use_ranger(k ~ ., data = teabags_train)

# Timing! # ! # ! # ! #
ptm <- proc.time()

# Create a recipe. Specify preprocessing steps to apply to the training data
ranger_recipe <- recipe(formula = k ~ clay+sand+silt+nitrogen+category, data = teabags_train)

# Specification for the ranger model. Setting values to tune() allows it to be 
  # optimized for different values. 
  # mtry: # variables randomly selected at each split
  # min_n: min # data points required to split a node further
ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees =  500) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

# Create a workflow where you can add in the recipe and specifications
ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

# sampling seed for the hyperparameter tuning
set.seed(43357)

# Hyperparameter tuning across 11 combo grid
# So trying 5 different combos for everything I think
# Resamples/performance testing is done with the bootstrapped resamples
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow, resamples = teabags_fold, grid = 5)


## Explore results
show_best(ranger_tune, metric = "rmse") # root mean squared error
show_best(ranger_tune, metric = "rsq") # R^2

# visualize statistical info
# (rmse low is good. rsq high is good.)
autoplot(ranger_tune)

# finalize parameters now, select best based on rmse
final_rf <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune))

# fit training data to test data
teabags_fit <- last_fit(final_rf, teabags_split)

# metrics computed on the test set
metrics <- collect_metrics(teabags_fit)

# predicted vs true k values based on the parameters

plot1 <- collect_predictions(teabags_fit) %>%
  ggplot(aes(k, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "blue") +
  coord_fixed() 


# save to make predictions on later
# predict on any object
predict(teabags_fit$.workflow[[1]], teabags_test[100,])
teabags_test[100, 1]


# For feature importance
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune)) %>%
  set_engine("ranger",
             importance = "permutation") 

# make another whole model
# extracting the importance - bar chart

rf_fit <- workflow() %>%
  add_recipe(ranger_recipe) %>% 
  add_model(imp_spec) %>%
  fit(teabags_train)

importance_values <- extract_fit_parsnip(rf_fit)$fit$variable.importance

importance_df <- as.data.frame(importance_values) %>%
  rownames_to_column(var = "predictor") %>%
  mutate(percentage = (importance_values / sum(importance_values)) * 100) %>%
  as_tibble()

plot2 <- ggplot(importance_df, aes(x = percentage, y = reorder(predictor, percentage))) +
  geom_bar(stat = "identity", alpha = 0.8, fill = "#DAB1DA") +
  labs(y = NULL, x = "Importance (%)")

plot_grid(plot1, plot2, ncol = 1)


# Timing! # ! # ! # ! #
proc.time() - ptm

ggsave("grace_tiegs_SULI/practice/practice data/combo_plot_11_vert.png",  width = 7, height = 6)

 # save the bar chart
#ggsave("grace_tiegs_SULI/practice/practice data/variable_importance.png",  width = 7, height = 6)
