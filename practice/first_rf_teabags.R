## Script for Basic Random Forest
## Teabag Study in Wetlands and Forests
##
## Grace Tiegs 6-10-2025
## 
##
## Variables: climate, temperature, elevation (30m), clay, sand, silt, .....
## Most focused on climate, elevation, organic matter quality
##
##
# ############## #
# ############## #

## Load in packages ############################################################

# make it easier to load in with pacman
require(pacman)

p_load(
       tidyverse, # for loading in data
       readxl, # for reading in an Excel
       tidymodels, # for starting to build the model
       usemodels, # for specifying the model
       vip, # exploration of feature importance
       ranger
       )

## Load in data ################################################################

# Note - if we want more variables, it is easy to add to this excel
# using teabag_sample_locations.R
file_path_tea <- ("grace_tiegs_SULI/data/GT_Teabag_Types.xlsx")
teabags <- read_excel(file_path_tea)

teabags_df <- teabags %>%
  filter(!is.na(k)) %>%
  select(k, category, koppen_geiger_climate_class, elevation_meters, organic_carbon_density,
         clay, nitrogen, sand, silt)

## Exploratory analysis ########################################################

# Here is a good outline of a plot to see what type of data we're looking at
teabags_df %>%
  ggplot(aes(clay, k, color = category)) +
  xlim(0, 400) +
  geom_point(alpha = 0.4)

## Build a model ###############################################################
# set up the experiment design/how we are splitting up the data
set.seed(123)

# split into different categories
teabags_split <- initial_split(teabags_df, strata = k)

# assign values to the training
teabags_train <- training(teabags_split)

# assign values to the testing (only use at end to get idea of performance)
teabags_test <- testing(teabags_split)

# sampling seed
set.seed(234)

# tune/resample data set
teabags_fold <- bootstraps(teabags_train, strata = k)

# generates "boiler plate" code to get started on ranger rf
# recipe, model specifications, workflow, tune grid

# generate recipe+spec+flow code outline to use
use_ranger(k ~ ., data = teabags_train)

# information to run model
{
ranger_recipe <- 
  recipe(formula = k ~ ., data = teabags_train) #%>%
    
 # can impute if needed
 # step_impute_knn(elevation_meters, organic_carbon_density)

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 200) %>% # num trys, trees
  # new try at generating
  # rand_forest(trees = 100) %>%
  set_mode("regression") %>% ## can set other modes too
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(43357)
ranger_tune <-
  tune_grid(ranger_workflow, resamples = teabags_fold, grid = 11)
}


## Explore results #############################################################
show_best(ranger_tune, metric = "rmse") # root mean squared error
show_best(ranger_tune, metric = "rsq") # R^2

# visualize statistical info
# (minimal node size = min_n, # ran = mtry)
# (rmse low is good. rsq high is good.)
autoplot(ranger_tune)

# finalize parameters now, select best based on rmse
final_rf <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune))

# fit training data to test data
teabags_fit <- last_fit(final_rf, teabags_split)

# metrics computed on the test set
collect_metrics(teabags_fit)

# predicted vs true k values based on the parameters
collect_predictions(teabags_fit) %>%
  ggplot(aes(k, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "blue") +
  coord_fixed()

# save to make predictions on later
# predict on any object
predict(teabags_fit$.workflow[[1]], teabags_test[150,])


# For feature importance
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune)) %>%
  set_engine("ranger",
             importance = "permutation") 

# make another whole model
# extracting the importance - bar chart
workflow() %>%
  add_recipe(ranger_recipe) %>% 
  add_model(imp_spec) %>%
  fit(teabags_train) %>%
  extract_fit_parsnip() %>%
  vip(num_features = 3, aesthetics = list(alpha = 0.8, fill = "#DAB1DA"))

# save the bar chart
ggsave("grace_tiegs_SULI/data/variable_importance.png",  width = 7, height = 6)

# Trying partial dependence stuff
#partialPlot(final_rf, k, elevation_meters, 1, x.lab="elevation", ylab="k")
