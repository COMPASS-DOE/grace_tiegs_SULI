## Making it into a function so I can test a lot of different parameter combos
##
## Grace Tiegs 6-19-2025
##
##
## Updated 6-23-2025 
##
# ############## #
# ############## #

## 1. Load in packages #########################################################
require(pacman)

p_load(
       tidyverse, # for loading in data
       readxl, # for reading in an Excel
       tidymodels, # for starting to build the model
       usemodels, # for specifying the model
       vip, # exploration of feature importance
       ranger, # random forest
       cowplot, # for organizing plots
       tictoc,
       hydroGOF,
       readr
       )


## 2. Load in data #############################################################

file_path_tea <- ("data/GT_Teabag_Types.xlsx")

teabags <- read_excel(file_path_tea) %>%
  filter(!is.na(k)) %>% 
  filter(category == "Wetland" | category == "Forest") %>%
    select(k, category, ecosystem_type_reported, koppen_geiger_climate_class, 
         elevation_meters, organic_carbon_density,
         clay, nitrogen, sand, silt, mean_annual_air_temp_c, mean_precip)

# Set the dependent variable for the models ??????????????
## Not needed here if we only have one dependent. Useful if you're making models
## for multiple different dependents
#var = "k"


model_data <- as_tibble(teabags) %>% 
  dplyr::select(c({{var}}, 
                  all_of(all_predictors))) %>%
  mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1)) %>% 
  mutate(dep = eval(parse(text = var))) %>% 
  dplyr::select(-c({{var}}, k)) %>%
  drop_na()

# Split up training and set
set.seed(123)
data_split <- initial_split(model_data, prop = 0.7)
data_train <- training(data_split)
data_test <- testing(data_split)
n_test = nrow(data_test)
#data_fold <- bootstraps(data_train, strata = dep)

# Set up recipe
ranger_recipe <- data_train %>%
  recipe(dep ~ .) %>%
  #as.formula(paste(dep, "~ ."))) %>%
  step_corr(all_predictors(), -all_outcomes()) %>%
  #step_corr(all_numeric_predictors(), -all_outcomes()) %>%
  prep()

# Make the actual model
set.seed(234)

rf_model <-
  rand_forest(mtry = m_try, trees =  ntree, mode = "regression") %>%
  set_engine("ranger") %>%
  fit(dep ~ ., data = data_train)

# Calculate predicted variables
model_fit <- data_test %>%
  mutate(predict(rf_model, data_test))

ggplot(model_fit, aes(dep, .pred)) + geom_point()

# Calculate metrics for model performance
rmse = hydroGOF::rmse(model_fit$.pred, model_fit$dep)
r2 = hydroGOF:: gof(model_fit$.pred, model_fit$dep)["R2", ]

# Shows which model is run to keep track of progress
print(paste(model, paste(predictors, collapse = ","), proportion, m_try, ntree))

# # All the information exported
output = tibble(dep = {{var}},
                predictors = paste(predictors, collapse = ","),
                proportion = proportion,
                m_try = m_try,
                ntree = ntree,
                rmse = rmse,
                r2 = r2,
                n_test = n_test)

ggplot(model_fit, aes(dep, .pred)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

output






## 3. Set up function and test -------------------------------------------------

source("R scripts/constants_RF.R")

var = "k" 

calculate_metrics <- function(proportion = proportion,
                              model = model,
                              m_try = m_try,
                              ntree = ntree,
                              model_no = model_no){
  
  model_data <- as_tibble(teabags) %>% 
    dplyr::select(c({{var}}, 
                    all_of(all_predictors))) %>%
    mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1)) %>% 
    mutate(dep = eval(parse(text = var))) %>% 
    dplyr::select(-c({{var}}, k)) %>%
    drop_na()
  
  # Split up training and set
  set.seed(123)
  data_split <- initial_split(model_data, prop = proportion)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  n_test = nrow(data_test)
  #data_fold <- bootstraps(data_train, strata = dep)

  # Set up recipe
  ranger_recipe <- data_train %>%
    recipe(dep ~ .) %>%
    step_corr(all_predictors(), -all_outcomes()) %>%
    prep()
  
  # Make the actual model
  set.seed(234)

  rf_model <-
    rand_forest(mtry = m_try, trees =  ntree, mode = "regression") %>%
    set_engine(model) %>%
    fit(dep ~ ., data = data_train)

  # Calculate predicted variables
  model_fit <- data_test %>%
    mutate(predict(rf_model, data_test))

  # Calculate metrics for model performance
  rmse = hydroGOF::rmse(model_fit$.pred, model_fit$dep)
  r2 = hydroGOF:: gof(model_fit$.pred, model_fit$dep)["R2", ]

  # Shows which model is run to keep track of progress
  print(paste(model, paste(predictors, collapse = ","), proportion, m_try, ntree))

  # # All the information exported
  output = tibble(dep = {{var}},
                  predictors = paste(predictors, collapse = ","),
                  proportion = proportion,
                  m_try = m_try,
                  ntree = ntree,
                  rmse = rmse,
                  r2 = r2,
                  n_test = n_test)

  ggplot(model_fit, aes(dep, .pred)) + geom_point() + 
    geom_abline(slope = 1, intercept = 0)
  
  output
}

## Test/troubleshoot function
calculate_metrics(proportion = 0.7, model = "ranger", m_try = 1, ntree = 100, model_no = 1)


## 3. Run models ###############################################################

# Assemble table of the initial models to be run
model_list <- tibble(expand.grid(m_try = m_try,
                                 ntree = ntree,
                                 proportion = proportion,
                                 model = model_package)) %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(model_no = seq.int(nrow(.))) 

tic("run model")
models_oob <- model_list %>%
  pmap(calculate_metrics) %>% 
  bind_rows()
toc()


ggplot(models_oob, aes(as.factor(proportion), r2)) + 
  geom_boxplot()

  
## Saving Outputs ##############################################################

write_csv(models_oob, "data/model_metrics_pr.csv")
 