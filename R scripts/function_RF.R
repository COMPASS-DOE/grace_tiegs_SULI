## Making it into a function so I can test a lot of different parameter combos
##
## Grace Tiegs 6-19-2025
## 
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
       hydroGOF
       )

source("grace_tiegs_SULI/R scripts/constants_RF.R")

## 2. Load in data #############################################################
file_path_tea <- ("grace_tiegs_SULI/data/GT_Teabag_Types.xlsx")
teabags <- read_excel(file_path_tea)

teabags_df <- teabags %>%
  filter(!is.na(k)) %>% filter(category == "Wetland" | category == "Forest") %>%
  select(k, category, ecosystem_type_reported, koppen_geiger_climate_class, 
         elevation_meters, organic_carbon_density,
         clay, nitrogen, sand, silt, mean_annual_air_temp_c, mean_precip)

teabags_df %>% mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1))

# Set the dependent variable for the models
var = "k"

############ VARIABLES FOR TESTING FUNCTION CODE #######################

data = teabags
predictors = k_predictors
proportion = 0.5
var = "k"
model = "ranger"
m_try = 2
ntree = 50
model_no = 1

########################################################
########################################################
########################################################

## 3. Run models ###############################################################
calculate_metrics <- function(data = data,
                              predictors = predictors,
                              proportion = proportion,
                              var = var,
                              model = model,
                              m_try = m_try,
                              ntree = ntree,
                              model_no = model_no){
  
    ## Create dataset with columns to be used
    model_data <- data %>% 
      dplyr::select({{var}}) %>% 
      mutate(dep = var) %>% 
      filter(!is.na(var))
    
    # Split up training and set
    set.seed(123)
    data_split <- initial_split(model_data, strata = var, prop = proportion)
    data_train <- training(data_split)
    data_test <- testing(data_split)
    n_test = nrow(testing_data)
    data_fold <- bootstraps(data_train, strata = var)
    
    # Set up recipe
    ranger_recipe <- data_train %>%
      recipe(as.formula(paste(var, "~ ."))) %>%
      # changed this to all_numeric from all
      step_corr(all_numeric_predictors(), -all_outcomes()) %>%
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
    print(paste(model, data, predictors, proportion, m_try, ntree))
    
    # All the information exported
    output = tibble(#data = data,
                    dep = {{var}},
                    predictors = predictors,
                    proportion = proportion,
                    m_try = m_try,
                    ntree = ntree,
                    rmse = rmse,
                    r2 = r2,
                    n_test = n_test
                    )
}

calculate_metrics(data = teabags, predictors = k_predictors, proportion = 0.5, var = "k", model = "ranger", m_try = 2, ntree = 50, model_no = 1)



#  tic("run model")
  
#  models_oob <- model_list %>%
#    pmap(calculate_metrics) %>%
#    bind_rows()
  
#  toc()
  
#write.xlsx(models_oob, "grace_tiegs_SULI/data/model_metrics.xlsx")
 