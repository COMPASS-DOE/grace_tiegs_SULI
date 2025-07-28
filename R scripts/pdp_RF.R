## Adding partial dependence to the feature_importance_RF
##
## Grace Tiegs 7-3-25
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
  readr,
  RColorBrewer,
  ggpmisc, # for R2 and linear fit
  rcompanion, # for normalizing
  DALEXtra # for the pdp
)

source("R scripts/predictors_RF.R")

var = "k" 

## 2. Load in data #############################################################

file_path_tea <- ("data/GT_Teabag_Types.xlsx")

teabags <- read_excel(file_path_tea) %>%
  filter(!is.na(k)) %>% 
  filter(category == "Wetland" | category == "Forest") %>%
  #filter(category == "Forest") %>%
  select(k, predictor_names)

## 3. Set up function and test -------------------------------------------------

# Calculate metrics and feature importance
{
  model_data <- as_tibble(teabags) %>% 
    dplyr::select(c({{var}}, 
                    all_of(predictor_names))) %>%
    mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1)) %>% 
    mutate(dep = eval(parse(text = var))) %>%
   # mutate(log_dep = log(dep + 1)) %>%
    #mutate(trans_dep = dep^0.375) %>%
    dplyr::select(-c({{var}}, k)) %>% ## need to have dep in here if doing trans
    drop_na()
  
  # Split up training and set
  set.seed(123)
  data_split <- initial_split(model_data, prop = 0.8)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  n_test = nrow(data_test)
  #data_fold <- bootstraps(data_train, strata = dep)
  
  # Make the actual model
  set.seed(234)
  
  rf_model <-
    rand_forest(mtry = 4, trees =  1000, mode = "regression") %>%
    set_engine("ranger", importance = "impurity") %>%
    fit(dep ~ ., data = data_train)
  
  # Calculate predicted variables
  model_fit <- data_test %>%
    mutate(predict(rf_model, data_test))
  
  # Calculate metrics for model performance
  rmse = hydroGOF::rmse(model_fit$.pred, model_fit$dep)
  r2 = hydroGOF:: gof(model_fit$.pred, model_fit$dep)["R2", ]
  nse = hydroGOF::NSE(model_fit$.pred, model_fit$dep)
  
  collect_predictions()
  
  ## All the information exported
  ggplot(model_fit, aes(dep, .pred)) + geom_point() + 
    labs(title = "log k vs predicted") +
    stat_poly_line() +
    stat_poly_eq()
  ggsave("data/report_combo_model_accuracy.png", width = 13, height = 5) 
  
} 

## Partial Dependence
teabags_explainer <- explain_tidymodels(
  rf_model,
  data = dplyr::select(data_train, -dep),
  y = data_train$dep,
  verbose = FALSE
)
  
pdp_plotter <-
  function(pdp_var, xlab){
    
    #mean_x = mean(model_data %>% select(pdp_var) %>% drop_na() %>% pull())
    #sd_x = sd(model_data %>% select(pdp_var) %>% drop_na() %>% pull())
      
    pdp_plot <- model_profile(
      teabags_explainer,
      variables = pdp_var,
      N = NULL,
      groups = "category"
    )
      
    as_tibble(pdp_plot$agr_profiles) %>%
      mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
      rename(pdp_var = `_vname_`) %>%
      ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      labs(
        x = xlab,
        y = "Predicted k",
        color = NULL
        #title = "Partial dependence plot for OM decay rate",
        #subtitle = "Predictions from a decision tree model"
      )
  }

p1 <- pdp_plotter("organic_carbon_density", expression("Organic Carbon Density   ("~hg/m^3~")"))
p2 <- pdp_plotter("water_content_10kpa", expression("Water Content at 10 kpa   ("~m^3~")"))
p3 <- pdp_plotter("water_content_1500kpa", expression("Water Content at 1500 kpa   ("~m^3~")"))
p4 <- pdp_plotter("mean_annual_air_temp_c", expression("Mean Annual Air Temperature   ("~C~")"))

plot_grid(p1, p2, p4, p3, nrow = 2)

## pdp_plotter("soil_organic_carbon_content", expression("Soil Organic Carbon Content   ("~dg/kg~")")),

plot_grid(
  pdp_plotter("s", expression("s")),
  pdp_plotter("water_content_1500kpa", expression("Water Content at 1500 kpa   ("~m^3~")")),
  pdp_plotter("sand", expression("Sand   ("~g/kg~")")),
  pdp_plotter("silt", expression("Silt   ("~g/kg~")")),
  nrow = 2
)

# sand, silt, nemotode
plot_grid(
  ggdraw() + 
    draw_label("Partial dependence plot for OM decay rate",
               fontface = 'bold',
               size = 12),
  pdp_plotter("sand", expression("Sand   ("~g/kg~")")),
  pdp_plotter("silt", expression("Mean Precipitation   ("~g/kg~")")),
  pdp_plotter("total_nemotode_per100g", expression("Total Nemotode    ("~g^-1~")")),
  nrow = 2
)

ggsave("data/pdp/assorted5_pdp.png", width = 13, height = 8)
  
### **** ### **
  {
  pdp_ocd <- model_profile(
    teabags_explainer,
    variables = "organic_carbon_density",
    N = NULL,
    groups = "category"
  )
  
  as_tibble(pdp_1500kpa$agr_profiles) %>%
    mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    labs(
      x = "Organic Carbon Density (hg/m^3)",
      y = "Predicted decay rate constant",
      color = NULL,
      title = "Partial dependence plot for OM decay rate",
      subtitle = "Predictions from a decision tree model"
    )
  
  ggsave("data/pdp/organic_carbon_density_pdp.png", width = 13, height = 5)
}


## Feature Importance
{
  var_names <- rf_model$fit$variable.importance
  col_names <- c("predictor", "raw_fi")
  
  fi0 <- as.data.frame(var_names) %>%
    tibble::rownames_to_column() %>%
    as_tibble()
  
  colnames(fi0) = col_names
  
  feature_data <- fi0 %>%
    filter(predictor != {{var}}) %>%
    #filter(predictor != "dep") %>%
    select(predictor, raw_fi) %>%
    mutate(fi = raw_fi / sum(raw_fi))
  
  feature_graph <- feature_data %>%
    ggplot(aes(fi * 100,
               reorder(predictor, fi), fill = predictor)) +
    geom_col(alpha = 0.8, show.legend = F, width = 0.7) +
    labs(x = "Feature Importance (%)",
         y = "", fill = "")
  
  print(feature_graph)
  
  write_csv(feature_data, "data/feature_importance/wetland_only_fi_transformed_k.csv")
  ggsave("data/feature_importance/fi_transformed_k.png", width = 13, height = 5) 
}

