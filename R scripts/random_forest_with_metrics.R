## Adding Feature Importance and Partial Dependence
## to the basic random forest
##
## Grace Tiegs 7-28-25
##
# ############## #
# ############## #


# Transforming the data:
#shapiro.test(transformTukey(model_data$dep, start = -10, end = 10, int = 0.025))
#shapiro.test(model_data$trans_dep)

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
  extrafont, # for graphs
  DALEXtra # for partial dependence
)

## Which packages to use over others
tidymodels_prefer()
conflicted::conflicts_prefer(ggplot2::annotate)

# Fonts for Graphs
font_import()
loadfonts(device = "win")

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
  model_data <- as_tibble(teabags) %>% 
    dplyr::select(c({{var}}, 
                    all_of(predictor_names))) %>%
    mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1)) %>% 
    mutate(dep = eval(parse(text = var))) %>%
    #mutate(trans_dep = dep^0.375) %>%
    dplyr::select(-c({{var}}, k)) %>% ## need to have dep in here if doing trans
    drop_na()
  
  # Split up training and set
  set.seed(123)
  data_split <- initial_split(model_data, prop = 0.8)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  n_test = nrow(data_test)

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
  
  # Plot model predictions vs actual values
  ggplot(model_fit, aes(dep, .pred)) + geom_point() + 
    labs(title = "Actual vs Model-Predicted Decay Rate",
         #subtitle = expression("Transformed k (k"^0.375*")"),
         x = "k",
         y = "Predicted k") +
    stat_poly_line() +
    stat_poly_eq() +
    annotate(geom = "text",
             x = 0.018,
             y = 0.027,
             hjust = 0.025,
             label = "n = 329") +
    theme(text=element_text(family="Calibri"))

  ggsave("data/model_predictive_accuracy.png", width = 12, height = 5) 

## 4. Feature Importance  ------------------------------------------------------
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
    labs(title = "Feature Importance for Decay Constant",
         subtitle = "Forests and Wetlands") +
    theme(text=element_text(family="Calibri")) +
    geom_col(alpha = 0.8, show.legend = F, width = 0.7) +
    labs(
         x = "Feature Importance (%)",
         y = "", fill = "")
  
  print(feature_graph)
  
  plot_grid(feature_graph, feature_graph_wetlands, feature_graph_forests, ncol = 1)
  
  write_csv(feature_data, "data/feature_importance/all_pred_all.csv")
  ggsave("data/feature_importance/combo_fi_plot.png", width = 15, height = 27) 
  
## 5. Partial Dependence--------------------------------------------------------
  teabags_explainer <- explain_tidymodels(
    rf_model,
    data = dplyr::select(data_train, -dep),
    y = data_train$dep,
    verbose = FALSE
  )
   # graph plotting helper function
  pdp_plotter <-
    function(pdp_var, xlab){
      
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
  
## Extra - Exploratory analysis ------------------------------------------------
ggplot(teabags, aes(k, soil_organic_carbon_content, color = bulk_density_0_5cm)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq() +
  labs(title = "soc and k forests")

ggsave("data/k_vs_soc_forests.png", width = 8, height = 5)
