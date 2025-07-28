## Making it into a function to test many different parameter combos
##
## Grace Tiegs 6-19-2025
##
##
## Updated 7-28-2025 
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
       RColorBrewer
       )


## 2. Load in data #############################################################

file_path_tea <- ("data/GT_Teabag_Types.xlsx")

teabags <- read_excel(file_path_tea) %>%
  filter(!is.na(k)) %>% 
  filter(category == "Wetland" | category == "Forest") %>%
    select(k, category, ecosystem_type_reported, koppen_geiger_climate_class, 
         elevation_meters, bulk_density_0_5cm,organic_carbon_density,
         clay, nitrogen, sand, silt, mean_annual_air_temp_c, mean_precip)

## 3. Set up function and test -------------------------------------------------

#source("R scripts/constants_RF.R")
source("R scripts/predictors_RF.R")

var = "k" 

## Calculate metrics function 
calculate_metrics <- function(proportion = proportion,
                              model = model,
                              m_try = m_try,
                              predictors = predictors,
                              ntree = ntree,
                              model_no = model_no){
  
  model_data <- as_tibble(teabags) %>% 
    dplyr::select(c({{var}}, 
                    any_of(predictors))) %>%
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

  # Set up for recipe if using in model 
  ranger_recipe <- data_train %>%
    recipe(dep ~ .) %>%
    step_normalize(all_numeric_predictors()) %>%  # Normalize numeric predictors
    step_unknown(all_nominal_predictors()) %>%   # Handle unknown factor levels
    step_dummy(all_nominal_predictors()) %>% 
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
  nse = hydroGOF::NSE(model_fit$.pred, model_fit$dep)

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
                  nse=nse,
                  n_test = n_test)

  ggplot(model_fit, aes(dep, .pred)) + geom_point() + 
    geom_abline(slope = 1, intercept = 0)
  
  output
}

## 4. Run models ###############################################################

tic("run model")
models_oob <- model_list %>%
  pmap(calculate_metrics) %>% 
  bind_rows()
toc()

write_csv(models_oob, "data/model_metrics_predictors5.csv")

## 5. Analyzing Data -----------------------------------------------------------
p1 <- ggplot(models_oob, aes(as.factor(proportion), r2)) + 
  geom_boxplot()

p2 <- ggplot(models_oob, aes(as.factor(m_try), r2)) + 
  geom_boxplot()

p3 <- ggplot(models_oob, aes(as.factor(ntree), r2)) + 
  geom_boxplot()

p4 <- ggplot(models_oob, aes(as.factor(proportion), nse)) + 
  geom_boxplot()

p5 <- ggplot(models_oob, aes(as.factor(m_try), nse)) + 
  geom_boxplot()

p6 <- ggplot(models_oob, aes(as.factor(ntree), nse)) + 
  geom_boxplot()

plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

ggsave("data/r2_nse_boxplot.png")

## Function for the generic boxplots -------------------------------------------
myboxplot <- function(my_factor, var, ylab, my_title){
  
  colors = c("#A9A9A9", "#8FBC8F", "#4682B4", "#8B4513", "#FEA82F", "#E5DADA", "#F46D75")
  
  ymin = min(models_oob %>% select({{var}}))
  
  ggplot(models_oob, 
         aes(x = as.factor({{my_factor}}), y = {{var}}, fill = as.factor({{my_factor}}))) + 
    geom_boxplot(alpha = 0.9, color = "#0D0E23") + 
    labs(x = "", y = ylab, title = my_title, fill = "") +
    scale_y_continuous(limits = c(ymin, 0.45)) + 
    scale_fill_manual(values = colors) + 
    theme(legend.position='none',
      plot.title = element_text(hjust = 0.5))
}
--------------------------------------------------------------------------------
## Function for the predictor boxplots -----------------------------------------
## Same as above but the labels are in the legend
my_legended_boxplot <- function(my_factor, var, ylab, my_title){
  colors <- colorRampPalette(brewer.pal(9, "Set1"))(64)
  
  ymin = min(models_oob %>% select({{var}}))
  
  ggplot(models_oob, 
         aes(xlab= "", x = as.factor({{my_factor}}), y = {{var}}, fill = as.factor({{my_factor}}))) + 
    geom_boxplot(alpha = 0.9, color = "#0D0E23") + 
    labs(x = "", y = ylab, title = my_title, fill = "") +
    scale_y_continuous(limits = c(ymin, 0.45)) + 
    scale_x_discrete(labels = NULL) +
    scale_fill_manual(values = colors) + 
    theme(legend.position = "left",
          scale_fill_manual(values = colors))
}

--------------------------------------------------------------------------------
## 6. Saving Data ##############################################################
## Model Parameters Plot
plot_grid(myboxplot(proportion, r2, parse(text = "R^2"), "Dataset Split Ratio"), 
          myboxplot(m_try, r2, "", "Variables Used Per Split"), 
          myboxplot(ntree, r2, "", "Num. Trees"), 
          nrow = 1)

myboxplot(predictors, r2, parse(text = "R^2"), "Predictor Combo")

ggsave("data/metrics_color_boxplot_missing_predictors3.png", width = 13, height = 5)

## Predictors Parameters Plot
my_legended_boxplot(predictors, r2, parse(text = "R^2"), "Predictor Combination")

ggsave("data/metrics_predictors_boxplot5.png", width = 30, height = 5)
