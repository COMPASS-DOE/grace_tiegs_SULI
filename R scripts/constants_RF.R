## Constants file for the model variables
##
## Grace Tiegs 6-20-2025
## 
##
# ############## #
# ############## #


## Set columns for independent and dependent variables #########################
###dependents <- c("k")
#k_predictors <- c("category", "ecosystem_type_reported", "koppen_geiger_climate_class",
                  #"elevation_meters", "organic_carbon_density", "clay", "nitrogen", "sand",
                  #"silt", "mean_annual_air_temp_c", "mean_precip")
k_predictors <- c("clay", "nitrogen", "sand", "silt", "mean_annual_air_temp_c", "mean_precip")

# ggplot theme
theme_set(theme_bw())

## Prep list of models to run ##################################################

dataset = ("teabags")
model_packages = c("randomForest", "ranger")
proportion = c(0.6, 0.7, 0.8, 0.9)
m_try = c(2, 3, 4, 5, 6)
ntree = c(100, 500, 1000, 2000)

# Assemble table of the initial models to be run
model_list <- tibble(expand.grid(data = dataset,
                                 model = model_packages,
                                 var = var,
                                 m_try = m_try,
                                 ntree = ntree,
                                 proportion = proportion)) %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(model_no = seq.int(nrow(.)))