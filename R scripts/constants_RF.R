## Constants file for the model variables
##
## Grace Tiegs 6-20-2025
## 
##
# ############## #
# ############## #


## Set columns for independent and dependent variables #########################
dep <- "k"
#k_predictors <- c("category", "ecosystem_type_reported", "koppen_geiger_climate_class",
                  #"elevation_meters", "organic_carbon_density", "clay", "nitrogen", "sand",
                  #"silt", "mean_annual_air_temp_c", "mean_precip")
all_predictors <- c("clay", "nitrogen", "sand", "silt", "mean_annual_air_temp_c", "mean_precip")

# ggplot theme
theme_set(theme_bw())

## Prep list of models to run ##################################################

dataset = "teabags"
model_package = "ranger"
predictors = all_predictors
proportion = c(0.6, 0.7, 0.8, 0.9)
m_try = c(2, 3, 4, 5, 6)
ntree = c(100, 500, 1000, 2000)


