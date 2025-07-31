## Constants file for the model variables
##
## Grace Tiegs 6-25-2025
## 
##
# ############## #
# ############## #

## 1. Making and setting up combinations of predictors ----------------------------

#predictor_names <- c("clay", "nitrogen", "sand",
                     #"silt", "mean_annual_air_temp_c", "mean_precip")
predictor_names <- c("s", "koppen_geiger_climate_class", "category", "ecosystem_zone",
                     "organic_carbon_density", "category", "mean_annual_air_temp_c",
                     "elevation_meters", "mean_annual_precip_mm", "p_h", "sand", "silt",
                     "soil_organic_carbon_content",	"water_content_10kpa",
                     "water_content_33kpa","water_content_1500kpa",
                     "total_nemotode_per100g",	"total_nemotode_per_square_meter", "bulk_density_0_5cm")
#predictor_names <- c("water_content_1500kpa", "sand", "silt", 
                     #"organic_carbon_density", "mean_precip", "mean_annual_air_temp_c",
                     #"elevation_meters")

generate_combos <- function(names_vector, min_combo_size){
  combos_list <- list()
  max_size <- length(names_vector)
  for (n in min_combo_size:max_size) {
    combos <- combn(names_vector, n, simplify = FALSE)
    combos_list <- c(combos_list, combos) 
    }
  return(combos_list)
}

## modify if you want to see variability in predictors.
## setting min # of predictors in each
combo_results <- generate_combos(predictor_names, 19)

# ggplot theme
theme_set(theme_bw())

## Prep list of models to run ##################################################
model_package = "ranger"
proportion = c(0.6, 0.7, 0.8, 0.9)
m_try = c(2, 3, 4, 5, 6)
ntree = c(100, 500, 1000, 2000)

model_list <- tibble(expand.grid(m_try = m_try,
                                 ntree = ntree,
                                 proportion = proportion,
                                 predictors = combo_results,
                                 model = model_package)) %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(model_no = seq.int(nrow(.))) 



