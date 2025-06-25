## Constants file for the model variables
##
## Grace Tiegs 6-25-2025
## 
##
# ############## #
# ############## #

## 1. Making and setting up combinations of predictors ----------------------------

predictor_names <- c("clay", "nitrogen", "sand",
                     "silt", "mean_annual_air_temp_c", "mean_precip")

generate_combos <- function(names_vector, min_combo_size){
  combos_list <- list()
  max_size <- length(names_vector)
  for (n in min_combo_size:max_size) {
    combos <- combn(names_vector, n, simplify = FALSE)
    combos_list <- c(combos_list, combos) 
    }
  return(combos_list)
}


combo_results <- generate_combos(predictor_names, 5)

# ggplot theme
theme_set(theme_bw())

## Prep list of models to run ##################################################

#dataset = "teabags"
model_package = "ranger"
predictors = combo_results
proportion = c(0.6, 0.7, 0.8, 0.9)
m_try = c(2, 3, 4, 5, 6)
ntree = c(100, 500, 1000, 2000)




