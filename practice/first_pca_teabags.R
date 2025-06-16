## Script for Basic PCA
## Teabag Study in Wetlands and Forests
##
## Grace Tiegs 6-13-2025
## 
##
## https://youtu.be/_1msVvPE_KY?si=H-wScdNuhuk9mfGt
##
# ############## #
# ############## #

## Load in packages ############################################################
require(pacman)
p_load(tidyverse,
       tidymodels,
       embed # UMAP
       )

## Explore the data ############################################################
file_path_tea <- ("grace_tiegs_SULI/data/GT_Teabag_Types.xlsx")
teabags <- read_excel(file_path_tea)

teabags_df <- teabags %>%
  na.omit() %>%
  filter(!is.na(k)) %>%
  select(objectid, k, category, koppen_geiger_climate_class, elevation_meters, organic_carbon_density,
         clay, nitrogen, sand, silt)
  # only for when category is being used as a predictor
#  %>% mutate(category = (case_when(
#    (grepl("Wetland", category) ~ 1),
#    (grepl("Forest", category) ~ 2),
#    TRUE ~ 3)))

## Principal component anaylsis ################################################
# use math to build low dimensional spaces to visualize where each of these
# samples are.

# no outcome for the recipe these time
pca_teabags_recipe <- recipe(~ ., data = teabags_df) %>%
  # I want to keep the id but don't want it in the analysis
  # I put category here for the time being to make the plot below.
  update_role(objectid, category, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

# some values computed. the recipe didn't compute anything
# this will tell you it is trained
pca_prep <- prep(pca_teabags_recipe)

# tidy up the pca things. what are the values for each component.
tidied_pca <- tidy(pca_prep, 2)

# visualization
# bar graph with +/-
tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

# bar graph 
tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  labs(y = NULL, fill = "Positive?")

# get data back out. name and where they are in the 5 pca dimensional space
juice(pca_prep)

# plot the juice
juice(pca_prep) %>%
  ggplot(aes(PC1, PC4)) +
  # category is the id now, but can be changed above
  geom_point(aes(color = category), alpha = 0.7, size = 2)

# juice with labels of each point
juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = objectid)) +
  geom_point(aes(color = category), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE)



## UMAP stuff ? ##
umap_rec <- recipe(~ ., data = teabags_df) %>%
  # again, category should maybe be taken out ?
  update_role(objectid, category, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

umap_prep <- prep(umap_rec)

juice(umap_prep) %>%
  ggplot(aes(PC3, PC4)) +
  geom_point(aes(color = category), alpha = 0.7, size = 2)
  