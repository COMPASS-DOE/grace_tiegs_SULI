## Practice Script for Loading in a Map
## Using Raul's data from Spring 2025
##
## Grace Tiegs 6-2-2025
##
# ############## #
# ############## #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)

p_load(tidyverse,
       tidymodels,
       readxl, # for reading in Raul's tea bag data
       sf, ## for the spatial stuff
       maps, ## for the world map
       janitor, ## for the clean names
       dplyr
)

## Set data path
data_path <- "raul_vera_SULI/RV Tea_bag_studies_Metadata.xlsx"
data_path_2 <- "raul_vera_SULI/scripts/CoastalTeaBagsPoints.xlsx"

## Set common coordinate reference system
common_crs = 4326


# 2. Read in spreadsheet -------------------------------------------------------

## Load in the second sheet of the excel, "Global data - clean" + clean names
teabags <- read_excel(data_path, sheet = 2) %>%  
  clean_names() 

## Filtering it down to drop the NA coordinates
teabags <- teabags %>% drop_na(longitude, latitude)
  
## Change it to a tibble to make it easier to work with
teabags <- as_tibble(teabags)


# 3. Convert the data into an sf object ----------------------------------------
sf_object <- st_as_sf(teabags, coords = c("longitude", "latitude"), 
                      crs = common_crs)

# 4. Plot the data only --------------------------------------------------------
ggplot() + 
  geom_sf(data = sf_object)

# 5. Add world map layer -------------------------------------------------------
world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_sf(data = sf_object, aes(size = k, color = ecosystem_zone)) +
  theme_bw() +
  labs(title = "Teabag Sampling Locations",
       x = "Longitude",
       y = "Latitude")

# 6. Save the figure -----------------------------------------------------------
ggsave("grace_tiegs_SULI/data/teabag_practice_map.png",  width = 7, height = 6)



### Make a west US map only ----------------------------------------------------
## trying a new map load in style
p_load(rnaturalearth, tmaptools)
remotes::install_github("ropensci/rnaturalearthhires")

west <- ne_states(country = "united states of america") %>%
  filter(region == "West") %>% filter(name != "Alaska" & name != "Hawaii")


western_samples <- sf_object %>% crop_shape(., west, polygon = T)

ggplot() +
  geom_sf(data = west) +
  geom_sf(data = western_samples, aes(size = k, color = ecosystem_zone)) +
  theme_bw()

ggsave("grace_tiegs_SULI/data/teabag_ecosystem_practice_map.png",
       width = 7, height = 6)


## Plot of temp vs k -----------------------------------------------------------
ggplot(sf_object, aes(x= mean_annual_air_temp_c, y=k)) +
  geom_point()
  
## ---------------Map of Sites separated by Forest/Wetlands --------------------

## Load in the excel
teabags_points <- read_excel(data_path_2) %>%  
  clean_names() 

## Filtering it down to drop the NA coordinates
teabags_points_fil <- teabags_points %>% drop_na(longitude, latitude)

## Lots of statements to categorize the wetland vs forest exactly how we want them
teabags_type <- teabags_points_fil %>%
  mutate(Category = (case_when(
    (grepl("Mangroves", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Mangroves - dwarf", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Mangroves - Fringe", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Oceanic raised bog", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Geothermal wet grassland - warmed", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Geothermal wet grassland - ambient", ecosystem_type_reported) ~ "Wetland"),
    (grepl("peatland", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Floating fen", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Flooded Grassland", ecosystem_type_reported) ~ "Wetland"),
    (grepl("Saltmarsh", ecosystem_type_reported) ~ "Wetland"),
    
    (grepl("Lowland tropical Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Boreal Forest/Taiga", ecosystem_type_reported) ~ "Forest"),
    (grepl("Conifer forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Mediterranean Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Temperate Broadleaf Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Temperate Conifer Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Tropical and Subtropical Dry Broadleaf Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Tropical and Subtropical Moist Broadleaf Forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Tropical forest", ecosystem_type_reported) ~ "Forest"),
    (grepl("Tundra", ecosystem_type_reported) ~ "Forest"),
    (grepl("Wet Forest", ecosystem_type_reported) ~ "Forest"),

    TRUE ~ "Other"
  )))

## change to simple feature
teabags_type_sf <- st_as_sf(teabags_type, coords = c("longitude", "latitude"), 
                      crs = common_crs)

## change the order that the labels appear in for ggplot purposes
teabags_type_sf$Category <- factor(teabags_type_sf$Category, levels=c("Forest", "Wetland", "Other"))

world <- ne_coastline()

## make the map
ggplot() +
theme_bw() +
      ## to remove the grid lines and change font
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
theme(text = element_text(family = "serif")) +
geom_sf(data = world) +
geom_sf(data = teabags_type_sf, aes(color = Category), alpha = 0.6) +
labs(title = "Teabag Sampling Locations") +
coord_sf(expand = FALSE)
  
## more coordinate marks
# + scale_y_continuous(limits = c(-60, 85))



# 6. Save the figure -----------------------------------------------------------
ggsave("grace_tiegs_SULI/data/sites_by_forest_or_wetland_map.png",  width = 7, height = 6)