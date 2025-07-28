## Script for Ecosystem Category Map
##
## Grace Tiegs 6-9-2025
##
# ############## #
# ############## #

## ---------------Map of Sites separated by Forest/Wetlands --------------------

## Load in packages
require(pacman)

## To be able to download data frame as Excel
install.packages("xlsx")

p_load(tidyverse,
       tidymodels,
       readxl, # for reading in Raul's tea bag data
       sf, ## for the spatial stuff
       maps, ## for the world map
       janitor, ## for the clean names
       dplyr,
       tidyr,
       openxlsx,
       rnaturalearth ## for the world map
)

## Set data path
data_path_2 <- "data/CoastalTeaBagsPoints.xlsx"

## Set common coordinate reference system
common_crs = 4326

## Read in the file
teabags_points <- read_excel(data_path_2) %>%  
  clean_names() 

## Filtering it down to drop the NA coordinates
teabags_points_fil <- teabags_points %>% drop_na(longitude, latitude)

## Lots of statements to categorize the wetland vs forest exactly how we want them
teabags_type <- teabags_points_fil %>%
  mutate(category = (case_when(
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

# Specific columns that we want to use
select_teabags_type <- teabags_type %>%
  select(objectid, longitude, latitude, koppen_geiger_climate_class, ecosystem_type_reported, ecosystem_zone,
         category, k, s, elevation_meters, clay, nitrogen, organic_carbon_density,
         p_h, sand, silt)

# save this information as an Excel file
write.xlsx(select_teabags_type, "data/GT_Teabag_Types.xlsx")

## change to simple feature
teabags_type_sf <- (st_as_sf(teabags_type, coords = c("longitude", "latitude"), 
                            crs = common_crs))
## change the order that the labels appear and color in for ggplot purposes
teabags_type_sf$category <- factor(teabags_type_sf$category,
                                   levels=c("Forest", "Wetland", "Other"))

world <- ne_coastline()

## Plot the map
ggplot() +
  theme_bw() +
  ## to remove the grid lines and change font
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(family = "serif")) +
  geom_sf(data = world, fill = "white") +
  geom_sf(data = teabags_type_sf, aes(fill = category), color = 'black', shape = 21, size = 3, stroke = 0.5) +
  scale_fill_manual(values = c("Forest" = "#bbdaa4",
                               "Wetland" = "#4a80f5",
                              "Other" = "#d4a1e1")) +
  labs(title = "Forest and Wetland\nTeabag Sampling Locations", subtitle = "n = 2383") +
  coord_sf(expand = FALSE)


# 6. Save the figure -----------------------------------------------------------
ggsave("data/sites_by_forest_or_wetland_map.png",  width = 7, height = 6)
