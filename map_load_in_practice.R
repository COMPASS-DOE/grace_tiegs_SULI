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
       janitor ## for the clean names
)
## Set data path
local_path <- "grace_tiegs_SULI/data/"
data_path <- "raul_vera_SULI/RV Tea_bag_studies_Metadata.xlsx"

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
  ## different maps
  world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_sf(data = sf_object, color = "darkgreen") +
  theme_minimal() +
  labs(title = "Teabag Sampling Locations",
       x = "Longitude",
       y = "Latitude")
