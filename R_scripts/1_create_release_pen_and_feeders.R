library(tidyverse)
library(progress)
library(sf)
library(terra)

CRS_used <- "EPSG:27700"

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) :eta",
  total = 5, # Total number of steps
  clear = FALSE,
  width = 60
)

# Step 1: Load site coordinates
pb$tick()
cat("Step 1: Loading site coordinates\n")
site_coords <- read.table("../coord_files/release_coords.csv", sep = ",", header = TRUE)

pen_coord <- site_coords %>%
  filter(Type == "Release Site")
feed_coord <- site_coords %>%
  filter(Type == "Feeder")
rm(site_coords)

# Step 2: Convert to sf objects
pb$tick()
cat("Step 2: Converting to sf objects\n")
pen_sf <- st_as_sf(pen_coord, coords = c("Longitude", "Latitude"), crs = "EPSG:4326") %>%
  st_transform(., CRS_used)
feed_sf <- st_as_sf(feed_coord, coords = c("Longitude", "Latitude"), crs = "EPSG:4326") %>%
  st_transform(., CRS_used)

# Step 3: Write pen shapefile
pb$tick()
cat("Step 3: Writing pen shapefile\n")
st_write(pen_sf, "../Outputs/Shapefiles/pen_shapefile.shp", append = FALSE, quiet = TRUE)

# Step 4: Write feeders shapefile
pb$tick()
cat("Step 4: Writing feeders shapefile\n")
st_write(feed_sf, "../Outputs/Shapefiles/feeders_shapefile.shp", append = FALSE, quiet = TRUE)

# Step 5: Done
pb$tick()
cat("Step 5: Process complete\n")
