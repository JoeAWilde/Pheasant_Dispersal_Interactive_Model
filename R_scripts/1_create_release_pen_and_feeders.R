library(tidyverse)
library(progress)
library(sf)
library(terra)

CRS_used <- "EPSG:27700"

# Define total steps
total_steps <- 5

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) :eta",
  total = total_steps,
  clear = FALSE,
  width = 60
)

# Step 1: Load site coordinates
site_coords <- read.table("../coord_files/release_coords.csv", sep = ",", header = TRUE)
pb$tick()  # Update the progress bar after this step

# Step 2: Convert to sf objects
pen_coord <- site_coords %>% filter(Type == "Release Site")
feed_coord <- site_coords %>% filter(Type == "Feeder")
rm(site_coords)
pen_sf <- st_as_sf(pen_coord, coords = c("Longitude", "Latitude"), crs = "EPSG:4326") %>%
  st_transform(., CRS_used)
feed_sf <- st_as_sf(feed_coord, coords = c("Longitude", "Latitude"), crs = "EPSG:4326") %>%
  st_transform(., CRS_used)
pb$tick()  # Update the progress bar after this step

# Step 3: Write pen shapefile (suppress other output)

st_write(pen_sf, "../Outputs/Shapefiles/pen_shapefile.shp", append = FALSE, quiet = TRUE)
pb$tick()  # Update the progress bar after this step

# Step 4: Write feeders shapefile (suppress other output)
st_write(feed_sf, "../Outputs/Shapefiles/feeders_shapefile.shp", append = FALSE, quiet = TRUE)
pb$tick()  # Update the progress bar after this step

# Step 5: Complete
pb$tick()  # Final tick
cat("\nProcess complete\n")
