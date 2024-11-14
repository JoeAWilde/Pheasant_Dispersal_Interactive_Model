library(tidyverse)
library(progress)
library(sf)
library(terra)
library(tidyterra)
library(readxl)

CRS_used <- "EPSG:27700"


site_coords <- read_xlsx("all_PA_sites.xlsx") %>%
  mutate(
    site_id = paste0(substr(Location, 1, 2), Approx_dist_from_PA)
  )

ex_pen <- read.table("data/Data for Exeter - anonymised LandscapeV2/Site A/Site A_Release Pen Coordinate data.csv", 
                     sep = ",", header = T)

ex_feed <- read.table("data/Data for Exeter - anonymised LandscapeV2/Site D/Site D Hopper_Feeder Location Data.csv", 
                      sep = ",", header = T)

for(i in 1:nrow(site_coords)) {

  if(i == 1) {
    pb <- progress_bar$new(total = nrow(site_coords), format = "[:bar] :percent eta::eta", clear = F)
    pb$tick(0)
  }
  site_id <- site_coords$site_id[i]
  
  ith_pen <- ex_pen %>%
    mutate(
      X = X + site_coords$Easting[i], 
      Y = Y + site_coords$Northing[i]
    )%>%
    dplyr::select(X, Y)
  # Ensure the loop is closed
  if(any(ith_pen[nrow(ith_pen), ] != ith_pen[1, ])) {
    ith_pen <- rbind(ith_pen, ith_pen[1, ])
  }
  # Convert to sf, specifying the CRS if known
  pen_sf <- st_as_sf(ith_pen, coords = c("X", "Y"), crs = CRS_used)
  # Convert to LINESTRING
  pen_line <- st_sfc(st_linestring(as.matrix(ith_pen)), crs = CRS_used)
  pen <- vect(st_cast(pen_line, "POLYGON", crs = CRS_used)) %>%
    st_as_sf()
  
  st_write(pen, paste0("outputs/PA sites/script_3/", site_id, "_pen_shapefile.shp"), append = F, quiet = T)
  
  ith_feeders <- ex_feed %>%
    mutate(
      X = X + site_coords$Easting[i], 
      Y = Y + site_coords$Northing[i]
    )  %>%
    st_as_sf(., coords = c("X", "Y"), crs = CRS_used)
  st_write(ith_feeders, paste0("outputs/PA sites/script_3/", site_id, "_feeders_shapefile.shp"), append = F, quiet = T)
  
  pb$tick()
}
