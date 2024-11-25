#Script to prepare covariates for simulation

# load required libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
source("../R_scripts/Functions/UKCEH_functions.R")
library(readxl)
library(progress)

pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) :eta",
  total = 24, # Total number of steps
  clear = FALSE,
  width = 60
)
pb$tick()
CRS_used <- "EPSG:27700"
pb$tick()
hab_uk <- rast("../Data/UKCEH-2018-25m_AllUK/hab.tif") %>%
  subset(., 1)
pb$tick()
hedges_uk <- st_read("../Data/Linear+Woody+Features_2393217/wlff-2016_5294765/GB_WLF_V1_0.gdb")

### load in the release pen and convert to a closed shape ####
pb$tick()
pen_sf <- st_read("../Outputs/Shapefiles/pen_shapefile.shp")

## Habitat raster ####

### load in, transform, crop the habitat raster and change values to UKCEH AC list ####
pb$tick()
hab <- hab_uk %>%
  subset(., 1) %>%
  crop(., st_buffer(pen_sf, dist = 5000)) %>%
  LC_to_AC(.) %>%
  `crs<-`(CRS_used)

### save the cropped habitat raster ####
pb$tick()
writeRaster(hab, "../Outputs/Rasters/cropped habitat raster.tif", overwrite = T)

### extract the extent of the cropped habitat raster to use for other data ####
pb$tick()
ext <- as.polygons(hab, extent=T) %>%
  st_as_sf(.)

## Distance to release pen ####

### create a raster of distance to release pen and save ####
pb$tick()
pen_dist <- distance(hab, pen_sf)
writeRaster(pen_dist, "../Outputs/Rasters/cropped pen distance raster.tif", overwrite = T)

## Release woodland ####

### create a buffer around the release pen as vect and raster ####
pb$tick()
pen_buffer <- buffer(vect(pen_sf), width = 500)
pb_rast <- rasterize(pen_buffer, hab, res = 1000) %>%
  ifel(. == 1, 33, .) #note: release area is marked as value 33

### merge pen buffer into the habitat raster and save ####
pb$tick()
hab_pb <- merge(pb_rast, hab)
writeRaster(hab_pb, "../Outputs/Rasters/cropped release pen habitat raster.tif", overwrite = T)

## Hedges ####

### load in and crop hedgerow data ####
pb$tick()
hedges <- hedges_uk %>%
  st_crop(., ext) 

### save cropped hedgerow shapefile ####
pb$tick()
st_write(st_as_sf(hedges), "../Outputs/Shapefiles/cropped hedgerow shapefile.shp", append = F)

### create a raster of distance to nearest hedgerow ####
pb$tick()
hedges_dist <- terra::distance(hab, hedges)

### save the distance to hedgerows raster ####
pb$tick()
writeRaster(hedges_dist, "../Outputs/Rasters/cropped hedgerow distance raster.tif", overwrite = T)

## Hedges and edges ####

### extract just the woodland from hab ####
pb$tick()
wood_rast <- ifel(hab %in% 1:2, 1, NA)

### save just woodland as a raster ####
pb$tick()
writeRaster(wood_rast, "../Outputs/Rasters/cropped wood raster.tif", overwrite = T)

### extract and save distance to woodland raster ####
pb$tick()
wood_dist <- distance(wood_rast)
writeRaster(wood_dist, "../Outputs/Rasters/cropped wood distance raster.tif", overwrite = T)

### convert the hedges shapefile into a raster ####
pb$tick()
hedges_rast <- rasterize(hedges, hab, res = 1000)

### merge the hedges and woodland to make hedges and edges and save ####
pb$tick()
hedges_edges_rast <- merge(wood_rast, hedges_rast)
writeRaster(hedges_edges_rast, "../Outputs/Rasters/cropped hedges_edges raster.tif", overwrite = T)

### create a raster of the distance to hedges and edges and save####
pb$tick()
he_dist <- distance(hedges_edges_rast)
writeRaster(he_dist, "../Outputs/Rasters/cropped hedges_edges distance raster.tif", overwrite = T)

## Edges of fields ####
pb$tick()
field_edges <- ifel(hab %in% 3:4, 1, NA) %>%
  as.polygons(., dissolve = TRUE) %>%
  .[!is.na(values(.)), ] %>%
  st_as_sf(.) %>%
  st_boundary(.)

pb$tick()
field_edges_dist <- distance(hab, field_edges)
writeRaster(field_edges_dist, "../Outputs/Rasters/cropped field_edges distance raster.tif", overwrite = T)

## Feeders ####

### load in the feeder points and convert to shapefile ####
pb$tick()
feeders <- st_read("../Outputs/Shapefiles/feeders_shapefile.shp")

### create a raster of distance to feeders and save ####
pb$tick()
feed_dist <- distance(hab, feeders)
writeRaster(feed_dist, "../Outputs/Rasters/cropped feeder distance raster.tif", overwrite = T)
gc()

