if(!require("tidyverse")) install.packages("tidyverse")
if(!require("terra")) install.packages("terra")

source("../R_scripts/Functions/UKCEH_functions.R")

hab_rast <- rast("../Outputs/Rasters/cropped habitat raster.tif") %>%
  LC_to_AC(.)

writeRaster(hab_rast, "../Outputs/Rasters/cropped habitat raster.tif", overwrite = T)
