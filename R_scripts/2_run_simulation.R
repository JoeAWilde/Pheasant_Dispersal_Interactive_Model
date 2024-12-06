#Script to run the dispersal simulation 

#check packages are installed
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("terra")) install.packages("terra")
if(!require("doSNOW")) install.packages("doSNOW")
if(!require("sf")) install.packages("sf")
if(!require("suncalc")) install.packages("suncalc")
if(!require("progress")) install.packages("progress")

# # load required libraries
library(tidyverse)
library(terra)
library(doSNOW)
library(sf)
library(suncalc)
library(progress)

source("../R_scripts/Functions/simulation function.R")

CRS_used <- "EPSG:27700"

## load in the step length parameters ####
sl_model <- readRDS("../Data/Analyses_results/step_length_pars.rds")

sl_pars <- list(
  Intercept = sl_model$estimates[1],
  Hab_values = as.integer(substr(sl_model$est_names[2:10], 9, 11)),
  Hab_betas = sl_model$estimates[2:10],
  Time_beta = sl_model$estimates[11],
  Gam_shape = sl_model$estimates[12]
)

## load in turning angle parameters ####
ta_model <- readRDS("../Data/Analyses_results/turning_angle_pars.rds")
ta_pars <- list(
  vm_mu = ta_model$estimates_angle[1], 
  vm_kappa = ta_model$estimates_angle[12]
)

# iSSF parameters ####

## load in iSSF coefficients ####
ssf_betas <- readRDS("../Data/Analyses_results/iSSF_pars.rds")

## extract names of coefficients ####
cov_names <- names(ssf_betas)


# Simulation parameters ####
st_date <- ymd_hms("2018-07-18 07:05:00")
n_IDS <- read.table("../Outputs/User input/n_IDs.csv", sep = ",", header = T)$Value[
  read.table("../Outputs/User input/n_IDs.csv", sep = ",", header = T)$Variable == "n_IDs"]
fix_rate <- 60
n_steps <- as.numeric(difftime(st_date + months(7), st_date, units = "mins")) / fix_rate
n_csteps <- 250
stop_if_left <- TRUE
dogin_dates <- as.Date(seq(st_date, st_date + months(1), by = "days"))
dogin_times <- hms::as_hms(c("18:30:00", "20:00:00"))
dogin_prob <- 0.9

# Other parameters for simulation ####

## create a dataframe of sunlight times at the centre of the pen ####
pen_cent <- st_read("../Outputs/Shapefiles/pen_shapefile.shp") %>%
  st_centroid(.) %>%
  st_transform(., "EPSG:4326") %>%
  st_coordinates(.)

suntimes <- getSunlightTimes(as.Date(seq(from = st_date,
                                         to = st_date + minutes(fix_rate * (n_steps-1)),
                                         length.out = n_steps)),
                             lat = pen_cent[2], 
                             lon = pen_cent[1], 
                             keep = c("sunrise", "sunset"),
);rm(pen_cent)

## calculate daily survival rates ####
### daily probability of death before winter ####
Autmort <- list(
  Autdaysno = as.numeric((ymd("2018-11-01")-ymd("2018-07-01"))), # days in autumn
  Autdays = seq(ymd("2018-07-01"), ymd("2018-11-01"), by = "day"),
  AutSurv = 0.47 # overall proportion of birds that survive this period
  # daily probability an individual dies
)
Autmort$Autdaily = ( 1 - Autmort$AutSurv^(1/Autmort$Autdaysno))

### daily probability of death during winter ####
Wintmort <- list(
  wintdaysno = as.numeric((ymd("2019-04-01")-ymd("2018-11-01"))), # days in winter
  Wintdays = seq(ymd("2018-11-01"), ymd("2019-04-01"), by = "day"),
  WintSurv = 0.36
)# overall proportion of birds that survive this period
Wintmort$Wintdaily <-(1 - Wintmort$WintSurv^(1/Wintmort$wintdaysno)) # daily probability an individual dies

### daily probability of death during spring ####
Springmort <- list(
  Springdaysno = as.numeric((ymd("2019-07-01")-ymd("2019-04-01"))), # days in spring
  Springdays = seq(ymd("2019-04-01"), ymd("2019-07-01"), by = "day"),
  SpringSurv = 0.36 # overall proportion of birds that survive this period
)
Springmort$Springdaily <-( 1 - Springmort$SpringSurv^(1/Springmort$Springdaysno)) # probability and individual dies on a day

# Parallel processing set up ####
## create cluster of cores ####
cl <- makeCluster(parallel::detectCores(logical = F)-2, type = "SOCK")
registerDoSNOW(cl)

##create progress bar for simulation loop ####
pb <- progress_bar$new(
  format = "ID = :ID [:bar] :percent | :elapsed | eta::eta", 
  total = n_IDS, 
  clear = F
)

ID_list <- 1:n_IDS

progress <- function(n) {
  pb$tick(tokens = list(ID = ID_list[n]))
}

opts <- list(progress = progress)
# Simulation loop ####
foreach(id = 1:n_IDS, .options.snow = opts) %dopar% {
  ## reload required packages for each worker ####
  require(tidyverse)
  require(terra)
  require(sf)
  
  pen_pts <- st_read("../Outputs/Shapefiles/pen_shapefile.shp")
  
  ## create the area where dogging in occurs ####
  dogin_buffer <- st_difference(st_buffer(st_geometry(pen_pts), dist = 200), pen_pts)
  dogin_outside_edge <- st_boundary(st_buffer(st_geometry(pen_pts), dist = 200))
  
  ## load in covariate rasters (can't be passed to workers) ####
  short_list <- T
  hab <- rast("../Outputs/Rasters/cropped habitat raster.tif")
  pen <- rast("../Outputs/Rasters/cropped pen distance raster.tif")
  feed <- rast("../Outputs/Rasters/cropped feeder distance raster.tif")
  wood <- rast("../Outputs/Rasters/cropped wood distance raster.tif")
  hedges <- rast("../Outputs/Rasters/cropped hedgerow distance raster.tif")
  field_edges <- rast("../Outputs/Rasters/cropped field_edges distance raster.tif")
  
  ## bind all covariate rasters together ####
  covs <- c(feed, hab, wood, pen, hedges, field_edges)
  
  wood_rast <- rast("../Outputs/Rasters/cropped wood raster.tif")
  
  ## load in the hedges and egdes rasters ####
  hedges_edges <- rast("../Outputs/Rasters/cropped hedges_edges raster.tif")
  hedges_edges_dist <- rast("../Outputs/Rasters/cropped hedges_edges distance raster.tif")
  
  try({
    ## start the simulation ####
    sim_df <- id_sim(id, sl_pars, ta_pars, ssf_betas, cov_names, pen_pts, dogin_dates, dogin_times, 
                     dogin_prob, dogin_buffer, dogin_outside_edge, covs, wood_rast, Autmort, Wintmort, Springmort, 
                     st_date, n_IDs, n_steps, n_csteps, fix_rate, stop_if_left, suntimes, short_list, 
                     hedges_edges, hedges_edges_dist)

    
    ## save the simulation ####
    saveRDS(sim_df, paste0("../Outputs/Simulations/Individual simulations/", id, "_sim_output.rds"))
    rm(sim_df)
  })
}; stopCluster(cl)
