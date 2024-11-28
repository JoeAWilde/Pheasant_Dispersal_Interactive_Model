library(tidyverse)
library(RColorBrewer)
library(progress)
library(sf)
library(amt)
library(terra)
library(tidyterra)
library(tidyverse)
library(ggnewscale)
library(ggblend)
library(ggspatial)
source("../R_scripts/Functions/UKCEH_functions.R")

df <- readRDS("../Outputs/Simulations/Combined simulations/summarised sim data.rds")



p1 <- ggplot(data = df) +
  geom_line(aes(x = month, y = birdhours_in_PA), linetype = "dashed") + 
  geom_point(aes(x = month, y = birdhours_in_PA, shape = "Mean"), size = 3) +
  geom_errorbar(aes(x = month, ymin= sd_low_PA_birdhours, ymax=sd_high_PA_birdhours, linetype = "±1 SD")) +
  geom_point(aes(x = month, y = low_birdhours_in_PA, shape = "Min"), size = 1.5) +
  geom_point(aes(x = month, y = high_birdhours_in_PA, shape = "Max"), size = 1.5) +
  scale_shape_manual(name = NULL, values = c("Mean" = 19, "Min" = 6, "Max" = 2)) + 
  scale_linetype_manual(name = NULL, values = c("±1 SD" = "solid")) + 
  scale_y_continuous(name = "Birdhours spent in protected area",
                     limits = c(0, max(df$high_birdhours_in_PA)+250)) +
  scale_x_discrete(name = "Month") +
  scale_fill_manual(name = "Data type", values = brewer.pal(3, "Dark2")) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

ggsave(p1, filename = "../Outputs/Plots/birdhours_in_PA.png", 
       height = 4320, width = 7890, units = "px")

sim_trk <- readRDS("../Outputs/Simulations/Combined simulations/all_simulation_data.rds") %>%
  na.omit() %>%
  make_track(tbl = ., .x = x, .y = y, .t = DateTime,
             ID = id, SinceRel = DaysSinceRel, crs = "EPSG:27700") %>%
  time_of_day(.) %>%
  mutate(week = week(t_),
         month = month(t_),
         day = yday(t_))

## Calculate KDEs ####
time_unit <- "month"

#make a template raster for KDEs
trast <- make_trast(sim_trk, res = 25)

#make multiple KDEs per time unit
hr_sim <- sim_trk %>%
  nest(data = -time_unit) %>%
  mutate(kde = map(data, hr_kde, trast = trast, levels = 0.95, h = c(25, 25)))

#load in habitat data and UKCEH colours
hab_rast <- terra::rast("../Outputs/Rasters/cropped habitat raster.tif") %>%
  ifel(. == 0, NA, .)

hab_cols <- UKCEH_colours(hab_rast, T)

PAs <- st_read("../Data/Protected Areas/All_UK_PAs.shp") %>%
  st_transform(crs = "EPSG:27700") %>%
  st_crop(., st_buffer(st_as_sf(sim_trk[1,], coords = c("x_", "y_"),
                                crs = "EPSG:27700"), dist = 5000))

pen <- st_read("../Outputs/Shapefiles/pen_shapefile.shp")

feed <- st_read("../Outputs/Shapefiles/feeders_shapefile.shp")

## Find the levels of the grouping factor that the real and simulated data have in common
## Need this as there are weeks worth of gaps in the real data
# TrkLevs <- as.data.frame(hr_pheas[,colnames(hr_pheas) == time_unit])
SimLevs <- as.data.frame(hr_sim[,colnames(hr_sim) == time_unit])
Facets <- unique(SimLevs$month[!SimLevs$month %in% 3:6])


#create the boundaries of the plot
min_x <- min(sim_trk$x_, na.rm = T)
max_x <- max(sim_trk$x_, na.rm = T)

min_y <- min(sim_trk$y_, na.rm = T)
max_y <- max(sim_trk$y_, na.rm = T)

if(min_x < xmin(hab_rast)) min_x <- xmin(hab_rast)
if(max_x > xmax(hab_rast)) max_x <- xmax(hab_rast)

if(min_y < ymin(hab_rast)) min_y <- ymin(hab_rast)
if(max_y > ymax(hab_rast)) max_y <- ymax(hab_rast)


gc()

## Loop through each level and make a plot
for(j in 3:length(Facets)){
  if(j == 3) {
    pb <- progress_bar$new(total = length(Facets)-2, format = "[:bar] :percent eta::eta", clear = F)
    pb$tick(0)
  }
  
  sim_kde <- hr_sim[hr_sim[,colnames(hr_sim) == time_unit] == paste0(Facets[j]),]$kde[[1]]
  sim_isos <- hr_isopleths(sim_kde)
  
  ## make plot
  
  pt <- ggplot() +
    geom_spatraster(data = as.factor(hab_rast), alpha = 0.35) +
    
    scale_fill_manual(name = "Habitat", values = hab_cols$colour,
                      labels = hab_cols$habitat) +
    ## Add home ranges
    geom_sf(data = sim_isos, fill = "orange3", alpha = 0.5, linewidth = 1) + 
    geom_sf(data = PAs, fill = "lightgrey", colour = "black", alpha = 0.3, aes(linetype = "Protected Area"), linewidth = 3) +
    scale_linetype_manual(name = "", values = c("Protected Area" = "dashed")) +
    ## Add release pen locations
    geom_sf(data = pen, aes(colour = "Release point",  size = "Release point")) +
    geom_sf(data = feed, aes(colour = "Feeders", size = "Feeders")) + 
    scale_colour_manual(name = "Feature", values = c("Release point" = "grey", 
                                                     "Feeders" = "firebrick")) + 
    scale_size_manual(name = "Feature", values = c("Release point" = 8, 
                                                     "Feeders" = 4)) + 
    
    annotation_north_arrow(which_north = "grid", height = unit(1, "cm"),
                           width = unit(1, "cm"), aes(location = "tr"),
                           pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm"),) +
    annotation_scale(height = unit(0.5, "cm"), pad_x = unit(2, "cm"),
                     pad_y = unit(0.6, "cm"), text_cex = 1) +
    
    ## change labels
    ggtitle(month.name[Facets[j]]) +
    
    
    ## set theme
    theme_light(base_size = 30) +
    theme(legend.key.width = unit(2, "cm")) +
    scale_y_continuous(name = "", 
                       breaks = NULL, limits = c(min_y, max_y)) +
    scale_x_continuous(name = "", 
                       breaks = NULL, limits = c(min_x, max_x)) +
    coord_sf(datum = pull_crs(hab_rast))
  # pt
  
  
  month <- paste0(Facets[j])
  
  output_path <- paste0("../Outputs/Plots/Estimated Homeranges/",j-2, "_", month.name[as.integer(month)], "_homeranges.png")
  ggsave(pt, filename = output_path, units = "px", height = 4320, width = 7980)
  
  pb$tick()
};while (dev.cur() != 1) dev.off()
