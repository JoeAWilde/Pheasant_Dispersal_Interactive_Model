library(tidyverse)
library(progress)
library(doSNOW)
library(sf)

root <- "../Outputs/Simulations/Individual simulations/"

sim_files <- paste0(root, list.files(root))

all_df <- lapply(sim_files, readRDS) %>%
  do.call(rbind, .)
saveRDS(all_df, "../Outputs/Simulations/Combined simulations/all_simulation_data.rds")

cen_pen <- st_read("../Outputs/Shapefiles/pen_shapefile.shp")

PAs <- st_read("../Data/Protected Areas/All_UK_PAs.shp") %>%
  st_transform(crs = "EPSG:27700") %>%
  st_crop(., st_buffer(cen_pen, dist = 5000))

sim_df <- all_df %>%
  mutate(month = month.name[month(DateTime)], 
         id_group = (id - 1) %/% 10 + 1)

grouped_sums <- sim_df %>%
  group_by(id_group) %>%
  mutate(
    total_birds_released = n_distinct(id), 
    total_fixes = n()
  ) %>%
  group_by(month, .add = T) %>%
  mutate(
    mean_birds = n_distinct(id),
    mean_monthly_fixes = n(),
  ) %>%
  ungroup() %>%
  distinct(month, id_group, .keep_all = T) %>%
  select(month, id_group, total_birds_released, total_fixes, mean_birds, mean_monthly_fixes)


cl <- makeCluster(parallel::detectCores(logical = F)-2, type = "SOCK")
registerDoSNOW(cl)

##create progress bar for simulation loop ####
pb <- progress_bar$new(
  format = "[:bar] :percent | :elapsed | eta::eta",
  total = nrow(grouped_sums),
  clear = F
)

progress <- function(n) {
  pb$tick()
}

opts <- list(progress = progress)
# Simulation loop ####
foreach(i = 1:nrow(grouped_sums), .options.snow = opts) %dopar% {
  require(sf)
  
  birdhours_in_PA = length(which((lengths(st_within(
    st_as_sf(sim_df[sim_df$id_group == grouped_sums$id_group[i] &
                      sim_df$month == grouped_sums$month[i], ],
             coords = c("x", "y"), crs = "EPSG:27700"), PAs)) > 0) == T))
  
  saveRDS(c(i, birdhours_in_PA), 
          paste0("../Outputs/Simulations/Combined simulations/Summarised loop outputs/meanfix_birdhours_", i, ".rds"))
  
  pb$tick()
}

stopCluster(cl)
mf_bh_root <- "../Outputs/Simulations/Combined simulations/Summarised loop outputs/"
mf_bh_files <- paste0(mf_bh_root, list.files(mf_bh_root)) %>%
  .[grepl("meanfix", .)]

i_mf_bh <- lapply(mf_bh_files, readRDS) %>%
  do.call(rbind, .) %>%
  data.frame(.) %>%
  rename(i = 1, 
         birdhours_in_PA = 2) %>%
  mutate(i = as.integer(i)) %>%
  arrange(i)

grouped_sums <- cbind(grouped_sums, i_mf_bh %>% select(-i)) %>%
  mutate(
    prop_birds_alive = mean_birds / total_birds_released, 
    monthly_prop = mean_monthly_fixes / mean_monthly_fixes, 
    total_prop = mean_monthly_fixes / total_fixes
  )

summarised <- grouped_sums %>%
  group_by(month) %>%
  mutate(
    total_birds_released = max(total_birds_released), 
    summ_mean_birds = mean(mean_birds), 
    sd_birds = sd(mean_birds), 
    low_birds = min(mean_birds), 
    high_birds = max(mean_birds), 
    summ_mean_monthly_fixes = mean(mean_monthly_fixes), 
    sd_fixes = sd(mean_monthly_fixes), 
    min_fixes = min(mean_monthly_fixes), 
    max_fixes = max(mean_monthly_fixes), 
    mean_monthly_prop = mean(monthly_prop), 
    sd_monthly_prop = sd(monthly_prop), 
    low_monthly_prop = min(monthly_prop), 
    high_monthly_prop = max(monthly_prop), 
    summ_birdhours_in_PA = mean(birdhours_in_PA), 
    sd_birdhours_in_PA = sd(birdhours_in_PA), 
    low_birdhours_in_PA = min(birdhours_in_PA), 
    high_birdhours_in_PA = max(birdhours_in_PA)
  ) %>%
  select(-mean_birds, -mean_monthly_fixes, -birdhours_in_PA) %>%
  rename(
    mean_birds = summ_mean_birds, 
    mean_monthly_fixes= summ_mean_monthly_fixes,
    birdhours_in_PA = summ_birdhours_in_PA
  ) %>%
  distinct(month, .keep_all = T) %>%
  select(-id_group) %>%
  ungroup()

saveRDS(summarised, "../Outputs/Simulations/Combined simulations/summarised sim data.rds")

