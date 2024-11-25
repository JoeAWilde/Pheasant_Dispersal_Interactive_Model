library(tidyverse)
library(progress)
library(doSNOW)
library(sf)
# library(plyr)

root <- "outputs/PA sites/script_5/"

site_prefix <- "As"

sites <- paste0(site_prefix, c(0, 250, 500, 1000, 2000))

# for(ss in sites) {
#   if(ss == sites[1]) pb <- progress_bar$new(total = length(sites),
#                                             format = "[:bar] :percent eta::eta",
#                                             clear = F); pb$tick(0)
#   
#   sim_files <- paste0(root, list.files(root)) %>%
#     .[grepl(ss, .)]
#   
#   all_df <- lapply(sim_files, readRDS) %>%
#     do.call(rbind, .)
#   saveRDS(all_df, paste0("outputs/PA sites/script_6/", ss, "simulation_data.rds"))
#   pb$tick()
# }

for(ss in sites) {
  cen_pen <- st_read(paste0("outputs/PA sites/script_3/", ss, "_pen_shapefile.shp")) %>%
    st_centroid() %>%
    st_coordinates()
  
  all_sites_df <- readRDS(paste0("outputs/PA sites/script_6/", ss, "simulation_data.rds"))
  
  PAs <- st_read("data/Protected Areas/All_UK_PAs.shp") %>%
    st_transform(crs = "EPSG:27700") %>%
    st_crop(., st_buffer(st_as_sf(all_sites_df[1,], coords = c("x", "y"), crs = "EPSG:27700"), dist = 10000))
  
  sim_df <- all_sites_df %>% 
    group_by(id) %>%
    mutate(
      dist_from_pen = sqrt(((x - cen_pen[1, 1])^2) + ((y - cen_pen[1, 2])^2)), 
      nearest_pen_band = plyr::round_any(dist_from_pen, 250, f = floor),
      sl_ = sqrt(((x - lag(x))^2 + (y - lag(y))^2))) %>%
    ungroup() %>%
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
      dist_from_pen_band = rep_len(seq(0, 2000, 250), n())
    ) %>%
    ungroup() %>%
    distinct(site, month, id_group, total_birds_released, mean_birds, mean_monthly_fixes,
             dist_from_pen_band, total_fixes) 
  
  
  cl <- makeCluster(parallel::detectCores(logical = F)-1, type = "SOCK")
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

    mean_fixes = nrow(
      sim_df[sim_df$id_group == grouped_sums$id_group[i] &
               sim_df$nearest_pen_band == grouped_sums$dist_from_pen_band[i] &
               sim_df$month == grouped_sums$month[i], ]
    )

    birdhours_in_PA = length(which((lengths(st_within(
      st_as_sf(sim_df[sim_df$id_group == grouped_sums$id_group[i] &
                        sim_df$month == grouped_sums$month[i], ],
               coords = c("x", "y"), crs = "EPSG:27700"), PAs)) > 0) == T))

    saveRDS(c(i, mean_fixes, birdhours_in_PA), paste0("outputs/PA sites/script_6/summarised data loop output/",
                                                      ss, "_meanfix_birdhours_", i, ".rds"))

    pb$tick()
  }

  stopCluster(cl)
  mf_bh_root <- "outputs/PA sites/script_6/summarised data loop output/"
  mf_bh_files <- paste0(mf_bh_root, list.files(mf_bh_root)) %>%
    .[grepl(ss, .) & grepl("meanfix", .)]
  
  i_mf_bh <- lapply(mf_bh_files, readRDS) %>%
    do.call(rbind, .) %>%
    data.frame(.) %>%
    rename(i = 1, 
           mean_fixes = 2, 
           birdhours_in_PA = 3) %>%
    mutate(i = as.integer(i)) %>%
    arrange(i)
  
  grouped_sums <- cbind(grouped_sums, i_mf_bh %>% select(-i)) %>%
    mutate(
      prop_birds_alive = mean_birds / total_birds_released, 
      monthly_prop = mean_fixes / mean_monthly_fixes, 
      total_prop = mean_fixes / total_fixes
    )
  
  summarised <- grouped_sums %>%
    group_by(dist_from_pen_band, month) %>%
    mutate(
      total_birds_released = max(total_birds_released), 
      summ_mean_birds = mean(mean_birds), 
      sd_birds = sd(mean_birds), 
      low_birds = min(mean_birds), 
      high_birds = max(mean_birds), 
      summ_mean_fixes = mean(mean_fixes), 
      sd_fixes = sd(mean_fixes), 
      min_fixes = min(mean_fixes), 
      max_fixes = max(mean_fixes), 
      mean_monthly_prop = mean(monthly_prop), 
      sd_monthly_prop = sd(monthly_prop), 
      low_monthly_prop = min(monthly_prop), 
      high_monthly_prop = max(monthly_prop), 
      summ_birdhours_in_PA = mean(birdhours_in_PA), 
      sd_birdhours_in_PA = sd(birdhours_in_PA), 
      low_birdhours_in_PA = min(birdhours_in_PA), 
      high_birdhours_in_PA = max(birdhours_in_PA)
    ) %>%
    select(-mean_birds, -mean_fixes, -birdhours_in_PA) %>%
    rename(
      mean_birds = summ_mean_birds, 
      mean_fixes= summ_mean_fixes,
      birdhours_in_PA = summ_birdhours_in_PA
    ) %>%
    distinct(month, dist_from_pen_band, .keep_all = T) %>%
    select(-id_group) %>%
    ungroup()
  
  saveRDS(summarised,
          paste0("outputs/PA sites/script_6/summarised data/", ss, " summarised data.rds"))
}

