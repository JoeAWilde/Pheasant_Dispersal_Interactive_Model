id_sim <- function(id, sl_pars, ta_pars, ssf_betas, cov_names, pen_pts, dogin_dates, dogin_times, 
                   dogin_prob, dogin_buffer, dogin_outside_edge, covs, wood_rast, Autmort, Wintmort, Springmort, 
                   st_date, n_IDs, n_steps, n_csteps, fix_rate, stop_if_left, suntimes, short_list, 
                   hedges_edges, hedges_edges_dist) {
  if(!require("tidyterra")) install.packages("tidyterra")
  if(!require("tidyverse")) install.packages("tidyverse")
  if(!require("sf")) install.packages("sf")
  if(!require("hms")) install.packages("hms")
  if(!require("Rfast")) install.packages("Rfast")
  if(!require("boot")) install.packages("boot")
  require(tidyterra)
  require(tidyverse)
  require(sf)
  require(hms)
  require(Rfast)
  require(boot)
  source("../R_scripts/Functions/kde_woodland.R")
  
  if(short_list) {
    urban_value <- 10
  } else {
    urban_value <- 20
  }
  
  hab_betas <- ssf_betas[which(grepl("HAB", toupper(names(ssf_betas))))]
  
  names(hab_betas) <- substr(names(hab_betas), 4, 6)
  
  feed_index <- which(grepl("FEED", toupper(names(ssf_betas))))
  pen_index <- which(grepl("PEN", toupper(names(ssf_betas))) & 
                       !grepl(":", toupper(names(ssf_betas))))
  hedge_index <- which(grepl("HEDGES", toupper(names(ssf_betas))))
  field_edges_index <- which(grepl("FIELD_EDGES", toupper(names(ssf_betas))))
  int_index <- which(grepl(":", toupper(names(ssf_betas))))
  sl_index <- which(grepl("SL_", toupper(names(ssf_betas))) & 
                      !grepl("LOG", toupper(names(ssf_betas))))
  log_sl_index <- which(grepl("LOG_SL", toupper(names(ssf_betas))))
  cos_ta_index <- which(grepl("COS_TA", toupper(names(ssf_betas))))
  
  df_id <- data.frame(
    id = rep_len(id, n_steps), 
    x = rep_len(0, n_steps), 
    y = rep_len(0, n_steps), 
    absta = rep_len(0, n_steps), 
    ta_ = rep_len(0, n_steps), 
    sl_ = rep_len(0, n_steps), 
    BoundaryHit = rep_len(F, n_steps), 
    DateTime = seq(from = st_date, 
                   to = st_date + minutes(fix_rate * (n_steps - 1)), 
                   length.out = n_steps), 
    SinceRel = seq(0, (n_steps - 1) * fix_rate, fix_rate), 
    DayNight = rep_len(0, n_steps), 
    DogIn = rep_len(F, n_steps), 
    BirdDead = rep_len(0, n_steps)
  ) %>%
    mutate(
      DateTime = if_else(format(DateTime, "%H:%M:%S") == "00:00:00", DateTime + seconds(1), DateTime), 
      DaysSinceRel = SinceRel / 1440, 
      DogIn = if_else(
        date(DateTime) %in% dogin_dates & 
          (as_hms(format(ymd_hms(DateTime), format = "%H:%M:%S")) >= dogin_times[1] & 
             as_hms(format(ymd_hms(DateTime), format = "%H:%M:%S")) <= dogin_times[2]), 
        TRUE, FALSE), 
      DayNight = if_else((DateTime >= suntimes$sunset & 
                            as_hms(DateTime) <= as_hms("23:59:59")) | 
                           (as_hms(DateTime) >= as_hms("00:00:00") &
                              DateTime <= suntimes$sunrise), 
                         "NIGHT",
                         "DAY")
    )
  
  df_id$x[1] <- st_coordinates(pen_pts)[, 1]
  df_id$y[1] <- st_coordinates(pen_pts)[, 2]
  
  is_within_raster <- T
  
  for(t in 2 : nrow(df_id)) {
    if(date(df_id$DateTime[t]) != date(df_id$DateTime[t - 1])) {
      if(date(df_id$DateTime[t]) %in% Autmort$Autdays) {
        death_prob <- Autmort$Autdaily
      } else if(date(df_id$DateTime[t]) %in% Wintmort$Wintdays) {
        death_prob <- Wintmort$Wintdaily
      } else if(date(df_id$DateTime[t]) %in% Springmort$Springdays) {
        death_prob <- Springmort$Springdaily
      }
      
      bird_dead <- sample(c(T, F), 1, prob = c(death_prob, 1 - death_prob))
      
      if(bird_dead | df_id$BoundaryHit[t-1]) {
        df_id$BirdDead[t : nrow(df_id)] <- 1
        break
      }
    }
    
    prev_point <- st_sfc(st_point(c(df_id$x[t - 1], df_id$y[t - 1])), crs = st_crs(dogin_buffer))
    in_buffer <- st_intersects(prev_point, dogin_buffer, sparse = F)[1, 1]
    
    if(df_id$DogIn[t] & in_buffer) {
      dog_into_pen <- sample(c(T, F), 1, prob = c(dogin_prob, 1 - dogin_prob))
      
      if(!dog_into_pen) {
        point_t <- st_nearest_points(prev_point, dogin_outside_edge)
      } else {
        point_t <- pen_pts
      }
      
      
      
      df_id$x[t] <- st_coordinates(point_t)[1, 1]
      df_id$y[t] <- st_coordinates(point_t)[1, 2]
      print(atan2(df_id$y[t] - df_id$y[t-1], df_id$x[t] - df_id$x[t-1]))
      df_id$absta[t] <- atan2(df_id$y[t] - df_id$y[t-1], df_id$x[t] - df_id$x[t-1])
      df_id$ta_[t] <- df_id$absta[t] - df_id$ta_[t-1]
      df_id$sl_[t] <- sqrt(((df_id$x[t] - df_id$x[t - 1])^2) + ((df_id$y[t] - df_id$y[t - 1])^2))
      
    } else {
      if(df_id$DayNight[t] == "NIGHT") {
        if(df_id$DayNight[t-1] == "DAY") {
          if(month(df_id$DateTime[t]) %in% 7:8) {
            kde_50_woodland <- kde_woodland(df_id[1:(t-1),] %>%
                                              filter(month(DateTime) == month(st_date) &
                                                       DayNight == "DAY"), wood_rast, 0.25)
          } else {
            kde_50_woodland <- kde_woodland(df_id[1:(t-1),] %>%
                                              filter(DayNight == "DAY"), wood_rast, 0.5)
            
          }
        }
        in_woods_value <- extract(kde_50_woodland, as.matrix(cbind(df_id$x[t - 1], df_id$y[t - 1])))[1, 1]
        in_woods_value <- ifelse(is.na(in_woods_value), 0, 1)
        
        in_woods <- ifelse(in_woods_value %in% c(1, 33), TRUE, FALSE)
        
        if(!in_woods) {
          control_steps_df <- data.frame(
            sl_ = rgamma(n_csteps, sl_pars$Gam_shape, sl_pars$Gam_shape / exp(sl_pars$Intercept)), 
            ta_ = as.numeric(rvonmises(n_csteps, ta_pars$vm_mu, ta_pars$vm_kappa))) %>%
            mutate(absta_ = (ta_ + df_id$absta[t - 1]) %% (2 * pi), 
                   x = df_id$x[t - 1] + (sl_ * cos(absta_)), 
                   y = df_id$y[t - 1] + (sl_ * sin(absta_))) %>%
            cbind(., extract(kde_50_woodland, as.matrix(cbind(.$x, .$y)))) %>%
            rename(dist = ncol(.)) %>%
            select(-layer)
          
          if(all(is.na(control_steps_df$dist))) {
            control_steps_df <- data.frame(
              sl_ = rgamma(n_csteps, sl_pars$Gam_shape, sl_pars$Gam_shape / exp(sl_pars$Intercept)), 
              ta_ = as.numeric(rvonmises(n_csteps, ta_pars$vm_mu, ta_pars$vm_kappa))) %>%
              mutate(absta_ = (ta_ + df_id$absta[t - 1]) %% (2 * pi), 
                     x = df_id$x[t - 1] + (sl_ * cos(absta_)), 
                     y = df_id$y[t - 1] + (sl_ * sin(absta_))) %>%
              cbind(., extract(covs, as.matrix(cbind(.$x, .$y)))) %>%
              rename(
                feed = 6, 
                hab = 7, 
                wood = 8, 
                pen = 9, 
                hedges = 10, 
                field_edges = 11
              ) %>%
              select(-c(feed, hab, pen, hedges, field_edges)) %>%
              rename(dist = wood)
            
          }
          
          min_kde_wood_dist <- which(control_steps_df$dist == min(control_steps_df$dist))
          
          if(length(min_kde_wood_dist) > 1) {
            control_steps_df <- control_steps_df[min_kde_wood_dist,]
            min_kde_wood_dist <- which(control_steps_df$sl_ == max(control_steps_df$sl_))
          }
          
          selected_index <- min_kde_wood_dist
          
          df_id$x[t] <- control_steps_df$x[selected_index]
          df_id$y[t] <- control_steps_df$y[selected_index]
          df_id$absta[t] <- control_steps_df$absta_[selected_index]
          df_id$ta_[t] <- control_steps_df$ta_[selected_index]
          df_id$sl_[t] <- control_steps_df$sl_[selected_index]
        } else {
          df_id$x[t] <- df_id$x[t-1]
          df_id$y[t] <- df_id$y[t-1]
        }
        
        
        if(any(is.na(df_id[t,]))) {
          in_within_raster <- F
        }
        
        if(!is_within_raster) {
          df_id$BoundaryHit[t] <- T
        }
      } else {
        gam_eta <- sl_pars$Intercept
        
        gam_mu <- exp(gam_eta)
        
        control_steps_df <- data.frame(
          sl_ = rgamma(n_csteps, sl_pars$Gam_shape, sl_pars$Gam_shape / gam_mu), 
          ta_ = as.numeric(rvonmises(n_csteps, ta_pars$vm_mu, ta_pars$vm_kappa))) %>%
          mutate(absta_ = (ta_ + df_id$absta[t - 1]) %% (2 * pi), 
                 x = df_id$x[t - 1] + (sl_ * cos(absta_)), 
                 y = df_id$y[t - 1] + (sl_ * sin(absta_))) %>%
          cbind(., terra::extract(covs, cbind(.$x, .$y))) %>%
          na.omit()
        
        if(nrow(control_steps_df) == 0) {
          df_id$BoundaryHit[t] <- T
          break
        }
        names(control_steps_df)[6:11] <- c("feed", "hab", "wood", "pen", "hedges", "field_edges")
        
        control_steps_df <- control_steps_df %>%
          filter(
            !hab %in% 7:9 
          )
        
        while(nrow(control_steps_df) == 0) {
          control_steps_df <- data.frame(
            sl_ = rgamma(n_csteps, sl_pars$Gam_shape, sl_pars$Gam_shape / gam_mu), 
            ta_ = as.numeric(rvonmises(n_csteps, ta_pars$vm_mu, ta_pars$vm_kappa))) %>%
            mutate(absta_ = (ta_ + df_id$absta[t - 1]) %% (2 * pi), 
                   x = df_id$x[t - 1] + (sl_ * cos(absta_)), 
                   y = df_id$y[t - 1] + (sl_ * sin(absta_))) %>%
            cbind(., terra::extract(covs, cbind(.$x, .$y))) %>%
            na.omit()
        }
        
        if(month(df_id$DateTime[t]) %in% 7:8) {
          control_steps_df <- control_steps_df %>%
            mutate(log_step_weight = pen * ssf_betas[feed_index] + 
                     pen * ssf_betas[pen_index] + 
                     hedges * ssf_betas[hedge_index] + 
                     field_edges * ssf_betas[field_edges_index] + 
                     sl_ * ssf_betas[sl_index] + 
                     log(sl_) * ssf_betas[log_sl_index] + 
                     cos(ta_) * ssf_betas[cos_ta_index] + 
                     pen * df_id$DaysSinceRel[t] * ssf_betas[int_index])
          
        } else {
          control_steps_df <- control_steps_df %>%
            mutate(log_step_weight = feed * ssf_betas[feed_index] + 
                     pen * ssf_betas[pen_index] + 
                     sl_ * ssf_betas[sl_index] + 
                     log(sl_) * ssf_betas[log_sl_index] + 
                     cos(ta_) * ssf_betas[cos_ta_index] + 
                     pen * df_id$DaysSinceRel[t] * ssf_betas[int_index])
        }
        
        
        for(i in 1 : nrow(control_steps_df)) {
          if(as.integer(control_steps_df$hab[i]) != 1) {
            control_steps_df$log_step_weight[i] <- control_steps_df$log_step_weight[i] + 
              hab_betas[which(as.character(control_steps_df$hab[i]) == names(hab_betas))]
          }
        }
        
        control_steps_df$step_weight <- exp(control_steps_df$log_step_weight)
        control_steps_df$step_prob <- control_steps_df$step_weight / sum(control_steps_df$step_weight)
        
        
        selected_index <- sample(1:nrow(control_steps_df), 1, prob = control_steps_df$step_prob)
        
        df_id$x[t] <- control_steps_df$x[selected_index]
        df_id$y[t] <- control_steps_df$y[selected_index]
        df_id$absta[t] <- control_steps_df$absta_[selected_index]
        df_id$ta_[t] <- control_steps_df$ta_[selected_index]
        df_id$sl_[t] <- control_steps_df$sl_[selected_index]
        
        if(any(is.na(df_id[t,]))) {
          in_within_raster <- F
        }
        
        if(!is_within_raster) {
          df_id$BoundaryHit[t] <- T
        }
      }
    }
  }
  
  dead_index <- which(df_id$birddead == 1 | df_id$BoundaryHit == T)[1]
  
  if(length(dead_index) > 0 & !is.na(dead_index)) {
    df_id <- df_id[1:(dead_index-1), ]
  }
  
  df_id <- subset(df_id, BirdDead != 1)
  return(df_id)
}



# ggplot() + 
#   geom_spatraster(data = as.factor(hab), alpha = 0.4) + 
#   geom_path(data = df_id[df_id$x!=0,], aes(x = x, y = y, colour = SinceRel), linewidth = 3) + 
#   scale_x_continuous(limits = c(min(df_id[df_id$x!=0,]$x) - 100, 
#                                 max(df_id[df_id$x!=0,]$x) + 100)) + 
#   scale_y_continuous(limits = c(min(df_id[df_id$x!=0,]$y) - 100, 
#                                 max(df_id[df_id$x!=0,]$y) + 100))
