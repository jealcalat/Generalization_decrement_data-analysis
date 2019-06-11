## Perform k-medoids of pooled data with the wrapper function
# bp_km_wrap.r

pooled_ind_ana_km_df <- function(list_ss, 
                                 paths, 
                                 sessions, 
                                 max_trials){  
  
  baseline <- bp_km_wrap(list_Ss = list_ss, 
                         sessions = sessions,
                         path_read = paths[1],
                         rp_index = c(1, 11))
  
  baseline$phase <- "training"
  
  experimental <- bp_km_wrap(list_Ss = list_ss, 
                             sessions = sessions,
                             path_read = paths[2],
                             rp_index = c(1, 11))
  
  experimental$phase <- "aco"
  
  experimental$trial <- 1
  experimental <- experimental %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  baseline$trial <- 1
  
  baseline <- baseline %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  booleantrial <- baseline$cum_trial <= max_trials
  baseline  <- baseline[booleantrial,]
  booleantrial <- experimental$cum_trial <= max_trials
  experimental       <- experimental[booleantrial, ]
  
  pool_BL_EXP  <- bind_rows(experimental, baseline)
  
  pool_BL_EXP 
}
