ind_ana_bp1 <- function(list_ss,
                        phases_names,
                        paths, 
                        sessions, 
                        max_trials){  
  
  baseline <- bp_fi_wrp(list_ss, sessions,
                        rp_index=c(1,11),
                        path_read = paths[1])
  
  baseline$phase <- phases_names[1]
  baseline$trial <- 1
  baseline <- baseline %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  experimental <- bp_fi_wrp(list_ss, sessions,
                            rp_index=c(1,11),
                            path_read = paths[2])
  
  experimental$phase <- phases_names[2]
  experimental$trial <- 1
  experimental<- experimental %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  boleantrial <- baseline$cum_trial <= max_trials
  baseline <- baseline[boleantrial, ]
  boleantrial <- experimental$cum_trial <= max_trials
  experimental      <- experimental[boleantrial, ]
  
  bind_rows(baseline,experimental)
  
}