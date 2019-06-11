# ind_ana_lhl.r
# Uses lhl_wrapper as the wrapper of low-high-low function, based on the
# algorithm sugested by Church et al, 1994.
# Inputs
#       - list_ss: vector of subjects numbers
#       - sessions: number of sessions (actual number will be less)
#       - max_trials: maximum number of trials taken from sessions

ind_ana_lhl <- function(list_ss,
                       phases_names,
                       paths, 
                       sessions, 
                       max_trials,
                       trial_duration){  
  
  baseline <- lhl_wrapper(list_ss, sessions,
                          rp_index=c(1,11),
                          path_read = paths[1],
                          trial_duration)
  
  baseline$phase <- phases_names[1]
  baseline$trial <- 1
  baseline <- baseline %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  experimental <- lhl_wrapper(list_ss, sessions,
                              rp_index=c(1,11),
                              path_read = paths[2],
                              trial_duration)
  
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
