# resp_times.r
# Get the times at every response for whole data frame


resp_times <- function(df,n_trials){
  
  df <- df %>% filter(cum_trial <= n_trials, 
                      cum_dt <= 180)
  
  df$bins <- df$cum_dt %>% 
    get_bins(., 0, 180, 1)

  df <- df %>%
  {
    var     <- . 
    event   <- var %>% `[[`("evento")
    boolean <- ifelse(event == 1 | event == 11,1,0) %>% as.logical()
    var     <- var[boolean, ]
    var
  }
  
 df %>% select(sujeto,evento,sesion,cde,bins,cum_trial)
  
}

