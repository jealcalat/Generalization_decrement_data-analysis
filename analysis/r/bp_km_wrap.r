## bp_km_wrap.R
# Function wrapping of the bp_km

bp_km_wrap <- function(list_Ss, sessions, 
                       path_read, 
                       rp_index = c(1, 11)) {
  
  last_n_list <- last_n_ses(list_Ss = list_Ss, 
                            n = sessions, 
                            path_read = path_read) 
  
  df_1 <-  lapply(last_n_list, function(k){
    
    df_2 <- read_sep_by_lever(k, 1) %>% 
      as.data.frame()
    
    if (!is_empty(df_2)) {
      
      df_2 <- df_2[df_2$evento %in% rp_index, ]
      # print(k)
      df_2$bins <- get_bins(df_2$cum_dt, 1, 180, 1) 
      
      cde <- unique(df_2$cde)
      subj <- unique(df_2$sujeto)
      sess <- unique(df_2$sesion)
      met2 <- data.frame()
      met3 <- data.frame()
      
      tcheck <- unique(df_2$trial)
      
      if (length(tcheck) > 0) {
        if (length(cde) > 1) {
          for (l in cde) {
            l <- cde[l]
            df_3 <- df_2[df_2$cde == l, ]
            trials <- unique(df_3$trial)
            
            for (j in 1:length(trials)) {
              
              df_4 <- df_3[df_3$trial == j, ]
              r_times <- df_4$bins
              
              if (length(r_times) > 9) {
                print(paste(k,l,j)) # for debugging
                met <- bp_km(r_times)
                met$trial <- j
                met$cde <- l
                met$sujeto <- subj
                met$sesion <- sess
                
              } else {
                met <- data.frame()
              }
              met2 <- bind_rows(met, met2)
            }
          }
        }
        met2
      }
    }
  }) %>% bind_rows()
  df_1
}
