# lhl_wrapper.r


bp_fi_wrp <- function(list_Ss,
                      sessions,
                      path_read,
                      rp_index){
  
  last_n_list <- last_n_ses(list_Ss=list_Ss, 
                            n=sessions, 
                            path_read=path_read) 
  
  lapply(last_n_list, function(k){
    
    df_2 <- read_sep_by_lever(k,0) %>% as.data.frame()
    
    if (!is_empty(df_2)) {
      
      df_2 <- df_2[df_2$evento %in% rp_index, ]
      # print(k)
      
      cde <- unique(df_2$cde)
      subj <- unique(df_2$sujeto)
      sess <- unique(df_2$sesion)
      met2 <- data.frame()
      tcheck <- unique(df_2$trial)
      
      if (length(tcheck) > 0) {
        
        if (length(cde) > 0) {
          
          for (l in cde) {
            
            l <- cde[l]
            df_3 <- df_2[df_2$cde == l, ]
            trials <- unique(df_3$trial)
            
            for(j in 1:length(trials)){
              
              df_4 <- df_3[df_3$trial == j,]
              r_times <- df_4$cum_dt
              len_rt = length(r_times)
              
              if(len_rt > 3 & max(r_times) < 90){
                
                # print(paste(k,l,j)) # for debbuging
                
                met <- bp_fi(r_times)
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
}
