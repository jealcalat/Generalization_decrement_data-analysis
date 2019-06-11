ov_rate = function(lss, n=40, path){
  
  lst_fi = last_n_ses(list_Ss=lss, n=n, path_read=path)
  
  all_df = lapply(lst_fi, function(k){
    
    sj <- read.table(k, sep=";", header=T, stringsAsFactors=F)
    
    if(!is_empty(sj)){
      
      if(sum(sj[[2]] %in% c(53,54,5,51), na.rm = T)){
        
        sj$t <- sj$t/10 # t to seconds
        sj$dt <- c(0,diff(sj$t)) # delta.t (time between events)

        evs = c(5,3,53,33,54,34,51,31)
        sj$mark.v <- ifelse(sj$evento %in% evs,1,0)
        sj$cum_id <- cumsum(sj$mark.v)
        t_ref    <- sum(sj$evento %in% c(21,2))
        i <- which(cumsum(sj$evento == 2 | sj$evento == 21) >= t_ref)[1]
        sj <- sj[1:i, ]
        select_columns = c("t","evento","sujeto","sesion","dt","cum_id")
        
        derecha <- sj[select_columns] %>% 
          group_by(cum_id) %>%  
          filter(cumsum(evento==5) != 0 | cumsum(evento == 53)!=0) %>% 
          as.data.frame()
        
        if(!is_empty(derecha[[1]])){

          if (sum(sj$evento == 13) > 0){ 
            derecha$cum <- cumsum(derecha$evento==13)
          } else {                   
            derecha$cum <- cumsum(derecha$evento==1)
          }
          derecha$t <- cumsum(derecha$dt)
          derecha$cde <- 1
        }
        
        izquierda <- sj[select_columns] %>% 
          group_by(cum_id) %>% 
          filter(cumsum(evento==51)!=0 | cumsum(evento==54)!=0) %>% 
          as.data.frame()
        
        if(!is_empty(izquierda[[1]])){
          if (sum(sj$evento == 14) > 0){
            izquierda$cum <- cumsum(izquierda$evento==14)
          } else { 
            izquierda$cum <- cumsum(izquierda$evento==11)
          }
          
          izquierda$t <- cumsum(izquierda$dt)
          izquierda$cde <- 2 
        }
        

        if(!is_empty(derecha[[1]]) & !is_empty(izquierda[[1]])){
          iz_der <- bind_rows(izquierda,derecha)
          
        } else if(is_empty(derecha[[1]])){
          iz_der <- izquierda
          
        } else {
          iz_der <- derecha
        }
 
        if(!is_empty(iz_der[[1]])){
          
          setDT(iz_der)[, trial := rleid(cum_id),by=.(cde)]
          setDT(iz_der)[, cum_dt := cumsum(dt) - cumsum(dt)[1],by = .(trial,cde)] 
          
          iz_der <- as.data.frame(iz_der)
          iz_der['cum_id'] <- NULL
        }
        
      } # end if of events
    } # end if of empy file name
    if(length(iz_der$cum) > 6){ # is there at least 6 responses?
      iz_der # return iz_der
    }
  })  %>% bind_rows() # Convert list to data frame
  
  all_df # Return data frame
}
  