# read_sep_by_lever.R 
# Input: File name from last_n_ses.R and peak where 1 = get just
#        peak trials and 0 = get all trials. 
# Output: Single dataframe of SubjectSesion with 8 columns


read_sep_by_lever <- function(sj,peak){
  # subject = sj # uncomment to use subject to identify errors
  sj <- read.table(sj,sep = ";",header = T,stringsAsFactors = F)
  
  if(!is_empty(sj)){
    
    if(sum(sj[[2]] %in% c(53,54,5,51), na.rm = T)){
      
      sj$t <- sj$t/10 # t to seconds
      sj$dt <- c(0,diff(sj$t)) # delta.t (time between events)
      
      # 1 if one of evs events ocurred, 0 if not
      evs = c(5,3,53,33,54,34,51,31)
      sj$mark.v <- ifelse(sj$evento %in% evs,1,0)
      
      # This create a identifier if a trial or end of trial ocurrend,
      # for peak trials and IF trials, for the two components
      sj$cum_id <- cumsum(sj$mark.v)
      # How many reiforcers ocurred?
      t_ref    <- sum(sj$evento %in% c(21,2))
      # Create an index of the last reinforcer
      i <- which(cumsum(sj$evento == 2 | sj$evento == 21) >= t_ref)[1]
      # Cut data frame from the first row to last reinforcer
      sj <- sj[1:i, ]
      
      # Columns to be used
      select_columns = c("t","evento","sujeto","sesion","dt","cum_id")
      
      # select right: if peak = 1, event = 53, else all "right"
      if(peak == 1){
        
        derecha <- sj[select_columns] %>% 
          group_by(cum_id) %>%  
          filter(cumsum(evento == 53)!=0) %>% 
          as.data.frame()
        
      } else {
        derecha <- sj[select_columns] %>% 
          group_by(cum_id) %>%  
          filter(cumsum(evento==5)!=0) %>% 
          as.data.frame()
      }
      
      if(!is_empty(derecha[[1]])){
        # Check if peak resp has ID 13 or not
        # else fue codificada como 1, avanzar
        # # Not necessary for experimental condition
        
        if (sum(sj$evento == 13) > 0){ 
          derecha$cum <- cumsum(derecha$evento==13)
        } else {                   
          derecha$cum <- cumsum(derecha$evento==1)
        }
        # Create a timer with the delta t
        derecha$t <- cumsum(derecha$dt)
        # identifier for component tandem
        derecha$cde <- 1
      }
      # Do the same as above, but for component Yoked
      if(peak == 1){
        izquierda <- sj[select_columns] %>% 
          group_by(cum_id) %>% 
          filter(cumsum(evento==54)!=0) %>% 
          as.data.frame()
      } else {
        izquierda <- sj[select_columns] %>% 
          group_by(cum_id) %>% 
          filter(cumsum(evento==51) != 0) %>% 
          as.data.frame()
      }
      
      if(!is_empty(izquierda[[1]])){
        if (sum(sj$evento == 14) > 0){
          izquierda$cum <- cumsum(izquierda$evento==14)
        } else { 
          izquierda$cum <- cumsum(izquierda$evento==11)
        }
        
        izquierda$t <- cumsum(izquierda$dt)
        
        izquierda$cde <- 2 
      }
      
      
      # Join them and verify they are not empty, otherwise, return
      # the nonempty data frame. (This could happen when, say, ocurred
      # just tandem trials).
      
      if(!is_empty(derecha[[1]]) & !is_empty(izquierda[[1]])){
        
        iz_der <- bind_rows(izquierda,derecha)
        
      } else if(is_empty(derecha[[1]])){
        
        iz_der <- izquierda
        
      } else {
        
        iz_der <- derecha
      }
      # print(subject) # uncomment to identify df with errors
      
      # Create a column of trials for all session, by component
      
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
}  # end function 
