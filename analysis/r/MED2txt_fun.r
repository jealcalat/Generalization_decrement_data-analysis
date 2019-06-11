MED2txt = function(SS_list,path_read,path_save){
  setwd(path_read)
  
  # ==============================================================================
  # Get a data.frame of subjects information:
  # - day of session
  # - number of session corresponding to day
  # - actual name of subject, starting with M#
  SS_list = SS_list
  df_ids <- lapply(SS_list, function(i) {
    
    f <- list.files(pattern = paste0("2(.*)M",i), recursive = F)
    
    id_df_su = lapply(f,function(x){
      print(x)
      info_x = read.table(x, nrow = 3,skip = 2, na.strings = "NA", fill = TRUE)
      
      sujeto = as.character(info_x[3,2]) 
      
      if(sujeto %in% as.character(326:333)){
        sujeto <- paste0("M",sujeto)
      }
      
      sesion = as.Date(info_x[1,3], format="%m/%d/%y")
      
      id_df_su = data.frame(sujeto = sujeto,
                            sesion = sesion)
      return(id_df_su)
    })
    id_df_su = bind_rows(id_df_su)
    
    setDT(id_df_su)[, sesion_id := rleid(sesion)]
    
    return(id_df_su)
  }) %>% bind_rows()
  
  # Next is to read MED files and write them in txt, separated with ";"
  
  #dfList <- 
    lapply(SS_list, function(i) {
    
    f = list.files(pattern = paste0("2(.*)M",i), recursive = F)
    
    # df_su = 
      lapply(f, function(x){
      print(x)
      dfx = read.table(x, skip = 3, na.strings = "NA", fill = T,
                       col.names = paste0("V",seq_len(6))) 
      
      if(is.null(dfx)){
        print(x)
        stop("File is empty.")
      }
      sujeto = as.character(dfx[3,2])
      
      if(sujeto %in% as.character(326:333)){
        sujeto <- paste0("M",sujeto)
      }
      
      sesion = as.Date(dfx[1,3], format="%m/%d/%y") 
      sesion_id = df_ids$sesion_id[df_ids$sujeto == sujeto & 
                                     df_ids$sesion == sesion]
      
      if(length(sesion_id)>1){
        print(x)
        print(sesion_id)
        print(sesion)
        stop("Session value should be unique.")
      }
      
      a = which((dfx$V1=="0:")==1)
      
      varC = slice(dfx,a[1]:(a[2]-2))
      
      if(length(varC[1,]) != 6){
        print(x)
        stop("Dim of data.frame should < 6") # by design
      }
      
      varC[1] = NULL
      # Evaluate if events were incorrectly loaded as factors or characters
      for (j in 1:length(varC[1, ])) {
        if(is.factor(varC[[j]])){
          varC[ ,j] = as.numeric(as.character(varC[[j]]))
        } else {
          varC[ ,j] = as.numeric(varC[[j]])
        }
      }
      
      varC = na.omit(stack(varC)) 
      
      varC[2] = NULL
      # There were some errors in MED files that changed events IDs,
      # for example, event 1 turned to 100. This is to fixe them
      vec_compare = c(100,110,120,200,210,500,510,700,800)
      vec_replace = c(1,11,12,2,21,5,51,7,8)
      
      m = matrix(c(vec_compare,vec_replace),ncol = 2)
      
      for(k in 1:nrow(m)){
        bool = varC$values %in% m[k,1]
        varC$values[bool] = m[k,2]
      }
      
      varC = data.frame(val=sort(varC$values))
      
      varC_ev = varC %>% 
        mutate(sujeto = sujeto,
               fase = "acq",
               sesion = sesion_id) %>% 
        separate(val, c("t","evento"), sep="\\.",convert = T) %>% 
        as.data.frame()
      
      write.table(varC_ev,
                  file = paste0(path_save,"SM", i,"S",sesion_id,".txt"),
                  sep = ";", row.names=FALSE)
      
     # return(varC_ev)
      
    }) # Second lapply
    
    # df_su = bind_rows(df_su)
    # 
    # return(df_su)
    
  } ) # %>% bind_rows()
  
}
