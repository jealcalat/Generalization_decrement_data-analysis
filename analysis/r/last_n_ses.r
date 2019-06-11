# last_n_ses.R
# Function that takes the files names of last n sessions (e.g., n = 6)
# Its outputs will be a list to iterate over

last_n_ses = function(list_Ss, n, path_read){
  sessions = lapply(list_Ss,function(x){
    su = paste0("SM",x)
    # Set directory to read files names
    setwd(path_read)
    
    sj = list.files(pattern = paste0("^",su,"(.*).txt"),recursive = T)
    
    # verify if n > length(sj)
    if(n > length(sj)){
      n = length(sj)
      sprintf("Input n is greater than %1.f, so %1.f",n,n)
    }
    # a <- first session
    a = length(sj) - n + 1
    # Vector from a to n session
    b = seq(a,length(sj)) 
    # Return list of sessions
    files_names = lapply(b, function(i){
      fns = paste0(su,"S",i,".txt")
    })
    
    files_names = unlist(files_names)
  })
  sessions = unlist(sessions)
  return(sessions)
}
