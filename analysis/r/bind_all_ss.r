# bind_all_ss.R 
# This function binds all dfs by subject session ids returned by 
# read_sep_by_lever.R
# Input: list_Ss: a sequence of subjects' number (e.g., 325,326,327...)
#        n: sessions to analyze (last n)
#        peak: data from peak trials? 0 = no, 1 = yes
#        path_read: directory of files
# Output: large df with all Ss and sessions

bind_all_Ss = function(list_Ss,
                       n,
                       peak,
                       path_read){
  
  # Get list of file names to read
  last_n_list = last_n_ses(list_Ss = list_Ss,n = n, 
                           path_read = path_read)
  
  all_df = lapply(last_n_list,function(k){
    # read and separete by component
    read_sep_by_lever(k,peak)
  })
  # add data.frame j by the end of the i data.frame
  all_df = bind_rows(all_df) 
  # cumulative trials by session and component
  # arranged in desc order of session
  all_df = all_df %>% 
    group_by(sujeto,cde) %>% 
    arrange(desc(sesion)) %>% 
    mutate(cum_trial = rleid(trial))
  # return df
  all_df
}
