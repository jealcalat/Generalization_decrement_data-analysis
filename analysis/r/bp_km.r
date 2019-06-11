

bp_km <- function(r_times){
  
  # get a 2x2 df with #resp per second
  res <- 1
  x <- seq(1,180,res)
  resp_sec <- r_times %>% 
  {
    bins <- .
    rate <- f_table(bins,1,180,res) # f_table is a custom fn
    rate
  }
  
  resp_sec <- data.frame(x = x, y = resp_sec)
  resp_sec_scaled <- apply(resp_sec, 2, scale) # clustering works better scaled
  colnames(resp_sec_scaled) <- c("x","y")
  set.seed(123)
  
  gap <- clusGap(resp_sec_scaled,pam,K.max = 4, B = 100) # gap statistic
  # to compute the optimal number of clusters btw 1 and 4
  k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")

  data.frame(opt_k = k)
} 
