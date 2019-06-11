# lhl.r
# Church two step breakpoint algorithm, from Church et al, 1994.
# Author: Emmanuel Alcalá
# Date: 5 may 2019
# 
# ------------------------------------------------------------------------------
# This is an improved version of lhl_old. It is just marginally faster, but
# cleaner and more intuitive. It sets first all possible combinations of s1
# and s2 (start and stop), then it computes the differences between the overall
# rate r and the state rates r1, r2 and r3, weighted by their respective
# durations t1, t2 and t3. Then it computes the index (i.e., an integer of the
# position in the vector) at which the differences were maximized, with which
# all other metrics can be solved. 
# inputs:
#        - r_times: every time at which there was a response
#        - trial_duration: the duration of peal trial
# ------------------------------------------------------------------------------

lhl = function(r_times, trial_duration) {
  # Number of responses
  nr = length(r_times)
  # overall response rate
  r = nr/trial_duration 
  # Vector of putative starts (all time of responses except the last)
  s1 = r_times[1:(nr - 1)]
  # Vector of putative stops (except the first)
  s2 = r_times[2:nr]
  
  # Get all combinations of s1 and s2 with s1 less than s2.
  #   For example, if s1 can be 1, 2, 3, and s2 = 2, 3, 4, 
  #   the combinations are (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)
  grid = expand.grid.jc(s1,s2) # matrix of nx2; n = all possible combinations
  grid = grid[grid[ ,1] < grid[ ,2], ] # select n with s1 < s2
  # Vector of 0 to store results
  A = numeric(nrow(grid))
  # Computations of differences between the r and r(i), with i = 1, 2, 3
  # which are the rates at the low, high and low states. 
  # Store the results of the differences in A[j]
  for (j in 1:nrow(grid)) { # loop trough all combinations of s1 & s2
    t1 = grid[j, 1]
    t2 = grid[j, 2] - t1
    t3 = trial_duration - grid[j, 2]
    # Previous versions looked if some t equals 0 and assigned 0 to rates
    # to avoid division by 0, but isn' necessary: there are 0 responses
    # with duration 0!
    r1 = sum(r_times <= t1)/t1
    r2 = sum(r_times > t1 & r_times <= grid[j, 2])/t2
    r3 = sum(r_times > grid[j, 2])/t3

    A[j] = sum(t1*(r - r1), t2*(r2 - r), t3*(r - r3), na.rm = T)
    
  } 
  # Get the index which maximizes A
  argmaxA = which.max(A)
  # Vector of 2 at which A were maximum. column 1 is s1, 2 is s2
  s1s2 = grid[argmaxA, ]
  # High rate state duration: s2 - s1
  t2 = s1s2[2] - s1s2[1]
  # Low rate 1
  r1 = sum(r_times <= s1s2[1])/s1s2[1]
  # Low rate 2
  r3 = sum(r_times > s1s2[2])/(trial_duration - s1s2[2])
  # High rate
  hrate = sum(r_times > s1s2[1] & r_times <= s1s2[2])/t2
  # Middle time
  mid = sum(s1s2)/2
  # Return results
  data.frame(start = s1s2[1],
             stop = s1s2[2],
             spread = t2,
             hrate = hrate,
             mid = mid,
             r1 = r1,
             r3 = r3)
}
