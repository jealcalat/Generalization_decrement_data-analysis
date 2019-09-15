  LeT_peak_deg = function(tx,
                        tpeak,
                        prop_tr,
                        trials,
                        params,
                        wn_vector,
                        same_sd = F) {
  
  n = params[1]
  nvec = 1:n
  mu = params[2]
  sd = params[3]
  alpha = params[4]
  beta = params[5]
  W0 = params[6]
  p = params[8]
  
  tvector = replicate(trials, sample(c(1, 0), 1, prob = c(p, 1-p)))
  
  IRI1 = numeric(trials)
  IRI2 = numeric(trials)
  
  
  if (wn_vector[tx] == 0 ) { 
    wn_vector[1:n] = W0
    
  }
  
  theta = params[7]
  
  t1 = floor(tx * (1 - prop_tr))
  tf = tx - t1
  
  t1vec = 1:t1
  
  R1 = matrix(0, nrow = trials, ncol = tpeak*3)
  R2 = matrix(0, nrow = trials, ncol = tpeak*3)
  
  R1p = matrix(0, nrow = trials, ncol = tpeak*3)
  R2p = matrix(0, nrow = trials, ncol = tpeak*3)
  
  if (same_sd) {
    l1 = rnorm(trials, mu, sd)
    l2 = rnorm(trials, mu, sd)
    
    while (any(l1 < 0) | any(l2 < 0)) {
      l1 = rnorm(trials, mu, sd)
      l2 = rnorm(trials, mu, sd)
    }
    
    nrf1vec = ceiling(l1 * t1) + tf
    nrf2vec = ceiling(l2 * nrf1vec)
    
  } else {
    l1 = rnorm(trials, mu, sd * 1.5)
    l2 = rnorm(trials, mu, sd)
    while (any(l1 < 0) | any(l2 < 0)) {
      l1 = rnorm(trials, mu, sd * 1.5)
      l2 = rnorm(trials, mu, sd)
    }
    
    nrf1vec = ceiling(l1 * (t1 + tf))
    nrf2vec = ceiling(l2 * nrf1vec)
    
  }
    
  nrf1pvec = ceiling(l1 * tpeak) 
  nrf2pvec = ceiling(l2 * tpeak)
  
  
  IRI1 = nrf1vec
  IRI2 = nrf2vec
  
  for (j in 1:trials) {
    ttype = tvector[j]
    if (ttype){
      
      Nt = ceiling(l1[j] * t1vec)
      
      R1[j, 1:length(Nt)] = ifelse(wn_vector[Nt] > theta, 1, 0)      
      
      t2vec = 1:nrf1vec[j]      
      Nt2 = ceiling(t2vec * l2[j])      
      R2[j, 1:length(Nt2)] = ifelse(wn_vector[Nt2] > theta, 1, 0)
      
      
      # Delayed component
      w1 = wn_vector[nrf1vec[j]] + beta * (1 - wn_vector[nrf1vec[j]]) 
      wn_vector[nrf1vec[j]] = w1 
      w2 = wn_vector[nvec < nrf1vec[j]] - (alpha / nrf1vec[j]) * wn_vector[nvec < nrf1vec[j]]
      wn_vector[nvec < nrf1vec[j]] = w2
      
      # Immediate component
      
      w1 = wn_vector[nrf2vec[j]] + beta * (1 - wn_vector[nrf2vec[j]])
      wn_vector[nrf2vec[j]] = w1
      w2 = wn_vector[nvec < nrf2vec[j]] - (alpha / nrf2vec[j]) * wn_vector[nvec < nrf2vec[j]]
      wn_vector[nvec < nrf2vec[j]] = w2
      
    } else {
      
      
      Nt = ceiling(l1[j] * 1:tpeak)
      Nt2 = ceiling(l2[j] * 1:tpeak)
      
      R1p[j, 1:length(Nt)] = ifelse(wn_vector[Nt] > theta, 1, 0)
      R2p[j, 1:length(Nt2)] = ifelse(wn_vector[Nt2] > theta, 1, 0)
      
      nrf1p = nrf1pvec[j]
      nrf2p = nrf2pvec[j]
      
      wp1 = wn_vector[nvec < nrf1p] - (alpha / nrf1p) * wn_vector[nvec < nrf1p]
      wn_vector[nvec < nrf1p] = wp1
      wp2 = wn_vector[nvec < nrf2p] - (alpha / nrf2p) * wn_vector[nvec < nrf2p]
      
      wn_vector[nvec < nrf2p] = wp2
    }

  }
  
  list(R1 = R1, # 1
       R2 = R2, # 2
       R1p = R1p, # 3
       R2p = R2p, # 4
       Wn1 = wn_vector, # 5
       iri1 = IRI1,
       iri2 = IRI2)
}
