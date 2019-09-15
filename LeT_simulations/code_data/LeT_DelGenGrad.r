library(tidyverse)
source('fwhm.R')
source('LeT_sd_versionsV2.r')

theta = 0.1; W0 = 0.12
alpha = 1; beta = 0.2
pfood = 0.75
mu = 1; sd = 0.2
trials = 60 * 8
tx = 60
tpeak = tx * 3
prop_tr = 0.1

n = tpeak * 3
params = c(n, mu, sd, alpha, beta, W0, theta, pfood)

wn_vector = numeric(n)
sesiones = 50
sL = vector('list', sesiones)

for (j in 1:sesiones) {
  
  if (j > 1) {
    wn_vector = sL[[j - 1]][[5]]
  }
  
  sL[[j]] = LeT_peak_deg(tx, tpeak, prop_tr,
                         trials, params,wn_vector, same_sd = F)
}


df = data.frame()
fwhm1 = c()
fwhm2 = c()
peak1 = c()
peak2 = c()
i1 = c()
i2 = c()

for (i in 20:sesiones) {
  R1 = sL[[i]][[3]][ ,1:180]
  R2 = sL[[i]][[4]][ ,1:180]
  
  Wn1 = sL[[i]][['Wn1']][1:180]
  iriD = sL[[i]][['iri1']]
  iriIm = sL[[i]][['iri2']]
  
  i1 = c(i1, iriD)
  i2 = c(i2, iriIm)
  
  sumsR1 = colSums(R1)
  sumsR2 = colSums(R2)
  
  d1 = density(sumsR1)
  d2 = density(sumsR2)
  
  ft1 = fwhm(d1$x, d1$y)
  ft2 = fwhm(d2$x, d2$y)
  
  fwhm1 = c(fwhm1, ft1$fwhm)
  peak1 = c(peak1, ft1$peak)
  
  fwhm2 = c(fwhm2, ft2$fwhm)
  peak2 = c(peak2, ft2$peak)
  
  dfTmp = data.frame(bin = 1:180,
                     R1 = sumsR1,
                     R2 = sumsR2,
                     Wn1 = Wn1,
                     sesion = i)
  df = rbind(df, dfTmp)
}

df_summary = df %>%
  group_by(bin) %>%
  summarise(r1 = mean(R1) / 180,
            r2 = mean(R2) / 180,
            wn1 = mean(Wn1)) 


# save(sL, dfFWHM, df_summary, file = 'LeT_PeakDiffSD.RData')

## Same rate lambda for both components

wn_vector = numeric(n)
sesiones = 50
sL = vector('list', sesiones)

for (j in 1:sesiones) {
  
  if (j > 1) {
    wn_vector = sL[[j - 1]][[5]]
  }
  
  sL[[j]] = LeT_peak_deg(tx, tpeak, prop_tr, 
                         trials, params,wn_vector, same_sd = T)
}


df = data.frame()
fwhm1 = c()
fwhm2 = c()
peak1 = c()
peak2 = c()
i1 = c()
i2 = c()

for (i in 1:sesiones) {
  R1 = sL[[i]][[3]][ ,1:180]
  R2 = sL[[i]][[4]][ ,1:180]
  
  Wn1 = sL[[i]][['Wn1']][1:180]
  iriD = sL[[i]][['iri1']]
  iriIm = sL[[i]][['iri2']]
  
  i1 = c(i1, iriD)
  i2 = c(i2, iriIm)
  
  sumsR1 = colSums(R1)
  sumsR2 = colSums(R2)
  
  d1 = density(sumsR1)
  d2 = density(sumsR2)
  
  ft1 = fwhm(d1$x, d1$y)
  ft2 = fwhm(d2$x, d2$y)
  
  fwhm1 = c(fwhm1, ft1$fwhm)
  peak1 = c(peak1, ft1$peak)
  
  fwhm2 = c(fwhm2, ft2$fwhm)
  peak2 = c(peak2, ft2$peak)
  
  dfTmp = data.frame(bin = 1:180,
                     R1 = sumsR1,
                     R2 = sumsR2,
                     Wn1 = Wn1,
                     sesion = i)
  df = rbind(df, dfTmp)
}

df_summary = df %>%
  group_by(bin) %>%
  summarise(r1 = mean(R1) / 180,
            r2 = mean(R2) / 180,
            wn1 = mean(Wn1))


# save(sL, dfFWHM, df_summary, file = 'LeT_PeakSameSD.RData')
