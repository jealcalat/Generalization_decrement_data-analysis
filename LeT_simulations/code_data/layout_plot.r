library(tidyverse)
source('fwhm.R')

load('LeT_PeakSameSD.RData')
sesiones = 100
df = data.frame()
fwhm1 = c()
fwhm2 = c()
peak1 = c()
peak2 = c()
i1 = c()
i2 = c()

for (i in 20:sesiones) {
  R1 = sL[[i]][['R1p']][ ,1:180]
  R2 = sL[[i]][['R2p']][ ,1:180]
  
  Wn1 = sL[[i]][['Wn1']][1:180]
  # Wn2 = sL[[i]][['Wn2']][1:180]
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
                     #Wn2 = Wn2,
                     sesion = i)
  df = rbind(df, dfTmp)
}

df_summary = df %>%
  group_by(bin) %>%
  summarise(r1 = mean(R1) / 180,
            r2 = mean(R2) / 180,
            wn1 = mean(Wn1)) 

pdf('SameRate.pdf', width = 6, height = 6, pointsize = 12)

  cex = 1.8
  
  par(#mfrow = c(2,2), 
    oma = c(1,1,0,0),
    mgp = c(2, 0.2, 0),
    mar = c(3, 3.8, 1, 1))
  
  ul = rep(1, 4)
  dr = rep(2, 2)
  ur = rep(3, 3)
  dl = rep(4, 3)
  
  
  layout(matrix(c(ul, dr,
                  ul, dr,
                  ul, dr,
                  dl, ur,
                  dl, ur,
                  dl, ur), nrow = 6, byrow = T))
  
  plot(df_summary$bin,
       (df_summary$r2 - min(df_summary$r2, df_summary$r1)) / 
         (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1)),
       xlab = 'Time in trial',
       ylab = 'Normalized response rate',
       axes = F, lwd = 0.6,
       bg = 'white', pch = 21, cex = cex)
  
  points(df_summary$bin,
         (df_summary$r1 - min(df_summary$r2, df_summary$r1)) / 
           (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1)),
         bg = '#808080', pch = 21,lwd = 0.6, cex = cex*0.8)
  maxy =  max(df_summary$r1 - min(df_summary$r2, df_summary$r1)) / 
    (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1))
  
  segments(x0 = which.max(df_summary$r1), 
           x1 = which.max(df_summary$r1), 
           y0 = 0, y1 = maxy, lty = 1, col = '#808080')
  
  segments(x0 =  which.max(df_summary$r2), 
           x1 =  which.max(df_summary$r2), 
           y0 = 0, y1 = 1, lty = 2)
  # arrows(x0 = which.max(df_summary$r1),
  #        x1 = which.max(df_summary$r2),
  #        y0 = 0.5, y1 = 0.5, code = 2,
  #        length = 0.07, lwd = 1.5)
  
  # text(110, 1, sprintf('Peak = %d', which.max(df_summary$r2)))
  # text(155, 0.4, sprintf('Peak = %d', which.max(df_summary$r1)))
  
  axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 60))
  axis(2, las = 2, tck = -0.01, lwd = 1.4)
  
  dfFWHM = data.frame(fwhm = c(fwhm1,fwhm2), 
                      r = c(rep(1.8,length(fwhm1)),
                            rep(2,length(fwhm2))))
  
  plot(jitter(as.numeric(dfFWHM$r)),
       dfFWHM$fwhm,
       pch = 21, cex = cex,lwd = 0.6,
       bg = c('grey80','white')[as.numeric(dfFWHM$r)],
       type = 'p', axes = F, xlim = c(1.7, 2.2), ylim = c(10, 90),
       xlab = 'Components', ylab = 'FWHM')
  
  points(1.8, median(fwhm1), pch = '-', cex = 6, col = 'black')
  points(2, median(fwhm2),  pch = '-', cex = 6, col = 'grey50')
  axis(side = 1, at = as.numeric(unique(dfFWHM$r)),
       labels = c('Delayed', 'Immediate'),
       tck = -0.01, lwd = 1.4, cex.axis = 0.8)
  axis(2,las = 2, tck = -0.01, lwd = 1.4, at = seq(10, 90, 20))
  
  # qqplot
  
  qqplot(i1, #[which(i1>60 & i2 > 60)], 
         i2, #[which(i1>60 & i2 > 60)], 
         pch = 21,lwd = 0.2, cex = cex,
         bg = 'grey80',
         ylim = c(0, 180), xlim = c(0, 180),
         axes = F,
         xlab = bquote('Delayed reinforced states'~~italic(n)^'+'),
         ylab = bquote('Immediate reinforced states'~~italic(n)^'+'))
  segments(x0 = 0, y0 = 0,
           x1 = 180, y1 = 180, lty =2 )
  axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 45))
  axis(2, las = 2, tck = -0.01, at = seq(0, 180, 45), lwd = 1.4)
  
  plot(df_summary$wn1, 
       xlab = 'Behavioral states (n)',
       ylab = 'W(n)',
       pch = 21, lwd = 0.6,
       axes = F, cex = cex,
       bg = 'grey30',
       ylim = c(0, 0.5),
       xlim = c(0, 180))
  points(x = which.max(df_summary$wn1), y = -0.006, 
         pch = 24, bg = 'grey89')
  axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 60))
  axis(2, las = 2, tck = -0.01, lwd = 1.4)

dev.off()

load('LeT_PeakDiffSD.RData')
sesiones = 100
df = data.frame()
fwhm1 = c()
fwhm2 = c()
peak1 = c()
peak2 = c()
i1 = c()
i2 = c()

for (i in 20:sesiones) {
  R1 = sL[[i]][['R1p']][ ,1:180]
  R2 = sL[[i]][['R2p']][ ,1:180]
  
  Wn1 = sL[[i]][['Wn1']][1:180]
  # Wn2 = sL[[i]][['Wn2']][1:180]
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
                     #Wn2 = Wn2,
                     sesion = i)
  df = rbind(df, dfTmp)
}

df_summary = df %>%
  group_by(bin) %>%
  summarise(r1 = mean(R1) / 180,
            r2 = mean(R2) / 180,
            wn1 = mean(Wn1)) # ,wn2 = mean(Wn2)

pdf('DiffRate.pdf', width = 6, height = 6, pointsize = 12)

cex = 1.8

par(#mfrow = c(2,2), 
  oma = c(1,1,0,0),
  mgp = c(2, 0.2, 0),
  mar = c(3, 3.8, 1, 1))

ul = rep(1, 4)
dr = rep(2, 2)
ur = rep(3, 3)
dl = rep(4, 3)


layout(matrix(c(ul, dr,
                ul, dr,
                ul, dr,
                dl, ur,
                dl, ur,
                dl, ur), nrow = 6, byrow = T))

plot(df_summary$bin,
     (df_summary$r2 - min(df_summary$r2, df_summary$r1)) / 
       (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1)),
     xlab = 'Time in trial',
     ylab = 'Normalized response rate',
     axes = F, lwd = 0.6,
     bg = 'white', pch = 21, cex = cex)

points(df_summary$bin,
       (df_summary$r1 - min(df_summary$r2, df_summary$r1)) / 
         (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1)),
       bg = '#808080', pch = 21,lwd = 0.6, cex = cex)
maxy =  max(df_summary$r1 - min(df_summary$r2, df_summary$r1)) / 
  (max(df_summary$r2, df_summary$r1) - min(df_summary$r2, df_summary$r1))

segments(x0 = which.max(df_summary$r1), 
         x1 = which.max(df_summary$r1), 
         y0 = 0, y1 = maxy, lty = 1, col = '#808080')

segments(x0 =  which.max(df_summary$r2), 
         x1 =  which.max(df_summary$r2), 
         y0 = 0, y1 = 1, lty = 2)
arrows(x0 = which.max(df_summary$r1),
       x1 = which.max(df_summary$r2),
       y0 = 0.5, y1 = 0.5, code = 2,
       length = 0.07, lwd = 1.5)

text(110, 1, sprintf('Peak = %d', which.max(df_summary$r2)))
text(155, 0.4, sprintf('Peak = %d', which.max(df_summary$r1)))

axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 60))
axis(2, las = 2, tck = -0.01, lwd = 1.4)

dfFWHM = data.frame(fwhm = c(fwhm1,fwhm2), 
                    r = c(rep(1.8,length(fwhm1)),
                          rep(2,length(fwhm2))))

plot(jitter(as.numeric(dfFWHM$r)),
     dfFWHM$fwhm,
     pch = 21, cex = cex,lwd = 0.6,
     bg = c('grey80','white')[as.numeric(dfFWHM$r)],
     type = 'p', axes = F, xlim = c(1.7, 2.2), ylim = c(10, 90),
     xlab = 'Components', ylab = 'FWHM')

points(1.8, median(fwhm1), pch = '-', cex = 6, col = 'black')
points(2, median(fwhm2),  pch = '-', cex = 6, col = 'grey50')
axis(side = 1, at = as.numeric(unique(dfFWHM$r)),
     labels = c('Delayed', 'Immediate'),
     tck = -0.01, lwd = 1.4, cex.axis = 0.8)
axis(2,las = 2, tck = -0.01, lwd = 1.4, at = seq(10, 90, 20))

# qqplot

qqplot(i1, #[which(i1>60 & i2 > 60)], 
       i2, #[which(i1>60 & i2 > 60)], 
       pch = 21,lwd = 0.2, cex = cex,
       bg = 'grey80',
       ylim = c(0, 190), xlim = c(0, 190),
       axes = F,
       xlab = bquote('Delayed reinforced states'~~italic(n)^'+'),
       ylab = bquote('Immediate reinforced states'~~italic(n)^'+'))
segments(x0 = 0, y0 = 0,
         x1 = 180, y1 = 180, lty =2 )
segments(x0 = 0, y0 = 0,
         x1 = 180, y1 = 180, lty =2 )
axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 45))
axis(2, las = 2, tck = -0.01, at = seq(0, 180, 45), lwd = 1.4)

plot(df_summary$wn1, 
     xlab = 'Behavioral states (n)',
     ylab = 'W(n)',
     pch = 21, lwd = 0.6,
     axes = F, cex = cex,
     bg = 'grey30',
     ylim = c(0, 0.5),
     xlim = c(0, 180))
points(x = which.max(df_summary$wn1), y = -0.006, 
       pch = 24, bg = 'grey89')
axis(1, tck = -0.01, lwd = 1.4, at = seq(0, 180, 60))
axis(2, las = 2, tck = -0.01, lwd = 1.4)

dev.off()
