## -------------------------- Data preparation --------------------------------

## Source wrapper ----  
## WARNING: wrapper erase all data in memory, run it with at the beggining;
## also, it will load several custum functions without which ANYTHING below
## will work as expected.

source("~/Documentos/2019A/paper_gen_decrement/analysis/r/wrapper.r")

# Paths for processed data. Check raw2processed_proc.r ----

path_read.general = "~/Documentos/2019A/paper_gen_decrement/Data/"
path_read.BL = path_read.general + "processed/baseline"
path_read.EXP = path_read.general + "processed/experimental"

# Paths for results of analysis (data_results) ---- 

path_save.general = "~/Documentos/2019A/paper_gen_decrement/analysis/"
path_save.data = path_save.general + "data_results/"

# ETC
## vector of subjects numbers, used in several functions below
subjects_vec = 326:333
## Names of phases
ph_names = c("Baseline", "Experimental")
# Path of BL and EXP to read
paths = c(path_read.BL, path_read.EXP)

## Extra: individual trial analysis for the FI ----
df_bp1_fi <- ind_ana_bp1(
  list_ss = subjects_vec,
  phases_names = ph_names,
  paths = paths,
  sessions = 40,
  max_trials = 2000
) %>%
  filter(sesion < 39) %>%
  as.data.frame() 



colores = c('#8c8c8c', "#8F2727")

ses = unique(df_bp1_fi$sesion)
cdes = unique(df_bp1_fi$cde)
trial = unique(df_bp1_fi$cum_trial)
phase = unique(df_bp1_fi$phase)

df1 = df_bp1_fi %>%
  group_by(phase, sesion, cde) %>%
  summarise(bp = mean(bp))

par(mfrow = c(1, 2))
cdes = c(1, 2)

for(i in unique(df1$phase)){
  
  
  xmax = max(df1$sesion[df1$phase == i])
  ymax = 45
  ymin = min(df1$bp)
  
  plot(0,0, ylab = '', xlab = '', 
       col = 'white', ylim = c(ymin, ymax), xlim = c(0, xmax))
  
  for (k in cdes){
    
    if (k == cdes[1]){
      col = colores[1]
    } else {
      col = colores[2]
    }
    x = df1$sesion[df1$cde == k & df1$phase == i]
    y = df1$bp[df1$cde == k & df1$phase == i]
    points(x,y, pch = 21, bg = col)
    lines(x,y, col = col)
  }
}


dF = data.frame()

for (i in phase) {
  di = df_bp1_fi[df_bp1_fi$phase == i, ]
  
  for (ii in ses){
    
    dii = di[di$sesion == ii, ]
    
    for (iii in cdes) {
      diii = dii[dii$cde == iii, ]
      sumr = sum(length(diii$r2) > 0 & length(diii$r2 > 0))
      if( sumr > 0) {
        ratio = mean(diii$r2 / diii$r1)
        ratio = log10(ratio)
        dFtmp = data.frame(phase = i, ses = ii, cde = iii, ratio = ratio)
      } else {
        dFtmp = data.frame()
      }
      
      dF = bind_rows(dF, dFtmp)
    }
  }
}


pdf('ts_logRatio.pdf',
    height = 4, width = 7)

par(mfrow = c(1, 2))

cdes = c(1, 2)
for (pi in phase) {
  
  xmax = max(dF$ses[dF$phase == pi])
  ymax = max(dF$ratio)
  ymin = min(dF$ratio)
  plot(NA,NA, xlim = c(1, xmax), ylim = c(ymin, ymax), axes = F, 
       xlab = 'Sessions', ylab = TeX('$\\log_{10}$ mean ratio'), 
       main = pi, panel.first = grid())
  
  for (cdi in cdes) {
    if (cdi == 1){
      col = colores[1]
    } else {
      col = colores[2]
    }
    x = dF$ses[dF$phase == pi & dF$cde == cdi]
    y = dF$ratio[dF$phase == pi & dF$cde == cdi]
    points(x, y, pch = 21, bg = col)
    lines(x, y, col = col)
  }
  if (pi == phase[1]){
    legend('topleft', legend = c('Delayed','Immediate'),
           col = colores, pch = c(16, 16), bty = 'n')
  }
  axis(1, tck = -0.02, seq(0, xmax, 5))
  axis(2, tck = -0.02)
}

dev.off()

pdf('density_logRatio.pdf',
    height = 4, width = 7)

par(mfrow = c(1, 2),
    mar = c(4,1,3,1))
cdes = c(1, 2)

for (pi in phase) {
  
  for (cdi in cdes){
    
    x = dF$ratio[dF$phase == pi & dF$cde == cdi]
    dx1 = density(x)
    
    if(pi == phase[1]){
      xlim = c(1, 1.8)
      ylim = c(0, 12)
    } else {
      xlim = c(1, 1.8)
      ylim = c(0, 12)
    }
    
    if (cdi == 1){
      col = colores[1]
      plot(dx1, type ='l', 
           xlim = xlim, ylim = ylim,
           ylab = '', xlab = TeX('$\\log_{10}$ mean ratio'),
           main = pi, col = col, 
           axes = F, panel.first = grid())
      
    } else {
      col = colores[2]
      lines(dx1, col = col)
    }
  }
  axis(1, tck = -0.02)
  if (pi == phase[1]){
    legend('topleft', legend = c('Delayed','Immediate'),
           col = colores, pch = c(16, 16), bty = 'n')
  }
  
}

dev.off()

# Quarter life analysis

quants0 <- quants25.75(
  list_ss = subjects_vec,
  phases_names = ph_names,
  paths = paths,
  sessions = 40,
  max_trials = 2000
) %>%
  filter(sesion < 39) %>%
  as.data.frame() 

quants = quants0 %>%
  group_by(cde, phase, sesion) %>%
  summarise(q25 = mean(q25),
            q75 = mean(q75))

ses = unique(quants$sesion)
cdes = unique(quants$cde)
ph = unique(quants$phase)

par(mfrow = c(1, 2),
    
    )

for (pi in ph){
  if (pi == ph[1]) {
    xlims = c(0, 40)
  } else {
    xlims = c(0, 30)
  }
  ylims = c(30, 80)
  plot(0,0, color = 'white', 
       xlab = '', ylab = '',
       xlim = xlims, ylim = ylims)
  
  for (ci in cdes){
    
    if ( ci == cdes[1]) {
      
      col = colores[1]
      
    } else {
      
      col = colores[2]
    }
    
    x = quants$sesion[quants$cde == ci & quants$phase == pi]
    y = quants$q25[quants$cde == ci & quants$phase == pi]
    y1 = quants$q75[quants$cde == ci & quants$phase == pi]
    points(x, y, pch = 21, bg = col)
    points(x, y1, pch = 21, bg = col)
  }
}
