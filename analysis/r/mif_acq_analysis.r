source("~/Documentos/2019A/paper_gen_decrement/analysis/r/wrapper.r")

path_read.general = "~/Documentos/2019A/paper_gen_decrement/Data/"
path_read.mif = path_read.general + "processed/mult_IF_acq"

subjects_vec = 326:333
colores = c('#8c8c8c', "#8F2727")

mif <- bp_fi_wrp(subjects_vec, 60,
                 rp_index=c(1,11),
                 path_read = path_read.mif)

mif$ctrial = 1

mif <- mif %>% 
  group_by(sujeto,cde) %>%
  arrange(sesion) %>%
  mutate(cum_trial = cumsum(ctrial))

png('bp_mif_acq.png', 
    res = 120, width = 600, height = 480)

par(mfrow = c(1, 2),
    mar = c(4,4,3,0.2))

scatter.smooth(mif$cum_trial[mif$cde == 1], 
               mif$bp[mif$cde == 1],
               pch = '.', ylim = c(0, 65), cex = 2,
               col = colores[1], axes = F,
               panel.first = grid(),
               lpars = list(col = colores[2], lwd = 3, lty = 1),
               xlab = 'Trials', ylab = 'Breakpoint (s)')

title(main = 'Delayed')
axis(1, tck = -0.02)
axis(2, tck = -0.02)
scatter.smooth(mif$cum_trial[mif$cde == 2], 
               mif$bp[mif$cde == 2],
               pch = '.', ylim = c(0, 65), cex = 2,
               panel.first = grid(),axes = F,
               lpars = list(col = 'black', lwd = 3, lty = 1),
               col = colores[2], ylab = '', xlab = '')
axis(1, tck = -0.02)
axis(2, tck = -0.02)
title(main = 'Immediate', xlab = 'Trials')
dev.off()

ses = unique(mif$sesion)
cdes = unique(mif$cde)

dF = data.frame()

for (ii in ses){
  
  dii = mif[mif$sesion == ii, ]
  
  for (iii in cdes) {
    diii = dii[dii$cde == iii, ]
    sumr = sum(length(diii$r2) > 0 & length(diii$r2 > 0))
    if( sumr > 0) {
      ratio = mean(diii$r2 / diii$r1)
      ratio = log10(ratio)
      dFtmp = data.frame(ses = ii, cde = iii, ratio = ratio)
    } else {
      dFtmp = data.frame()
    }
    
    dF = bind_rows(dF, dFtmp)
  }
}

dF = dF %>%
  arrange(ses) %>%
  as.data.frame()

cdes = c(1, 2)
colores = c('#8c8c8c', "#8F2727")

xmax = max(dF$ses)
ymax = max(dF$ratio)
ymin = min(dF$ratio)

png('ratio_r1r2_mifacq.png',
    res = 120)

plot(NA,NA, xlim = c(1, xmax), ylim = c(ymin, ymax), axes = F, 
     xlab = 'Sessions', ylab = TeX('$\\log_{10}$ mean ratio'), 
     panel.first = grid())

for (cdi in cdes) {
  if (cdi == 1){
    col = colores[1]
  } else {
    col = colores[2]
  }
  x = dF$ses[dF$cde == cdi]
  y = dF$ratio[dF$cde == cdi]
  points(x, y, pch = 21, bg = col)
  lines(x, y, col = col)
}

legend('topleft', legend = c('Delayed','Immediate'),
       col = colores, pch = c(16, 16), bty = 'n')
axis(1, tck = -0.02, seq(0, xmax, 5))
axis(2, tck = -0.02)
dev.off()
## qlife

qlife_mif <- qlife_wrp(subjects_vec, 60,
                       rp_index=c(1,11),
                       path_read = path_read.mif)

qlife_mif$ctrial = 1

qlife_mif <- qlife_mif %>% 
  group_by(sujeto,cde) %>%
  arrange(sesion) %>%
  mutate(cum_trial = cumsum(ctrial))


png('qlife_mif_acq.png', res = 120,
    width = 666, height = 480)

par(mfrow = c(1, 2),
    mar = c(4,4,3,0.2))

scatter.smooth(qlife_mif$cum_trial[qlife_mif$cde == 1],
               qlife_mif$q25[qlife_mif$cde == 1],
               pch = '.', ylim = c(0, 65), cex = 2,
               col = colores[1], axes = F,
               panel.first = grid(),
               lpars = list(col = colores[2], lwd = 3, lty = 1),
               xlab = 'Trials', ylab = 'Quarter life (s)')

title(main = 'Delayed')
axis(1, tck = -0.02)
axis(2, tck = -0.02)
scatter.smooth(qlife_mif$cum_trial[qlife_mif$cde == 2], 
               qlife_mif$q25[qlife_mif$cde == 2],
               pch = '.', ylim = c(0, 65), cex = 2,
               panel.first = grid(),axes = F,
               lpars = list(col = 'black', lwd = 3, lty = 1),
               col = colores[2], ylab = '', xlab = '')
axis(1, tck = -0.02)
axis(2, tck = -0.02)
title(main = 'Immediate', xlab = 'Trials')
dev.off()

# ==============================================================================

quants = qlife_mif %>%
  group_by(cde, sesion) %>%
  summarise(q25 = mean(q25),
            q75 = mean(q75))

png('mean_qlife_ses.png', res = 120)

plot(quants$sesion[quants$cde == 1], 
     quants$q25[quants$cde == 1], 
     type = 'o', bg = colores[1],
     axes = F, xlab = 'Sessions',
     ylab = 'Quarter life (s)', cex = 0.8,
     panel.first = grid(), pch = 21)
points(quants$sesion[quants$cde == 2], 
       quants$q25[quants$cde == 2], pch = 21,
       bg = colores[2], type = 'o')
axis(1, tck = -0.02)
axis(2, tck = -0.02)
legend('bottomright', legend = c('Delayed','Immediate'),
       col = colores, pch = c(16, 16), bty = 'n')
dev.off()

# ==============================================================================


sigmoid = function(x, m, c, s){
  m / (1 + exp(-(x - c) / s))
}

resf = function(y, m, c, s, x) {
  y - m / (1 + exp(-(x - c) / s))
}


m = 2
c = 10
s = 2
x = seq(0, 100, 0.1)
eps = rnorm(length(x), 0, 0.4)

y = m / (1 + exp( -(x - c) / s )) + eps

nlss = nls(formula = y ~ m / (1 + exp( -(x - c) / s ) ),
           data = data.frame(x = x, y = y),
           start = list(m = 9.1, c = 5, s = 2),
           control = nls.control(maxiter = 500))

rsquared = function(y, res) {
  tss = (y - mean(y)) %*% (y - mean(y))
  rss = res %*% res
  1 - rss / tss
}

m = c(1, 1, 2, 2, 5, 5)
c = c(10, 10, 5, 5, 10, 10)
s = c(1, 4, 4, 2, 2, 1)
x = seq(0, 100, 0.1)
y0 = sigmoid(x, m[1], c[1], s[1])

png('r2_nls_sig_demo.png',
    res = 120,
    width = 780, height = 620)

par(mfrow = c(2, 3),
    mgp = c(1.5, 0.5, 0),
    mar = c(4,4,3.5,0.2))

for (i in 1:length(m)) {
  y = sigmoid(x, m[i], x[i], s[i]) + eps
  
  nlss = nls(formula = y ~ m / (1 + exp( -(x - c) / s ) ),
             data = data.frame(x = x, y = y),
             start = list(m = 9.1, c = 5, s = 2),
             control = nls.control(maxiter = 500))
  r2 = rsquared(y, residuals(nlss))
  
  plot(x, y, type = 'l', ylim = c(0, 6), axes = F,
       xlab = 'x', ylab = 'f(x)', col = 'grey60')
  lines(x, fitted(nlss), col = colores[2], lwd = 2)
  title(main = sprintf('R^2 = %0.2f, m = %d, \n c = %d, s = %s
                       ', r2, m[i], c[i], s[i]))
  axis(1, tck = -0.02)
  axis(2, tck = -0.02)
}
dev.off()

# exponencial

exp_learn = function(x, k, R){
  k * (1 - exp(- x / R))
}

x = seq(0, 100, 0.1)
k = 5
R = 10
y = exp_learn(x, k, R) + eps

plot(x,y)


# A test


k = c(2, 2, 5, 5, 10, 10)
R = c(5, 10, 5, 10, 5, 10)

png('r2_nls_exp_demo.png',
    res = 120,
    width = 780, height = 620)

par(mfrow = c(2, 3),
    mgp = c(1.5, 0.5, 0),
    mar = c(4,4,3.5,0.2))

for (i in 1:length(k)) {
  
  y = exp_learn(x, k[i], R[i]) + eps
  
  nlss = nlsLM(formula = y ~ k * (1 - exp(- x / R)),
               data = data.frame(x = x, y = y),
               start = list(k = k[i] * 0.6, R = R[i] * 0.7),
               control = nls.lm.control(maxiter = 500))
  
  r2 = rsquared(y, residuals(nlss))
  plot(x, y, type = 'l', ylim = c(0, 11), axes = F,
       xlab = 'x', ylab = 'f(x)', col = 'grey60')
  lines(x, fitted(nlss), col = colores[2], lwd = 2)
  title(main = sprintf('R^2 = %0.2f, k = %d, \n R = %d', r2, k[i], R[i]))
  axis(1, tck = -0.02)
  axis(2, tck = -0.02)
}

dev.off()

hyp_learn = function(x, k, R) {
  k * ( x / ( x + R ) )
}

x = seq(0, 100, 0.1)
k = 5
R = 10
y = hyp_learn(x, k, R) + eps

plot(x,y)


# A test

k = c(2, 2, 5, 5, 10, 10)
R = c(5, 10, 5, 10, 5, 10)

png('r2_nls_hyp_demo.png',
    res = 120,
    width = 780, height = 620)

par(mfrow = c(2, 3),
    mgp = c(1.5, 0.5, 0),
    mar = c(4,4,3.5,0.2))

for (i in 1:length(k)) {
  
  y = hyp_learn(x, k[i], R[i]) + eps
  
  nlss = nlsLM(formula = y ~ k * ( x / ( x + R ) ),
               data = data.frame(x = x, y = y),
               start = list(k = k[i] * 0.6, R = R[i] * 0.7),
               control = nls.lm.control(maxiter = 500))
  
  r2 = rsquared(y, residuals(nlss))
  plot(x, y, type = 'l', ylim = c(0, 11), axes = F,
       xlab = 'x', ylab = 'f(x)', col = 'grey60')
  lines(x, fitted(nlss), col = colores[2], lwd = 2)
  title(main = sprintf('R^2 = %0.2f, k = %d, \n R = %d', r2, k[i], R[i]))
  axis(1, tck = -0.02)
  axis(2, tck = -0.02)
}

dev.off()

# Using mean ratio

e1 = nlsLM(y ~ k * (1 - exp( - x / R)),
           data = data.frame(x = dF$ses[dF$cde == 1], 
                             y = dF$ratio[dF$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.lm.control(maxiter = 500))

s1 = nlsLM(formula = y ~ m / (1 + exp( -(x - c) / s ) ),
           data = data.frame(x = dF$ses[dF$cde == 1], 
                             y = dF$ratio[dF$cde == 1]),
           start = list(m = 9.1, c = 5, s = 2),
           control = nls.control(maxiter = 500))

h1 = nlsLM(formula = y ~ k * ( x / ( x + R ) ),
           data = data.frame(x = dF$ses[dF$cde == 1], 
                             y = dF$ratio[dF$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.control(maxiter = 500))

library(MuMIn)

model.sel(e1, s1, h1)


# Using BP


e1 = nlsLM(y ~ k * (1 - exp( - x / R)),
           data = data.frame(x = mif$sesion[mif$cde == 1], 
                             y = mif$bp[mif$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.lm.control(maxiter = 500))

s1 = nlsLM(formula = y ~ m / (1 + exp( -(x - c) / s ) ),
           data = data.frame(x = mif$sesion[mif$cde == 1], 
                             y = mif$bp[mif$cde == 1]),
           start = list(m = 9.1, c = 5, s = 2),
           control = nls.control(maxiter = 500))

h1 = nlsLM(formula = y ~ k * ( x / ( x + R ) ),
           data = data.frame(x = mif$sesion[mif$cde == 1], 
                             y = mif$bp[mif$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.control(maxiter = 500))

model.sel(e1, s1, h1)

# Using quarter life

e1 = nlsLM(y ~ k * (1 - exp( - x / R)),
           data = data.frame(x = quants$sesion[quants$cde == 1], 
                             y = quants$q25[quants$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.lm.control(maxiter = 500))

s1 = nlsLM(formula = y ~ m / (1 + exp( -(x - c) / s ) ),
           data = data.frame(x = quants$sesion[quants$cde == 1], 
                             y = quants$q25[quants$cde == 1]),
           start = list(m = 9.1, c = 5, s = 2),
           control = nls.control(maxiter = 500))

h1 = nlsLM(formula = y ~ k * ( x / ( x + R ) ),
           data = data.frame(x = quants$sesion[quants$cde == 1], 
                             y = quants$q25[quants$cde == 1]),
           start = list(k = 5, R = 4),
           control = nls.control(maxiter = 500))


model.sel(e1, s1, h1)
AICc(e1, s1, h1)
