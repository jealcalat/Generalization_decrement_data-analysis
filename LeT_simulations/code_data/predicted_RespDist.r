r1 = dnorm(1:120, 1*66, 0.2*66)
r2 = dnorm(1:120, 1*62, 0.3*62)

pdf('predicted_Rdist.pdf',
    width = 6, height = 2.5)
par(mgp = c(2,1,0),
    mar = c(2,0,0,0))
plot(r1, xlab = '', ylab = '', 
     axes = F, type = 'l',
     lwd = 4)
arrows(x0 = 66, x1 = 66, length = 0.15,
       y0 = 0, y1 = max(r1), 
       lwd = 4)
lines(r2, xlab = '', ylab = '', 
      type = 'l', lwd = 4, col = 'grey50')

arrows(x0 = 62, x1 = 62,
       y0 = 0, y1 = max(r2), 
       lwd = 4, length = 0.15, 
       col = 'grey50')
axis(1, at = c(0, 60, 120), 
     lwd = 4, cex.axis = 2,
     tck = -0.02, labels = c('', 60, ''))

dev.off()

