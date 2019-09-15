# fwhm.R 
# Full width half maximum and maximum at full width.
# The arguments x and y comes from the output
# of the density() function, i.e, x = density()$x, y = density()$y

# There are 2 ifferent functions. Some trials have more than one peak. 
# This is a problem I tried to solve with the more complex one (second),
# but for most of the subjects, the first worked well

# fwhm <- function(x,y){
# 
#   xy <- data.frame(x = x,y = y)
#   xmax <- xy$x[xy$y==max(xy$y)][1] # thakes just one max if there were 2
#   x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
#   x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
#   fwhm <- x2 - x1
#   list(fwhm = fwhm,peak = x[xmax])
# }


# This other could implement the same estimating a cutoff point between
# two peaks with the findpeaks() function in pracma library. 

fwhm <- function(x,y){

  xy <- data.frame(x = x,y = y)
  xy <- xy[xy$x > 0 & xy$x < 180,]
  y.peaks <- pracma::findpeaks(xy$y,npeaks = 2)

  if(length(y.peaks[,1]) !=1 ){
    p1 <- xy[xy$y == y.peaks[1,1],1]
    p2 <- xy[xy$y == y.peaks[2,1],1]
    # Find minimum, which will be the cutoff point
    min <- optimize(approxfun(xy$x,xy$y),interval=c(p1,p2))$minimum
    xy <- xy[xy$x <= min,]
  }

  xmax <- xy$x[xy$y==max(xy$y)]
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
  fwhm <- x2 - x1

  list(fwhm = fwhm,peak = xmax)

}


# Use: 
# 
# Delayed
# times_vec1 <- resp_times_aco$bins[resp_times_aco$sujeto == "M333" &
#                                    resp_times_aco$sesion == 30 &
#                                    resp_times_aco$cum_trial == 2 &
#                                    resp_times_aco$cde == 1]
# # Immediate
# times_vec2 <- resp_times_aco$bins[resp_times_aco$sujeto == "M333" &
#                                     resp_times_aco$sesion == 30 &
#                                     resp_times_aco$cum_trial == 2 &
#                                     resp_times_aco$cde == 2]
# 
# xy <- density(times_vec1) # vector of times of resp
# 
# xmax <- xy$x[xy$y==max(xy$y)][1] # thakes just one max if there were 2
# x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
# x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
# fwhm <- x2 - x1
# 
# pdf("SM_Figure3.pdf",
#     width = 7,
#     height = 3.5,
#     pointsize = 9)
# 
# par(mfrow = c(1,2),
#     mgp = c(2.7,0.5,0),
#     mar = c(2,1,0.5,0.5),
#     oma = c(1.5,4,0,0))
# xy <- density(times_vec1) # vector of times of resp
# 
# xmax <- xy$x[xy$y==max(xy$y)][1] # thakes just one max if there were 2
# x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
# x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
# fwhm <- x2 - x1
# 
# plot(xy, axes = F,ylim = c(0,0.02),
#      xlab = 'Times of responses',
#      xlim = c(0,180),
#      ylab = '',
#      main = '')
# points(x = c(x1,x2,xmax),y = c(max(xy$y)/2,max(xy$y)/2,max(xy$y)),
#        pch = 21,col = 'black',bg = 'navy')
# segments(x0 = x1,x1 = x2,y0 = max(xy$y)/2,y1 = max(xy$y)/2)
# rug(times_vec1)
# axis(1,tck = -0.02,at = seq(0,180,30))
# axis(2, las = 1,at = seq(0,0.02,0.005),tck = -0.02)
# text(xmax-6,max(xy$y)/2.13,labels = sprintf('FWHM: %0.1f s',fwhm),
#      cex = 0.9)
# text(xmax+14,max(xy$y),labels = 'Peak')
# box(lwd = 0.001)
# xy <- density(times_vec2) # vector of times of resp
# 
# xmax <- xy$x[xy$y==max(xy$y)][1] # thakes just one max if there were 2
# x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
# x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
# fwhm <- x2 - x1
# 
# plot(xy, axes = F,ylim = c(0,0.02),
#      xlab = 'Times of responses',
#      xlim = c(0,180),
#      ylab = '',
#      main = '')
# points(x = c(x1,x2,xmax),y = c(max(xy$y)/2,max(xy$y)/2,max(xy$y)),
#        pch = 21,col = 'black',bg = 'navy')
# segments(x0 = x1,x1 = x2,y0 = max(xy$y)/2,y1 = max(xy$y)/2)
# rug(times_vec1)
# axis(1,tck = -0.02,at = seq(0,180,30))
# # axis(2, las = 1,at = seq(0,0.02,0.005),tck = -0.02)
# text(xmax-6,max(xy$y)/2.13,labels = sprintf('FWHM: %0.1f s',fwhm),
#      cex = 0.9)
# text(xmax+14,max(xy$y),labels = 'Peak')
# box(lwd = 0.001)
# grid.text("Times of responses",
#           x = 0.52,y=0.03,
#           gp = gpar(fontsize=14))
# grid.text("Density",
#           rot = 90,
#           x = 0.02,y=0.5,
#           gp = gpar(fontsize=14))
# 
# dev.off()
# 
# 
# 
# # # E.g.
# set.seed(123)
# x = rnorm(100)
# dx <- density(x)
# 
# y = dx$y
# x = dx$x
# fwhm(dx$x,dx$y)
# 
