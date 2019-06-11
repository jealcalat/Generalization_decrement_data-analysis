# fwhm.R 
# Full width half maximum and maximum at full width.
# The arguments x and y comes from the output
# of the density() function, i.e, x = density()$x, y = density()$y

fwhm <- function(x,y){
  
  xy <- data.frame(x = x,y = y)
  xmax <- xy$x[xy$y==max(xy$y)][1] # thakes just one max if there were 2
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
  fwhm <- x2 - x1
  list(fwhm = fwhm,peak = xmax)
}