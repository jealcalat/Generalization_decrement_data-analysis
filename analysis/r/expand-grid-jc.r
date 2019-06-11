# Custom function that do the same as expand.grid, but faster.
# Taken from
# https://stackoverflow.com/questions/10405637/use-outer-instead-of-expand-grid

expand.grid.jc <- function(v1,v2) {
  cbind(rep.int(v1, length(v2)), 
        rep.int(v2, rep.int(length(v1),length(v2))))
}
