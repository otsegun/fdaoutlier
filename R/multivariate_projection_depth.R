# function to compute multivariate projection depth
# adapted from fda.usc
projection_depth <- function(matx, n_projections = 500, seed = null, delta = 1e-15){

  if(!is.null(seed)){
    set.seed(seed)
  }
  n <- nrow(matx)
  d <- ncol(matx)

  u_matrix <- matrix(runif(d*n_projections,-1,1), n_projections, d)
  u_matrix <- u_matrix/sqrt(rowSums(u_matrix*u_matrix))

  z <- u_matrix %*% t(matx)

  depthcol <- apply(z, 1, function(x){
    Fn <- ecdf(x)
    pmin(Fn(x), (1 - Fn(x-delta)))
  })
  depths <- apply(depthcol, 1, mean)
}

