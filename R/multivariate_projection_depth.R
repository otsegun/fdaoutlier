#' Random Projection for Multivariate Data
#'
#'
#' Helper function to compute the random projection depth of a multivariate data with respect to itself.
#'
#'
#' @param dt A matrix or dataframe of size n observations by d dimension).
#' @param n_projections An integer, indicating the number of directions for random projections. By default,
#'   500 random directions for projection are generated from a scaled uniform distribution.
#' @param seed An integer to set a seed when generating the random directions. Useful for reproducibility.
#' @param delta Accuracy. The left limit of the empirical distribution function.
#'
#' @return A vector contaning the depth values \code{dt} with respect to itself.
#' @author Version adapted from original code contained in \code{fda.usc}.
#' @export
#' @importFrom stats ecdf runif
#' @examples
projection_depth <- function(dt, n_projections = 500L, seed = NULL, delta = 1e-15){

  if(is.data.frame(dt)){
    dt <- as.matrix(dt)
  }
  if(!is.array(dt))
    stop("dt must be a matrix or dataframe.")

  if (any(is.na(dt)) || any(is.infinite(dt)))
    stop("Missing or infinite values are not allowed.")

  if(!is.null(seed)){
    set.seed(seed)
  }

  n <- nrow(dt)
  d <- ncol(dt)

  u_matrix <- matrix(runif(d*n_projections,-1,1), n_projections, d)
  u_matrix <- u_matrix/sqrt(rowSums(u_matrix*u_matrix))

  z <- u_matrix %*% t(dt)

  depthcol <- apply(z, 1, function(x){
    Fn <- ecdf(x)
    pmin(Fn(x), (1 - Fn(x-delta)))
  })
  depths <- apply(depthcol, 1, mean)
}

