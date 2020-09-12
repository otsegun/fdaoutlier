#' Random Projection for Multivariate Data
#'
#'
#' Helper function to compute the random projection depth of multivariate point(s) with respect
#' to a multivativariate data.
#'
#' @param dts A matrix or data frame of size \code{m} observations by \code{d} dimension or vector a of length \code{d}.
#'   Contains the observation(s) whose depth is to be computed.
#' @param dt A matrix or dataframe of size \code{n} observations by \code{d} dimension. Equals to
#'   \code{dts} by default.
#' @param n_projections The number of directions for random projections. By default,
#'   500 random directions for projection are generated from a scaled uniform distribution between -1 and 1.
#' @param seed The random seed to set when generating the random directions. Defaults to NULL.
#'
#' @return A vector contaning the depth values \code{dt} with respect to itself.
#' @author Oluwasegun Ojo \code{fda.usc}.
#' @export
#' @importFrom stats runif
#' @examples
projection_depth <- function(dts, dt = dts, n_projections = 500L, seed = NULL){

  if(is.data.frame(dt)){
    dt <- as.matrix(dt)
  }
  if(is.data.frame(dts)){
    dts <- as.matrix(dts)
  }

  if (!is.matrix(dts) && is.vector(dts, mode = "numeric" )){
    dts <- matrix(dts, nrow=1)
  }

  if(ncol(dts) != ncol(dt)){
    stop("Argument \"dts\ must have the same dimension as \"dt\"")
  }

  if(!(is.array(dt) && is.array(dts) && is.numeric(dt) && is.numeric(dts)  ))
    stop("Arguments \"dt\" and \"dts\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dt)) || any(!is.finite(dts)))
    stop("Missing or infinite values are not allowed in arguments \"dt\" and \"dts\".")

  if(!is.null(seed)){
    set.seed(seed)
  }
  # generate and scale directions
  m <- nrow(dts)
  n <- nrow(dt)
  d <- ncol(dt)

  u_matrix <- matrix(runif(d*n_projections,-1,1), n_projections, d)
  u_matrix <- u_matrix/sqrt(rowSums(u_matrix*u_matrix))

  depths <- double(m) # create vector to store answer
  # change inplace
  projectionDepth(dts, dt, u_matrix, m = m, n = n, d = d, k = n_projections,  depths = depths)
  return(depths)
}

