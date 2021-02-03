#' Random projection for multivariate data
#'
#'
#' Helper function to compute the random projection depth of multivariate point(s) with respect
#' to a multivariate data.
#'
#' @param dts A matrix or data frame of size \code{m} observations by \code{d} dimension or vector of length \code{d}.
#'   Contains the observation(s) whose depth is to be computed.
#' @param dt A matrix or dataframe of size \code{n} observations by \code{d} dimension. Equals to
#'   \code{dts} by default.
#' @param n_projections The number of directions for random projections. By default,
#'   500 random directions for projection are generated from a scaled uniform distribution between -1 and 1.
#' @param seed The random seed to set when generating the random directions. Defaults to NULL.
#'
#' @return A vector containing the depth values of \code{dts} with respect to \code{dt}.
#' @author Oluwasegun Taiwo Ojo
#' @export
#' @importFrom stats runif
#' @useDynLib fdaoutlier, .registration = TRUE, .fixes = "C_"
#' @seealso \code{\link{msplot}} for outlier detection using msplot and \code{\link{dir_out}}
#'  for directional outlyingness.
#' @examples
#' projection_depth(dts = iris[1:5, -5], dt = iris[1:10, -5], n_projection = 7, seed = 20)
#'
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
    stop("Argument \"dts\ must have the same dimension as \"dt\".")
  }

  if(!is.array(dt) || !is.array(dts) || !is.numeric(dt) || !is.numeric(dts))
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

  # u_matrix <- matrix(runif(d*n_projections,-1,1), d, n_projections)
  # u_matrix <- u_matrix/rep(sqrt(rowSums(u_matrix*u_matrix)), rep(d,n))
  # # d by n / n

  result = .Call(C_projectionDepth,
                 as.double(t(dts)),
                 as.double(t(dt)),
                 as.double(t(u_matrix)),
                 #as.double(u_matrix),
                 m, n, d, n_projections,
                 PACKAGE = "fdaoutlier")
  return(result)
}


