#'Dai & Genton (2019) Directional outlyingness for univariate or multivariate functional data.
#'
#'
#'Compute the directional outlyingness of a univariate or multivariate functional data
#'based on Dai and Genton (2019) \doi{10.1016/j.csda.2018.03.017}
#'and Dai and Genton (2018) \doi{10.1080/10618600.2018.1473781}.
#'
#'@param dts A matrix (or data frame) for univariate functional data (of size \code{n} observations by \code{p} domain
#'  points) or a 3-dimensional array for multivariate functional data (of size \code{n}
#'  observations by \code{p} domain points by \code{d} dimension).
#'
#'@param data_depth The method for computing the depth. The random projection depth is
#'  always used as suggested in Dai and Genton (2018) \doi{10.1080/10618600.2018.1473781}.
#'  Support for more depth methods will be added.
#'
#'@param n_projections The number of directions for computing random projection depth. By default,
#'   200 random directions are generated from a scaled uniform distribution between -1 and 1.
#'@param seed An integer indicating the seed to set when generating the random directions. Defaults to NULL
#' in which case a seed is not set.
#'
#'@param return_distance A logical value. If TRUE, returns: a matrix whose columns are
#'  the mean and variation of directional outlyingness, the mahalanobis distance of the
#'  observations of this matrix, and the robust estimate of the mean and covariance of
#'  this matrix (computed using the minimum covariance determinant method).
#'
#'@param return_dir_matrix A logical value. If TRUE, returns the directional outlyingness
#'  matrix (or array for multivariate functional data). Computed from the chosen \code{depth_depth}.
#'
#'@details
#'
#'The directional outlyingness, as defined in Dai and Genton (2019)
#'\doi{10.1016/j.csda.2018.03.017} is
#'\deqn{O(Y, F_Y) = (1/d(Y, F_Y) - 1).v} where \eqn{d} is a depth notion, and \eqn{v} is
#'the unit vector pointing from the median of \eqn{F_Y} to \eqn{Y}. For univariate and
#'multivariate functional data, the projection depth is always used as suggested by
#'Dai and Genton (2019) \doi{10.1016/j.csda.2018.03.017}.
#'
#'
#'@return Returns a list containing:
#' \item{mean_outlyingness}{an \code{n x d} matrix of the mean
#'  of directional outlyingness.} \item{var_outlyingness}{ a vector of length n containing
#'  the variation of directional outlyingness.}
#'  \item{ms_matrix}{ if\code{return_distance} = T, an \code{n x (d+1)} matrix whose columns are the mean and
#'  variation of directional outlyingness.}
#'  \item{distance}{ if \code{return_distance} = T, a vector of distance computed from the
#'  \code{ms_matrix} using a robust estimate of the mean and covariance matrix.}
#'  \item{mcd_obj}{ if \code{return_distance} = T, a list containing the robust
#'  (minimum covariance determinant) estimate of the mean and covariance of the \code{ms_matrix}.}
#'  \item{dirout_matrix}{ if \code{return_dir_matrix} = T, an n x p (or n x p x d) matrix (or array)
#'   containing the directional outlyingness values for the univariate (or multivariate) functional \code{dts}.}
#'@author Oluwasegun Taiwo Ojo.
#'
#'@references Dai, W., and Genton, M. G. (2018). Multivariate functional data
#'visualization and outlier detection. \emph{Journal of Computational and Graphical
#'Statistics}, 27(4), 923-934.
#'
#'Dai, W., and Genton, M. G. (2019). Directional outlyingness for multivariate functional
#'data. \emph{Computational Statistics & Data Analysis}, 131, 50-65.
#'
#'Zuo, Y. (2003). Projection-based depth functions and associated medians. \emph{The
#'Annals of Statistics}, 31(5), 1460-1490.
#'
#'@seealso \code{\link{msplot}} for outlier detection using msplot and \code{\link{projection_depth}}
#'  for multivariate projection depth.
#'
#' @examples
#' # univariate magnitude model in Dai and Genton (2018).
#' dt4 <- simulation_model4()
#' dirout_object <- dir_out(dts = dt4$data, return_distance = TRUE)
#'@export
#'@importFrom stats mad mahalanobis median var
#'@importFrom MASS cov.rob

dir_out <- function(dts, data_depth = "random_projections",
                    n_projections = 200L, seed = NULL,
                    return_distance = TRUE,
                    return_dir_matrix = FALSE){
  # library used: Mass::cov_rob,
  data_dim  <-  dim(dts)
  #data_depth <- match.arg(data_depth)
  if(is.data.frame(dts)){
    dts <- as.matrix(dts)
  }
  if(!is.array(dts))
    stop("Argument \"dts\" must be a dataframe, a matrix or 3-dimensional array.")

  if (any(!is.finite(dts)))
    stop("Missing or infinite values are not allowed in argument \"dts\".")

  if(data_dim[1] < 3){
    stop("n must be greater than 3.")
  }

  if (length(data_dim) == 2){ # univariate
    #############################################################3
    #median_vec <- apply(dt, 2, median)
    #mad_vec <- apply(dt, 2, mad)
    #dir_out_matrix <- sweep(sweep(dt, 2, median_vec), 2, mad_vec, FUN = "/")
    ################################################
    dts  <- t(dts)
    median_vec <- apply(dts, 1, median)
    mad_vec <- apply(dts, 1, mad)
    dir_out_matrix <- t((dts-median_vec)/(mad_vec)) # dir_out_matrix <- (dts-median_vec)/(mad_vec)
    mean_dir_out <- apply(dir_out_matrix, 1, mean, na.rm = T) # colMeans(dir_out_matrix, na.rm = T)
    var_dir_out <- apply(dir_out_matrix, 1, var, na.rm = T) # apply(dir_out_matrix, 2, var, na.rm = T)

    if(return_distance){
      ms_matrix <- (cbind(mean_dir_out, var_dir_out))
      mcd_obj  <- cov.rob(ms_matrix, method = "mcd", nsamp = "best")
      robust_cov <- mcd_obj$cov
      robust_mean <- (mcd_obj$center)
      distance <- unname(mahalanobis(ms_matrix, robust_mean, robust_cov))
    }
  } else if (length(data_dim) == 3) {# multivariate
    n <- data_dim[1]
    p <- data_dim[2]
    d <- data_dim[3]
    #dir_out_matrix2  <- array(0, dim = c(n, p, d))  # to cpp
    if(data_depth == "random_projections"){
      dir_out_matrix <- apply(dts, 2, function(x){
        outlyingness <- (1/projection_depth(x, n_projections = n_projections, seed = seed)) - 1
        median_vec  <-  x[order(outlyingness)[1], ] # dts[order(outlyingness)[1],j,]
        median_dev <- sweep(x, 2, median_vec )
        spatial_sign <- sqrt(rowSums((median_dev)^2))
        spatial_sign <- median_dev/spatial_sign
        spatial_sign[!is.finite(spatial_sign[,1]), ] <- 0 # check which row has an nan or infinite
        spatial_sign * outlyingness
      })
      #dir_out_matrix <- t(dir_out_matrix)
      #dim(dir_out_matrix) <- c(p, n, d)
      #dir_out_matrix <- aperm(dir_out_matrix, c(2, 1, 3))
      dir_out_matrix <- aperm(`dim<-`(t(dir_out_matrix), c(p, n, d)), c(2, 1, 3))

    } else {
      stop("depth method not supported.")
    }
    mean_dir_out  <- apply(dir_out_matrix, c(1,3), mean, na.rm = T)
    var_dir_out <- (apply(dir_out_matrix^2, 1, sum, na.rm = T)/p) - rowSums(mean_dir_out^2, na.rm = T)
    if (return_distance){
      ms_matrix <- (cbind(mean_dir_out, var_dir_out))
      mcd_obj  <- MASS::cov.rob(ms_matrix, method = "mcd", nsamp = "best")
      robust_cov <- mcd_obj$cov
      robust_mean <- (mcd_obj$center)
      distance <- mahalanobis(ms_matrix, robust_mean, robust_cov)
    }
  }
  else{
    stop("A 2-dimensional or 3-dimensional array is required.")
  }
  if (return_distance){
    if (return_dir_matrix){
      return(list(mean_outlyingness = unname(mean_dir_out),
                  var_outlyingness = unname(var_dir_out),
                  distance = distance,
                  ms_matrix = unname(ms_matrix),
                  mcd_obj = mcd_obj,
                  dirout_matrix = dir_out_matrix))
    } else{
      return(list(mean_outlyingness = unname(mean_dir_out),
                  var_outlyingness = unname(var_dir_out),
                  distance = distance,
                  ms_matrix = unname(ms_matrix),
                  mcd_obj = mcd_obj))
    }
  }
  else{
    if(return_dir_matrix){
      return(list(mean_outlyingness = unname(mean_dir_out),
                  var_outlyingness = unname(var_dir_out),
                  dirout_matrix = dir_out_matrix))
    }
    else{
      return(list(mean_outlyingness = unname(mean_dir_out),
                  var_outlyingness = unname(var_dir_out)))
    }

  }

}
