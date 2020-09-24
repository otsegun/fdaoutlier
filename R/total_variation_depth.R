#' Compute the Total Variation Depth and Modified Shape Similarity Index
#'
#'
#' @param data A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#'
#' @details
#' This function computes the total variation depth (TVD) and modified shape similarity (MSS) index of a univariate
#' functional data. The definition of the estimates of TVD and MSS can be found in Huang and Sun (2019).
#' @return Returns a list contaning the following
#' \describe{
#'   \item{\code{tvd}}{the total variation depths of the observations of \code{data}}
#'   \item{\code{mss}}{the modified shape similarity index of the observations of \code{data}}
#'   }
#'
#' @author Oluwasegun Ojo
#' @references
#' Huang, H., & Sun, Y. (2019). A decomposition of total variation depth for
#' understanding functional outliers. \emph{Technometrics}, 61(4), 445-458.
#'
#' @seealso \code{\link{tvd_mss}} for outlier detection using TVD and MSS.
#'
#' @export
#'
#' @examples
#' data(sim_data1)
#' tvd_object <- total_variation_depth(sim_data1$data)
#' @importFrom graphics boxplot
total_variation_depth <- function(data){
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }

  #i

  if(!is.array(data) || !is.numeric(data))
    stop("Argument \"data\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(data))){
    stop("Missing or infinite values are not allowed in argument \"data\"")
  }

  if(nrow(data) < 3) stop("The number of curves must be greater than 2")

  n_curves = nrow(data)
  n_points = ncol(data)

  data_t <- t(data)
  pointwise_ranks <- t(apply(data_t,1,rank))/n_curves
  total_variation <- pointwise_ranks * (1 - pointwise_ranks)
  tempres <- .Call(C_totalVariationDepth,
                          as.double(data), as.double(data_t),
                          n_curves, n_points,
                          PACKAGE = "fdalite")
  shape_variation <- matrix(tempres$shape_variation, n_points - 1, n_curves, byrow = T)
  diff_data <- matrix(tempres$difference_data, nrow = n_curves, ncol = n_points, byrow = T)[,-1]
  #row_sum_diff_data <- rowSums(diff_data)
  v_weights <- t(diff_data/rowSums(diff_data))

  tvd = colMeans(total_variation);
  mss <- colSums(shape_variation * v_weights)
  return(list(tvd = tvd, mss = mss))
  # return(list(shape_variation = shape_variation,
  #             vw = v_weights))
}

