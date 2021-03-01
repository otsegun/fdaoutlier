#' Total Variation Depth and Modified Shape Similarity Index
#'
#'This function computes the total variation depth (tvd) and the modified shape similarity index (mss)
#'proposed in Huang and Sun (2019) \doi{10.1080/00401706.2019.1574241}.
#'
#' @param dts A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#'
#' @details
#' This function computes the total variation depth (TVD) and modified shape
#' similarity (MSS) index of a univariate functional data. The definition of the
#' estimates of TVD and MSS can be found in Huang and Sun (2019)
#' \doi{10.1080/00401706.2019.1574241}.
#' @return Returns a list containing the following
#'   \item{\code{tvd}}{the total variation depths of the observations of \code{dts}}
#'   \item{\code{mss}}{the modified shape similarity index of the observations of \code{dts}}
#'
#' @author Oluwasegun Ojo
#'
#' @references
#' Huang, H., & Sun, Y. (2019). A decomposition of total variation depth for
#' understanding functional outliers. \emph{Technometrics}, 61(4), 445-458.
#'
#' @seealso \code{\link{tvd_mss}} for outlier detection using TVD and MSS.
#'
#' @export
#'
#' @examples
#' dt6 <- simulation_model6()
#' tvd_object <- total_variation_depth(dt6$data)
#' @importFrom graphics boxplot
total_variation_depth <- function(dts){
  if(is.data.frame(dts)){
    dts <- as.matrix(dts)
  }

  #i

  if(!is.array(dts) || !is.numeric(dts))
    stop("Argument \"dts\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dts))){
    stop("Missing or infinite values are not allowed in argument \"dts\"")
  }

  if(nrow(dts) < 3) stop("The number of curves must be greater than 2")

  ddim <- dim(dts)
  n_curves  <- ddim[1]
  n_points = ddim[2]

  dtst <- t(dts)
  pointwise_ranks <- t(apply(dtst,1,rank))/n_curves
  total_variation <- pointwise_ranks * (1 - pointwise_ranks)
  tempres <- .Call(C_totalVariationDepth,
                          as.double(dts), as.double(dtst),
                          n_curves, n_points,
                          PACKAGE = "fdaoutlier")
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

