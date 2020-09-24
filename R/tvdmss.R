#' Find shape and magnitude outliers  using the Total Variation Depth
#'  and Modified Shape Similarity Index
#'
#' @param data A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#' @param emp_factor The empirical factor of the boxplot used on the modified shape
#'   similarity index.
#'
#' This method uses a combination of total variation depth (TVD) and modified shape similarity (MSS) index
#'  defined in Huang and Sun (2019) to find magnitude and shape outliers. The MSS of the observations are
#'  first computed and a classical boxplot applied on the indices. Outliers detected by the boxplot are
#'  flagged as shape outliers. The shape outliers are then removed from the data and then TVD of the
#'  remaning observations are used in a functional boxplot to detect magnitude outliers.
#'
#' @author Oluwasegun Ojo
#'
#' @references Huang, H., & Sun, Y. (2019). A decomposition of total variation depth for
#' understanding functional outliers. \emph{Technometrics}, 61(4), 445-458.
#'
#' @returns Returns a list contaning the following
#' \describe{
#'   \item{\code{outliers}}{the indices of the (shape and magnitude) outliers}
#'   \item{\code{shape_outliers}}{the indices of the shape outliers}
#'   \item{\code{magnitude_outliers}}{the indices of the magnitude outliers}
#'   \item{\code{tvd}}{the total variation depths of the observations of \code{data}}
#'   \item{\code{mss}}{the modified shape similarity index of the observations of \code{data}}
#'   }
#' @export
#' @seealso \code{\link{msplot}} for outlier detection using msplot.
#' @examples
#' data(sim_data1)
#' res <- tvd_mss(sim_data1$data)
tvd_mss <- function(data,
                    emp_factor = 1.5){

  depths_mss <- total_variation_depth(data = data)

  tvd <- depths_mss$tvd
  mss <- depths_mss$mss


  n_curves <- nrow(data)
  n_points <- ncol(data)
  index <- (1:n_curves)
  n_central_obs <- ceiling(n_curves/2)

  # shape outliers
  shape_boxstats <- boxplot(tvd, range = emp_factor, plot=F);
  shape_outliers = NULL

  if(length(shape_boxstats$out) != 0){
    shape_outliers <- sapply(shape_boxstats$out, function(x) which(tvd == x))
    data <- data[-shape_outliers, ]
    tvd <- tvd[-shape_outliers]
    index <- index[-shape_outliers]
  }

  sorted_index <- order(tvd, decreasing = T)

  central_obs <- data[sorted_index[1:n_central_obs], ]
  inf <- apply(central_obs, 2, min)
  sup <- apply(central_obs, 2, max)
  dist <- 1.5 * (sup - inf)
  upper_bound <- sup + dist
  lower_bound <- inf - dist

  outliers <- which(apply(data, 1, function(x){
    any(x <= lower_bound) || any(x >= upper_bound)
  }))

  magnitude_outliers = NULL
  if(length(outliers) != 0) magnitude_outliers = index[outliers]
  return(list(outliers = sort(c(magnitude_outliers, shape_outliers)),
              shape_outliers = shape_outliers,
              magnitude_outliers = magnitude_outliers,
              tvd = tvd,
              mss = mss))
}





