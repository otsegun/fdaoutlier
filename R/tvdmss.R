#' Find shape and magnitude outliers  using the Total Variation Depth
#'  and Modified Shape Similarity Index
#'
#' @param data A matrix of size \eqn{n} observations by \eqn{p} domain
#'   points.
#' @param n_curves The number of observations or curves. Set to \code{nrow(data)}
#'   by default.
#' @param n_points The number of domain/evaluation points. Set to \code{ncol(data)}
#'   by default.
#' @param emp_factor The empirical factor of the boxplot used on the modified shape
#'   similarity index.
#'
#' @returns
#' @export
#'
#' @examples
tvd_mss <- function(data,
                    n_curves = nrow(data),
                    n_points = ncol(data),
                    emp_factor = 1.5){
  depths_mss <- total_variation_depth(data = data,
                                      n_curves =  n_curves,
                                      n_points = n_points)
  tvd <- depths_mss$tvd
  mss <- depths_mss$mss

  # shape outliers
  shape_boxstats <- boxplot(tvd, range = emp_factor, plot=F);
  shape_outliers <- sapply(shape_boxstats$out, function(x) which(tvd == x))

  # magnitude outliers
  n_central_obs <- ceiling(n_curves/2)
  data_pure <- data[-shape_outliers, ]
  tvd_pure <- tvd[-shape_outliers]
  index_pure <- (1:n_curves)[-shape_outliers]
  sorted_index <- order(tvd_pure, decreasing = T)

  central_obs <- data_pure[sorted_index[1:n_central_obs], ]
  inf <- apply(central_obs, 2, min)
  sup <- apply(central_obs, 2, max)
  dist <- 1.5 * (sup - inf)
  upper_bound <- sup + dist
  lower_bound <- inf - dist

  outliers <- which(
    apply(data_pure, 1, function(x){
    any(x <= lower_bound) || any(x >= upper_bound)
  })
  )

  magnitude_outliers = index_pure[outliers]
  return(list(outliers = sort(c(magnitude_outliers, shape_outliers)),
              shape_outliers = shape_outliers,
              magnitude_outliers = magnitude_outliers,
              tvd = tvd,
              mss = mss))
}





