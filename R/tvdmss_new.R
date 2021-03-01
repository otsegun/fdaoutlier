#' Outlier detection using the total variation depth and modified shape similarity index.
#'
#' Find shape and magnitude outliers  using the Total Variation Depth and Modified
#' Shape Similarity Index proposed in Huang and Sun (2019) \doi{10.1080/00401706.2019.1574241}.
#'
#' @param dts,data A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#' @param emp_factor_mss The empirical factor of the classical boxplot used on the modified shape
#'   similarity index. Defaults to 1.5.
#' @param emp_factor_tvd The empirical factor of the functional boxplot used on the TVD of observations.
#'   Defaults to 1.5.
#' @param central_region_tvd A number between 0 and 1 indicating the central region probability of
#'   the functional boxplot used on the TVD of the observations. Defaults to 0.5. See also details.
#'
#'@details
#' This method uses a combination of total variation depth (TVD) and modified shape similarity (MSS) index
#'  defined in Huang and Sun (2019) \doi{10.1080/00401706.2019.1574241}
#'  to find magnitude and shape outliers. The TVD and MSS of all the observations are
#'  first computed and a classical boxplot is then applied on the MSS. Outliers detected
#'  by the boxplot of MSS are flagged as shape outliers. The shape outliers are then removed
#'  from the data and the TVD of the remaining observations are used in a functional boxplot
#'  to detect magnitude outliers.  The central region
#'  of this functional boxplot (\code{central_region_tvd}) is w.r.t. to the original number of curves. Thus if
#'  8 shape outliers are found out of 100 curves, specifying \code{central_region_tvd} = 0.5 will ensure that
#'  50 observations are used as the central region in the functional boxplot on the remaining 92 observations.
#'
#'
#' @returns Returns a list containing the following
#'   \item{\code{outliers}}{the indices of the (shape and magnitude) outliers}
#'   \item{\code{shape_outliers}}{the indices of the shape outliers}
#'   \item{\code{magnitude_outliers}}{the indices of the magnitude outliers}
#'   \item{\code{tvd}}{the total variation depths of the observations of \code{data}}
#'   \item{\code{mss}}{the modified shape similarity index of the observations of \code{data}}
#' @author Oluwasegun Ojo
#' @references
#' Huang, H., & Sun, Y. (2019). A decomposition of total variation depth for
#' understanding functional outliers. \emph{Technometrics}, 61(4), 445-458.
#'
#' @seealso \code{\link{msplot}} for outlier detection using msplot.
#'
#' @export
#'
#' @examples
#' dt6 <- simulation_model6()
#' res <- tvdmss(dt6$data)
#' res$outliers
#'
tvdmss <- function(dts,
                    emp_factor_mss = 1.5,
                    emp_factor_tvd = 1.5,
                    central_region_tvd = 0.5){

  depths_mss <- total_variation_depth(dts = dts)

  tvd <- tvd_old <- depths_mss$tvd
  mss <- depths_mss$mss


  dta_dim <- dim(dts)
  n_curves <- dta_dim[1]
  n_points <- dta_dim[2]
  index <- (1:n_curves)
  n_central_obs <- ceiling(n_curves/2)# set fbplot to .5

  # shape outliers
  shape_boxstats <- boxplot(mss, range = emp_factor_mss, plot=F);
  shape_outliers <- NULL

  if(length(shape_boxstats$out) != 0){
    #segun:
    #shape_outliers <- sapply(shape_boxstats$out, function(x) which(mss == x))
    shape_outliers <- which(mss %in% shape_boxstats$out[shape_boxstats$out < mean(mss)])
    dts <- dts[-shape_outliers, ]
    tvd <- tvd[-shape_outliers]
    index <- index[-shape_outliers]
  }

  magnitude_outliers  <- NULL
  # central region wrt to original number of curves
  outliers <- functional_boxplot(dts, depth_values = tvd,
                                 emp_factor = emp_factor_tvd,
                                 central_region = central_region_tvd*n_curves/nrow(dts))$outliers
  # sorted_index <- order(tvd, decreasing = T)
  #
  #
  #
  # central_obs <- data[sorted_index[1:n_central_obs], ]
  # inf <- apply(central_obs, 2, min)
  # sup <- apply(central_obs, 2, max)
  # dist <- 1.5 * (sup - inf)
  # upper_bound <- sup + dist
  # lower_bound <- inf - dist
  #
  # outliers <- which(apply(data, 1, function(x){
  #   any(x <= lower_bound) || any(x >= upper_bound)
  # }))


  if(length(outliers) != 0) magnitude_outliers = index[outliers]
  return(list(outliers = sort(c(magnitude_outliers, shape_outliers)),
              shape_outliers = shape_outliers,
              magnitude_outliers = magnitude_outliers,
              tvd = tvd_old,
              mss = mss))
}





