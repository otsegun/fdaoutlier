#' Functional Boxplot for a sample of functions.
#'
#' This function finds outliers in a sample of curves using the functional boxplot by Sun and Genton (2011)
#' \doi{10.1198/jcgs.2011.09224}.
#' Unlike the name suggests, the function does not actually produce a plot but is only used as support in
#' finding outliers in other functions. Different depth and outlyingness methods are supported for ordering
#' functions. Alternatively, the depth values of the functions can be supplied directly.
#'
#' @param dts A matrix or data frame of size \eqn{n} observations/curves by \eqn{p} domain/evaluation points for univariate functional data.
#' @param depth_method A character value specifying the method to use for computing the depth values (if \code{depth_values} is not supplied)
#'  used in ordering the functions. The following methods are are supported:
#'  \describe{
#'   \item{"mbd":}{The modified band depth with bands defined by 2 functions.
#'   Uses the algorithm of Sun et al. (2012)\doi{10.1002/sta4.8}.}
#'   \item{"tvd"}{The total variation depth of Huang and Sun (2019) \doi{10.1080/00401706.2019.1574241}.}
#'   \item{"extremal"}{The extremal depth of Narisetty and Nair (2016) \doi{10.1080/01621459.2015.1110033}.}
#'   \item{"dirout"}{Uses the robust distance of the mean and variation of directional outlyingness (\code{\link{dir_out}})
#'   defined in Dai and Genton (2019) \doi{10.1016/j.csda.2018.03.017}.
#'   Since this method is a measure of outlyingness of a function the negative
#'    of the computed robust distance is used in ordering the functions.}
#'   \item{"linfinity"}{The L-infinity depth defined in Long and Huang (2015)
#'    \href{https://arxiv.org/abs/1506.01332}{<arXiv:1506.01332} is used in ordering functions.}
#'   \item{"bd"}{Uses the band depth with bands defined by 2 functions
#'   according to the algorithm of Sun et al. (2012) \doi{10.1002/sta4.8}.}
#'   \item{erld}{Uses the extreme rank length depth used in Dai et al. (2020) \doi{10.1016/j.csda.2020.106960}.}
#'   \item{"dq"}{Uses the directional quantile (DQ) used in Dai et al. (2020) \doi{10.1016/j.csda.2020.106960}.
#'    Since DQ is a measure of outlyingness, the negative of the DQ values is used in ordering the functions.}
#'    }
#'   The default method is \code{"mbd"}. Alternatively, the \code{depth_values} of the functions can be supplied in which case
#' the depths are not computed and \code{depth_method} is ignored.
#' @param depth_values A numeric vector containing the depth values of the functions in \code{dts} to use for ordering functions.
#'    \code{length(depth_values)} must be equal to number of rows of \code{dts}. If \code{depth_values} is specified, the depth is not
#'     computed and any method specified in \code{depth_method} is ignored.
#' @param emp_factor A numeric value specifying the empirical factor for the boxplot. Defaults to 1.5.
#' @param central_region A numeric value between 0 and 1 indicating the probability of central region. Defaults to 0.5.
#' @param erld_type If \code{depth_method = "erld"}, the type of ordering to use in computing the extreme rank length depth.
#' Can be one of \code{"two_sided"}, \code{"one_sided_left"} or \code{"one_sided_right"}. A \code{"two_sided"} ordering is used by
#' default if \code{erld_type} is not specified.
#' See \link{extreme_rank_length} for more details.
#' @param dq_quantiles If \code{depth_method = "dq"}, a numeric vector of length 2 specifying the probabilities
#'  of upper and lower quantiles. Defaults to \code{c(0.025, 0.975)} for the upper and lower 2.5\% quantiles.
#'  See \link{directional_quantile} for details.
#'
#'
#'
#' @return A list containing: \item{outliers}{The indices of the functions/curves flagged as outliers.}
#' \item{depth_values}{The depths of the functions/curves in \code{dts}.}
#' \item{median_curve}{The index of the median curve, which is the curve with the largest depth value (or smallest outlyingness value).}
#'
#' @references
#' Sun, Y., & Genton, M. G. (2011). Functional boxplots. \emph{Journal of Computational and Graphical
#'  Statistics}, 20(2), 316-334.
#'
#' @seealso \code{\link{seq_transform}} for functional outlier detection using  sequential transformation.
#'
#'@export
#' @examples
#' dt1 <- simulation_model1()
#' fbplot_obj <- functional_boxplot(dt1$data, depth_method = "mbd")
#' fbplot_obj$outliers
functional_boxplot <- function(dts,
                               depth_method = c("mbd", "tvd", "extremal", "dirout",
                                                 "linfinity", "bd", "erld", "dq"),
                               depth_values = NULL,
                               emp_factor = 1.5,
                               central_region = 0.5,
                               erld_type = NULL,
                               dq_quantiles = NULL){
  dm <- dim(dts)
  n <- dm[1]
  p <- dm[2]

  if (is.data.frame(dts)) {
    dt <- as.matrix(dts)
  }
  if (any(!is.finite(dts))) {
    stop("Missing or infinite values are not allowed in argument \"dts\"")
  }

  if (!is.array(dts) || !is.numeric(dts))
    stop("Argument \"dts\" must be a numeric matrix or dataframe.")



  if (length(dm) != 2) stop("Dimension of 'dts' must be of length 2. Only univariate functional data is supported.")

  if(is.null(depth_values)){
    depth_method <- match.arg(depth_method)
    if(depth_method == "mbd"){
      depth_values <- modified_band_depth(dts)
    }else if( depth_method == "tvd"){
      depth_values <- total_variation_depth(dts)$tvd
    }else if(depth_method == "extremal"){
      depth_values <- extremal_depth(dts)
    }else if(depth_method == "dirout"){
      depth_values <- -dir_out(dts, return_distance = T)$distance
    }else if(depth_method == "linfinity"){
      depth_values <- linfinity_depth(dts)
    }else if(depth_method == "bd"){
      depth_values <- band_depth(dts)
    } else if(depth_method == "erld"){
      if(is.null(erld_type)){
        warning("The 'type' argument for extreme rank length depth not specified. Using the default type of 'two_sided'. ")
        depth_values <- extreme_rank_length(dts)
      }else{
        depth_values <- extreme_rank_length(dts, type = erld_type)
      }

    }else if(depth_method == "dq"){
      if(is.null(dq_quantiles)){
        warning("Using the default quantile probabilites of 0.025 and 0.975 for directional quantile.")
        depth_values <- -directional_quantile(dts)
      }else{
        depth_values <- -directional_quantile(dts, quantiles = dq_quantiles)
      }
    }
  } else{
    if(length(depth_values) != n){
      stop("Length of argument 'depth_values' must be equal to the number of rows in 'dts'.")
    }
  }

  if(central_region >= 1 || central_region <= 0   ){
    stop("Argument 'central_region' must be greater than 0 and less than 1.")
  }



  sorted_depths <- sort(depth_values, decreasing = T, index.r = T)
  index_sorted_depth <- sorted_depths$ix
  sorted_depths <- sorted_depths$x
  median_curve <- index_sorted_depth[1]

  #repnp <- rep(n, p)

  n_obs_central <- ceiling(n*central_region) # at least 50%
  center <- dts[index_sorted_depth[1:n_obs_central], ]

  inf <- apply(center,2,min)
  sup <- apply(center,2,max)
  distt <- emp_factor*(sup-inf)
  upper <- sup + distt
  lower <- inf - distt
  dts <- t(dts)
  outlier_test <- (dts <= lower) + (dts >= upper)
  outliers <- which(colSums(outlier_test) > 0)
  return(list(outliers = unname(outliers),
              depth_values = depth_values,
              median_curve = median_curve))
}







