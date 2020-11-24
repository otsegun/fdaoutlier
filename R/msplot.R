#' Outlier Detection using Magnitude-Shape Plot (MS-Plot) based on the directional outlyingness for functional
#' data.
#'
#' This function finds outliers in univariate and multivariate functional data using the MS-Plot
#' method described in Dai and Genton (2018)\href{https://doi.org/10.1080/10618600.2018.1473781}{<doi:10.1080/10618600.2018.1473781>}.
#' Indices of observations flagged as outliers are returned. Despite the name, this function does not produce a plot.
#' However, the mean and variation of directional outlyingness (\eqn{MO} and \eqn{VO}) can be requested and a subsequent
#' plot of \eqn{MO} against \eqn{VO} can be easily generated using any preferred plotting tool.
#'
#' @param data A matrix/data frame for univariate functional data (of size \eqn{n} observations by \eqn{p} domain
#'   points) or a \eqn{3-}dimensional array for multivariate functional data (of size \eqn{n}
#'   observations by \eqn{p} domain points by \eqn{d} dimension).
#' @param data_depth The depth used in the computation of the directional outlyingness of
#'   \code{data}. The projection depth is always used. Support for other depth methods will be added.
#' @param n_projections The number of random directions to generate for computing the random projection
#'   depth. By default 200 directions are generated.
#' @param seed An integer indicating the seed to set when generating random directions for computing the random projection depth.
#'   NULL by default in which case no seed is set.
#'
#' @param return_mvdir A logical scalar indicating whether to return the mean and variation of directional
#'  outlyingness (\eqn{MO} and \eqn{VO}). For univariate functional data, \eqn{MO} and \eqn{VO} are vectors.
#'  For multivariate functional data, \eqn{VO} is a vector while \eqn{MO} is a matrix of size
#'   \eqn{n x d}.
#'
#' @details
#'
#' MS-Plot finds outliers by computing
#' the mean and variation of directional outlyingness (\eqn{MO} and \eqn{VO}) described
#' in Dai and Genton (2019)\href{https://doi.org/10.1016/j.csda.2018.03.017}{<doi:10.1016/j.csda.2018.03.017>}.
#' A multivariate data whose columns are the computed \eqn{MO} and \eqn{VO} is then constructed and
#' the robust mahalanobis distance(s) of the rows of this matrix are computed
#' (using the minimum covariate determinant estimate of the location and scatter). The tail
#' of the distribution of these distances is approximated using the \eqn{F} distribution
#' according to Hardin and Rocke (2005)\href{https://doi.org/10.1198/106186005X77685}{<doi:10.1198/106186005X77685>} to get the cutoff. The projection depth is always used
#' for computing the directional outlyingness (as suggested by Dai and Genton (2019)\href{https://doi.org/10.1016/j.csda.2018.03.017}{<doi:10.1016/j.csda.2018.03.017>}).
#'
#'
#' @return Returns a list containing:
#'  \item{outliers_index}{an integer vector containing the indices of the outliers.}
#'   \item{median_curve}{the index of the median function (which is the
#'   function with the smallest robust mahalanobis distance computed from the matrix whose
#'   columns are made up of \eqn{MO} and \eqn{VO}).}
#'   \item{mean_outlyingness}{if \code{return_mvdir} = TRUE, a numeric vector of the mean of directional
#'   outlyingness for univariate functional data or an \eqn{n x d} matrix of the mean of
#'   directional outlyingness for multivariate functional data.}
#'   \item{var_outlyingness}{if \code{return_mvdir} = TRUE, a numeric vector of length \eqn{n} observations
#'    containing the variation of directional outlyingness.}
#'
#'
#' @author Oluwasegun Taiwo Ojo.
#'
#' @references Dai, W., and Genton, M. G. (2018). Multivariate functional data
#'   visualization and outlier detection. \emph{Journal of Computational and Graphical
#'   Statistics}, 27(4), 923-934.
#'
#'   Dai, W., and Genton, M. G. (2019). Directional outlyingness for multivariate
#'   functional data. \emph{Computational Statistics & Data Analysis}, 131, 50-65.
#'
#'   Hardin, J., and Rocke, D. M. (2005). The distribution of robust distances.
#'   \emph{Journal of Computational and Graphical Statistics}, 14(4), 928-946.
#'
#' @seealso \code{\link{dir_out}} for directional outlyingness and \code{\link{projection_depth}}
#'  for multivariate projection depth.
#'
#' @examples
#' # Univariate magnitude model in Dai and Genton (2018).
#' data(sim_data1)
#' msplot_object <- msplot(data = sim_data1$data)
#' msplot_object$outliers_index
#' msplot_object$mean_outlyingness
#' msplot_object$var_outlyingness
#'
#' @export
#' @importFrom grDevices rgb
msplot <- function(data,
                   data_depth = c("random_projections"),
                   n_projections = 200, seed = NULL,
                   return_mvdir = TRUE) {
  ### pairwise plots of variation of outlyingness (VO) against mean outlyingness (MO)###
  data_dim  <- dim(data)
  #data_depth <- match.arg(data_depth)
  #if(plot) plot_type <- match.arg(plot_type)
  n <- data_dim[1]
  dir_result <- dir_out(data, data_depth = data_depth, n_projections = n_projections, seed = seed)

  # univariate
  if (length(data_dim) == 2){
    dist <- dir_result$distance
    rocke_factors <- hardin_factor_numeric(n, 2)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$factor2 # C in paper
    cutoff_value <- rocke_cutoff/rocke_factor1 #rocke_cutoff/rocke_factor1
    outliers_index <- which(dist > cutoff_value)
    median_curve <- which.min(dist)
    if (return_mvdir){
      return(list(outliers_index = outliers_index,
                  median_curve = median_curve,
                  mean_outlyingness = dir_result$mean_outlyingness,
                  var_outlyingness = dir_result$var_outlyingness))


    } else{
      return(list(outliers_index = outliers_index,
                  median_curve = median_curve))
    }

  } else if (length(data_dim) == 3){
    d <- data_dim[3]
    rocke_factors  <- hardin_factor_numeric(n = n, dimension = d + 1)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$factor2

    cutoff_value  <- rocke_cutoff/rocke_factor1
    outliers_index <- which(dir_result$distance > cutoff_value)
    median_curve <- which.min(dir_result$distance)

    if (return_mvdir){
      return(list(outliers_index = outliers_index,
                  median_curve = median_curve,
                  mean_outlyingness = dir_result$mean_outlyingness,
                  var_outlyingness = dir_result$var_outlyingness))
    }else{
      return(list(outliers_index = outliers_index,
                  median_curve = median_curve))
    }
  }}
