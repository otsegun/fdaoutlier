#' Outlier Detection using Magnitude-Shape Plot (MS-Plot) based on the directional outlyingness for functional
#' data.
#'
#' This function finds outliers in univariate and multivariate functional data using the MS-Plot
#' method described in Dai and Genton (2018) \doi{10.1080/10618600.2018.1473781}.
#' Indices of observations flagged as outliers are returned. In addition, the
#' scatter plot of \eqn{VO} against \eqn{MO} (||MO||) can be requested for univariate
#' (multivariate) functional data.
#'
#'
#' @param dts A matrix/data frame for univariate functional data (of size \eqn{n}
#'  observations by \eqn{p} domain points) or a \eqn{3-}dimensional array for
#'  multivariate functional data (of size \eqn{n} observations by \eqn{p}
#'  domain points by \eqn{d} dimension).
#' @param data_depth The depth used in the computation of the directional outlyingness of
#'   \code{dts}. The projection depth is always used. Support for other depth methods will be added.
#' @param n_projections The number of random directions to generate for computing the random projection
#'   depth. By default 200 directions are generated.
#' @param seed An integer indicating the seed to set when generating random directions for computing the random projection depth.
#'   NULL by default in which case no seed is set.
#'
#' @param return_mvdir A logical value indicating whether to return the mean and variation of directional
#'  outlyingness (\eqn{MO} and \eqn{VO}). For univariate functional data, \eqn{MO} and \eqn{VO} are vectors.
#'  For multivariate functional data, \eqn{VO} is a vector while \eqn{MO} is a matrix of size
#'   \eqn{n x d}.
#' @param plot A logical indicating whether to make the msplot of \eqn{VO} against \eqn{MO}. In the case
#'  of multivariate functional data, a plot of  \eqn{VO} against \eqn{||MO||} is made.
#' @param plot_title The title of the plot. Set to "Magnitude Shape Plot" by default. Ignored if
#' \code{plot = FALSE}.
#' @param title_cex Numerical value indicating the size of the plot title relative to the device default.
#' Set to 1.5 by default. Ignored if \code{plot = FALSE}.
#' @param show_legend A logical indicating whether to add legend to plot if \code{plot = TRUE}.
#' @param xlabel The label of the x-axis if \code{plot = TRUE}. If not specified (default), set to "MO"
#'  for univariate functional data and "||MO||" for multivariate functional data.
#' @param ylabel The label of the y-axis. Set to "VO" by default.
#'
#'
#'
#' @details
#'
#' MS-Plot finds outliers by computing
#' the mean and variation of directional outlyingness (\eqn{MO} and \eqn{VO}) described
#' in Dai and Genton (2019) \doi{10.1016/j.csda.2018.03.017}.
#' A multivariate data whose columns are the computed \eqn{MO} and \eqn{VO} is then constructed and
#' the robust mahalanobis distance(s) of the rows of this matrix are computed
#' (using the minimum covariate determinant estimate of the location and scatter). The tail
#' of the distribution of these distances is approximated using the \eqn{F} distribution
#' according to Hardin and Rocke (2005) \doi{10.1198/106186005X77685} to get the cutoff.
#' The projection depth is always used for computing the directional outlyingness
#' (as suggested by Dai and Genton (2019) \doi{10.1016/j.csda.2018.03.017}).
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
#' dt1 <- simulation_model1()
#' msplot_object <- msplot(dts = dt1$data)
#' msplot_object$outliers_index
#' msplot_object$mean_outlyingness
#' msplot_object$var_outlyingness
#'
#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics axis legend mtext par points
msplot <- function(dts,
                   data_depth = c("random_projections"),
                   n_projections = 200, seed = NULL,
                   return_mvdir = TRUE,
                   plot = TRUE,
                   plot_title = "Magnitude Shape Plot",
                   title_cex = 1.5,
                   show_legend = T,
                   ylabel = "VO",
                   xlabel) {
  ### pairwise plots of variation of outlyingness (VO) against mean outlyingness (MO)###
  data_dim  <- dim(dts)
  #data_depth <- match.arg(data_depth)
  n <- data_dim[1]
  dir_result <- dir_out(dts, data_depth = data_depth,
                        n_projections = n_projections, seed = seed)

  if (length(data_dim) == 2){ # univariate
    dist <- dir_result$distance
    rocke_factors <- hardin_factor_numeric(n, 2)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$factor2 # C in paper
    cutoff_value <- rocke_cutoff/rocke_factor1 #rocke_cutoff/rocke_factor1
    outliers_index <- which(dist > cutoff_value)
    median_curve <- which.min(dist)
    if (plot){
      # mo and vo
      myx <- dir_result$mean_outlyingness
      myy <- dir_result$var_outlyingness
      if(missing(xlabel)) xlabel <- "MO"
    }

  } else if (length(data_dim) == 3){ # multivariate
    d <- data_dim[3]
    rocke_factors  <- hardin_factor_numeric(n = n, dimension = d + 1)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$factor2

    cutoff_value  <- rocke_cutoff/rocke_factor1
    outliers_index <- which(dir_result$distance > cutoff_value)
    median_curve <- which.min(dir_result$distance)

    if (plot){
      # mo and vo
      myx <- sqrt(rowSums(dir_result$mean_outlyingness^2, na.rm = T))
      myy <- dir_result$var_outlyingness
      if(missing(xlabel)) xlabel <- "||MO||"
    }

  }
  if(plot){
    # plot area
    plot(myx, myy, type = "n", xlab = xlabel,
         ylab = ylabel, xlim = range(myx) + c(-sd(myx), 1.5*sd(myx) ),
         ylim = range(myy) + c(-.2*sd(myy), 1*sd(myy) ), axes = F,
         col.lab = "gray20")
    #add axis
    axis(1, col = "white", col.ticks = "grey61", lwd.ticks = .5, tck = -0.025,
         cex.axis = 0.9, col.axis = "gray30")
    axis(2,col = "white", col.ticks = "grey61",  lwd.ticks = .5, tck = -0.025,
         cex.axis = 0.9, col.axis = "gray30")
    grid(col = "grey75", lwd = .3)
    box(col = "grey51")

    if(length(outliers_index > 0)){
      points(myx[-outliers_index], myy[-outliers_index], bg = "gray60", pch = 21)
      points(myx[outliers_index], myy[outliers_index], pch = 3)
    } else{
      points(myx, myy, bg = "gray60", pch = 21)
    }
    mtext(plot_title,3, adj = 0.5, line = 1, cex = title_cex, col = "gray20")
    # mtext(paste0(length(outliers_index), " outliers detected"),
    #       1, adj = 1, line = 3, cex =.8, font = 3, col = "gray41")
    if(show_legend){
      legend("topright", legend = c("normal", "outlier"),
             pch = c(21, 3), cex = 1,
             pt.bg = "gray60", col = "gray0",
             text.col = "gray30", bty = "n",
             box.lwd = .1, xjust = 0, inset = .01)
    }
  }


  if (return_mvdir){
    return(list(outliers = outliers_index,
                median_curve = median_curve,
                mean_outlyingness = dir_result$mean_outlyingness,
                var_outlyingness = dir_result$var_outlyingness))


  } else{
    return(list(outliers = outliers_index,
                median_curve = median_curve))
  }

}
