#' Massive Unspervised Outlier Detection (MUOD)
#'
#' MUOD finds outliers by computing for each functional data, a magnitude, amplitude
#'  and shape index. Outliers are then detected in each set of index and outliers
#'  found are classified as either a magnitude, shape or amplitude outlier.
#'
#' @param dts a matrix or dataframe of size \code{n} observation by \code{p} domain points.
#' @param cut_method a character value indicating method to use for finding indices cutoff.
#'  Must be either \code{"boxplot"} or \code{"tangent"}.
#'
#' @details
#' MUOD was proposed in Azcorra et al. (2020) \doi{10.1038/s41598-018-24874-2}
#' as a support method for finding influential users in a social network data.
#' It was also mentioned in Vinue and Epiphano (2020) \doi{10.1007/s11634-020-00412-9}
#' where it was compared with other functional outlier detection methods.
#'
#' MUOD computes for each curve three indices: amplitude, magnitude and shape indices.
#' Then a cutoff is determined for each set of indices and outliers are identified
#' in each set of index. Outliers identified in the magnitude indices are flagged
#' as magnitude outliers. The same holds true for the amplitude and shape indices.
#' Thus, the outliers are not only identified but also classified.
#'
#' @return Returns a list containing the following
#'   \item{\code{outliers}}{a vector containing the indices of outliers identified.}
#'   \item{\code{indices}}{a dataframe containing the shape, magnitude and amplitude indices}
#' @export
#'
#' @examples
#' dt1 <- simulation_model1()
#' md <- muod(dts = dt1$data)
#' str(md$outliers)
#' dim(md$indices)
#' @references
#' Azcorra, A., Chiroque, L. F., Cuevas, R., Anta, A. F., Laniado, H., Lillo, R. E., Romo, J., & Sguera, C. (2018).
#'  Unsupervised scalable statistical method for identifying influential users in online social networks.
#'   Scientific reports, 8(1), 1-7.
#'
#' @importFrom  stats predict rbinom rnorm sd smooth.spline
#' @importFrom grDevices boxplot.stats
muod <- function(dts,
                 cut_method = c("boxplot", "tangent")){
  if(is.data.frame(dts)){
    dts <- as.matrix(dts)
  }
  if(!is.array(dts) || !is.numeric(dts))
    stop("Argument \"dts\" must be a numeric matrix or dataframe.")
  if (any(!is.finite(dts))){
    stop("Missing or infinite values are not allowed in argument \"dts\"")
  }
  if((dm <- dim(dts))[1] < 3) stop("The number of curves must be greater than 2")
  n <- dm[1]
  p <- dm[2]

  cut_method <- match.arg(cut_method)
  indices <- muod_indices(dt = dts, n = n, p = p)
  outliers <- indices_outliers(indices, cut_method)
  return(list(outliers = outliers,
              indices = indices))
}


muod_indices <- function(dt, n, p, benchmark =  c(1, 0, 1)){
  # compute pre_indices: dt is n by p
  data_means <- rowMeans(dt, na.rm = T) # length n
  data_vars <- apply(dt, 1, var, na.rm = T) # length n
  data_sds <- apply(dt, 1, sd, na.rm = T)
  data2 <- dt - data_means # n by p - n
  #data2 <- datat - rep(data_means, rep(p, n))
  #data2 <- t(t(data) - data_means) #pre computed mean-distance data
  pre_ind <- .Call(C_corCovBlock,
                   as.double(data2), # parses the transpose to cpp
                   data_means,
                   data_vars,
                   data_sds,
                   1, # start of block
                   n, # end of block
                   n,
                   p,
                   PACKAGE = "fdaoutlier")
  pre_ind <- matrix(pre_ind, nrow = n, ncol = 3, byrow = T)
  pre_ind[,1] <- pre_ind[, 1]/data_sds
  pre_ind[,2] <- data_means - pre_ind[,2]
  pre_ind <- data.frame(pre_ind)
  colnames(pre_ind) <- c("shape", "magnitude", "amplitude")
  abs(as.data.frame(pre_ind - matrix(benchmark, n, 3, byrow = T)))
}


indices_outliers <- function(inds, cutm){
  outlier_types <- c("shape", "amplitude", "magnitude")
  sapply(outlier_types, function(outlier_name){
    metric <- sort(inds[,outlier_name])
    cutoff <- outlier_cutoff(sorted_index = metric, cutmm = cutm)
    # filter outliers
    outl <- which(inds[, outlier_name] > cutoff)
    # arrange according to size of index.
    # largest indices come first
    outl[order(inds[outl, outlier_name], decreasing = T)]

  }, USE.NAMES = T)
}


outlier_cutoff <- function(sorted_index, cutmm){
  if(cutmm == "boxplot"){
    cutoff <- boxplot.stats(sorted_index)$stats[5] # upper whisker is a data point
  }else if(cutmm == "tangent"){
    which_best <- tangent_cutoff(sorted_index)
    cutoff <- sorted_index[which_best]
  }else{
    stop(cutmm, " is not a valid cutting method.")
  }
  return(cutoff)
}

tangent_cutoff <- function(y){
  ly <- length(y)
  which_x = which.max(diff(y)) + 1
  x <- seq(0, 1, length = ly)
  spl <- smooth.spline(y ~ x)
  newx_0 <- x[which.min(diff(diff(y)))]
  newx_1 <- mean(x[c(which_x - 1, which_x)], na.rm = T)
  pred0 <- predict(spl, x = newx_0, deriv = 0)
  pred1 <- predict(spl, x = newx_1, deriv = 1)
  #slope correction
  pred1$y <- max(pred1$y, 1.5)

  y_intercept <- pred0$y - (pred1$y*newx_0)
  x_intercept <- -y_intercept/pred1$y
  ceiling(x_intercept * ly)

  # if(plot) {
  #   plot(x, y, type = "l", ylim = c(0, max(y)))
  #   abline(h = 0, col = 8)
  #   lines(spl, col = 2) # spline
  #   points(pred0, col = 2, pch = 19) # point to predict tangent
  #   lines(x, (y_intercept + pred1$y * x), col = 3) # tangent (1st deriv. of spline at newx)
  #   points(x_intercept, 0, col = 3, pch = 19) # x intercept
  # }

}
