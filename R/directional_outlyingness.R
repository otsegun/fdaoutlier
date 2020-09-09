#'Directional outlyingness for univariate or multivariate functional data.
#'
#'
#'Compute the directional outlyingness of a univariate or multivariate functional data
#'based on Dai and Genton (2018).
#'
#'@param data A matrix for univariate functional data (of size n observations by p domain
#'  points) or a 3-dimensional array for multivariate functional data (of size n
#'  observations by p domain points by d dimension).
#'
#'@param data_depth The method for computing the depth. The random projection depth is
#'  always used as suggested in Dai and Genton (2018). Support more depth methods will be added.
#'
#'@param return_distance A logical scalar. If TRUE, returns the matrix whose columns are
#'  the mean and variation of directional outlyiness, the mahalanobis distance of the
#'  observations of this matrix, and the robust estimate of the mean and covariance of
#'  this matrix (computed using the minimum covariance determinant method).
#'
#'@param return_dir_matrix A logical scalar. If TRUE, returns the directional outlyingness
#'  matrix (or array for multivariate functional data). Computed from the chosen \code{depth_depth}.
#'
#'@details
#'
#'This function computes the directional outlyingness of a univariate or multivariate
#'functional data. The directional outlyingness, as defined by Dai and Genton (2018) is
#'\deqn{O(Y, F_Y) = (1/d(Y, F_Y) - 1).v} where \eqn{d} is a depth notion, and \eqn{v} is
#'the unit vector pointing from the median of \eqn{F_Y} to \eqn{Y}. For univariate
#'functional data, the projection depth based on Zuo (2003) is always used as suggested by
#'Dai and Genton (2019) while for multivariate functional data, any of "mahalanobis",
#'"random_projections", "simplicial", or "half_space" depths can be used.
#'
#'
#'@return Returns a list containing: \item{mean_outlyingness}{ an n x d matrix of the mean
#'  of directional outlyingness.} \item{var_outlyingness}{ a vector of length n containing
#'  the variation of directional outlyingness.} \item{ms_matrix}{ if
#'  \code{return_distance} = T, an n x (d+1) matrix whose columns are the mean and
#'  variation of directional outlyiness.} \item{distance}{ if \code{return_distance} = T,
#'  a vector of distance computed from the \code{ms_matrix} using the robust estimate of
#'  the mean and covariance.} \item{mcd_obj}{ if \code{return_distance} = T, a list
#'  containing the robust (minimum covariance determinant) estimate of the mean and
#'  covariance of the \code{ms_matrix}.} \item{dirout_matrix}{ if \code{return_dir_matrix}
#'  = T, an n x p (x d) matrix (array) containing the directional outlyingness values for
#'  the univariate (multivariate) functional \code{data}.}
#'@author Version created by Oluwasegun Taiwo Ojo based on the original code written by
#'Wenlin Dai and Marc G. Genton.
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
#'@seealso
#'
#' @examples
#' # univariate magnitude model in Dai and Genton (2018).
#' data(sim_data1)
#' dirout_object <- dir_out(data = sim_data1$data, return_distance = TRUE)
#'
#'\dontrun{
#' # spanish weather data multivariate functional data in Dai and Genton (2018)
#' data(aemet)
#' # smooth data with bsplines 11 basis for temperature
#' bsp11 <- fda::create.bspline.basis(aemet$temp$rangeval, nbasis=11)
#' s_bsp11  <-  fda.usc::S.basis(aemet$temp$argvals, bsp11)
#' sdata1 <- aemet$temp$data \%*\% s_bsp11
#' # for log precipitation
#' bsp11 <- fda::create.bspline.basis(aemet$logprec$rangeval, nbasis=11)
#' s_bsp11  <-  fda.usc::S.basis(aemet$logprec$argvals, bsp11)
#' sdata2 <- aemet$logprec$data \%*\% s_bsp11
#' # set up array data of dimension n x p x d
#' n <- dim(aemet$temp)[1]; p <- dim(aemet$temp)[2]
#' data_multiv <- array(0, dim = c(n, p, 2),
#'  dimnames = list(c(1:n), c(1:p), c("temp", "log_prep")))
#' data_multiv[,,1] <- sdata1; data_multiv[,,2] <- sdata2
#' # run directional outlyingness
#' dirout_object <- dir_out(data = data_multiv, return_distance = TRUE)
#' }
#'@export
#'@importFrom stats mad mahalanobis median var

dir_out <- function(data, data_depth = "random_projections",
                    return_distance = TRUE, return_dir_matrix = FALSE){
  # library used: Mass::cov_rob,
  data_dim  <-  dim(data)
  #data_depth <- match.arg(data_depth)
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }
  if(!is.array(data))
    stop("Data must be a dataframe, a matrix or 3-dimensional array.")

  if (any(is.na(data)) || any(is.infinite(data)))
    stop("Missing or infinite values are not allowed.")

  if(data_dim[1] < 3){
    stop("n must be greater than 3.")
  }

  if (length(data_dim) == 2){ # univariate
    #############################################################3
    #median_vec <- apply(dt, 2, median)
    #mad_vec <- apply(dt, 2, mad)
    #dir_out_matrix <- sweep(sweep(dt, 2, median_vec), 2, mad_vec, FUN = "/")
    ################################################
    data  <- t(data)
    median_vec <- apply(data, 1, median)
    mad_vec <- apply(data, 1, mad)
    dir_out_matrix <- t((data-median_vec)/(mad_vec)) # dir_out_matrix <- (data-median_vec)/(mad_vec)
    mean_dir_out <- apply(dir_out_matrix, 1, mean, na.rm = T) # colMeans(dir_out_matrix, na.rm = T)
    var_dir_out <- apply(dir_out_matrix, 1, var, na.rm = T) # apply(dir_out_matrix, 2, var, na.rm = T)

    if(return_distance){
      ms_matrix <- (cbind(mean_dir_out, var_dir_out))
      mcd_obj  <- MASS::cov.rob(ms_matrix, method = "mcd", nsamp = "best")
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
      dir_out_matrix <- apply(data, 2, function(x){
        outlyingness <- (1/projection_depth(x, n_projections = 500, seed = 20)) - 1
        median_vec  <-  x[order(outlyingness)[1], ] # data[order(outlyingness)[1],j,]
        median_dev <- sweep(x, 2, median_vec )
        spatial_sign <- sqrt(rowSums((median_dev)^2))
        spatial_sign <- median_dev/spatial_sign
        spatial_sign[!is.finite(spatial_sign[,1]), ] <- 0 # check which row has an nan or infinite
        spatial_sign * outlyingness
      })
      dir_out_matrix <- aperm(`dim<-`(t(dir_out_matrix), list(p, n, d)), c(2, 1, 3))

    } else if(data_depth == "mahalanobis") {
      cat("support for mahalanobis depth not yet added")
      # for (j in 1:p) {
      #   outlyingness  <- (1/fda.usc::mdepth.MhD(data[,j,])$dep) - 1
      #   median_vec  <-  data[order(outlyingness)[1],j,]
      #   median_dev <- sweep(data[,j,], 2, median_vec ) #t(data[,j,])-median_obs
      #   spatial_sign <- rowSums((median_dev)^2)^(1/2) #sqrt(rowSums((median_dev)^2))
      #   spatial_sign <- median_dev/spatial_sign
      #   spatial_sign[!is.finite(spatial_sign[,1]), ] <- 0 # check which row has an nan or infinite
      #   dir_out_matrix[,j,] <- spatial_sign * outlyingness
      # }
    } else if (data_depth == "simplicial"){
      cat("support for simplicial depth not yet added")
      # for (j in 1:p) {
      #   outlyingness  <- (1/fda.usc::mdepth.SD(data[,j,])$dep) - 1
      #   median_vec  <-  data[order(outlyingness)[1],j,]
      #   median_dev <- sweep(data[,j,], 2, median_vec ) #t(data[,j,])-median_obs
      #   spatial_sign <- rowSums((median_dev)^2)^(1/2) #sqrt(rowSums((median_dev)^2))
      #   spatial_sign <- median_dev/spatial_sign
      #   spatial_sign[!is.finite(spatial_sign[,1]), ] <- 0 # check which row has an nan or infinite
      #   dir_out_matrix[,j,] <- spatial_sign * outlyingness
      # }
    } else if(data_depth == "half_space" ) {
      cat("half space depth not yet added")
      # for (j in 1:p) {
      #   outlyingness  <- (1/fda.usc::mdepth.HS(data[,j,])$dep) - 1
      #   median_vec  <-  data[order(outlyingness)[1],j,]
      #   median_dev <- sweep(data[,j,], 2, median_vec ) #t(data[,j,])-median_obs
      #   spatial_sign <- rowSums((median_dev)^2)^(1/2) #sqrt(rowSums((median_dev)^2))
      #   spatial_sign <- median_dev/spatial_sign
      #   spatial_sign[!is.finite(spatial_sign[,1]), ] <- 0 # check which row has an nan or infinite
      #   dir_out_matrix[,j,] <- spatial_sign * outlyingness
      # }
    } else {
      cat("depth not suported")
    }
    mean_dir_out  <- apply(dir_out_matrix, c(1,3), mean, na.rm = T)
    var_dir_out <- (apply(dir_out_matrix^2, 1, sum, na.rm = T)/p) - rowSums(mean_dir_out^2, na.rm = T)
    #########################################################################################
    #Dirout_dev <- array(0, dim = c(n,p,d))
    ##
    #for (i in 1:d) {
    #  Dirout_dev[,,i] <- dir_out_matrix[, , i] - mean_dir_out[,i]
    #}
    #Dirout_dev_norm <- matrix(0, n, p)
    #for (j in 1:p) {
    #  Dirout_dev_norm[,j] <- rowSums(Dirout_dev[,j,]^2)
    #}
    #var_out_test <- apply(Dirout_dev_norm, 1, FUN=function(y) mean(y,na.rm=TRUE))
    ########################################################################################

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
