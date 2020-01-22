#' Directional outlyingness for univariate or multivariate functional data.
#'
#'
#' Compute the directional outlyingness of a univariate or multivariate
#' functional data based on Dai and Genton (2018).
#'
#' @param data A matrix for univariate functional data (of size n observations
#'   by p domain points) or a 3-dimensional array for multivariate functional
#'   data (of size n observations by p domain points by d dimension).
#'
#' @param data_depth The method for computing the depth. Can be one of "mahalanobis", "random_projections",
#'  "simplicial", or "half_space" depth. Default is "random_projections".
#'
#' @param return_distance A logical scalar. If TRUE, returns the matrix whose rows are the mean and variation of directional
#'  outlyiness, the mahalanobis distance of the observations of this matrix, and the robust estimate of the
#'  mean and covariance of this matrix (computed using the minimum covariance determinant method).
#'
#' @param dirout_matrix A logical scalar. If TRUE, returns the directional outlyingness matrix.
#'
#' @return Returns a list containing:
#'  \itemize{
#'   \item{\code{mean_outlyingness}}{a matrix of the mean of directional outlyingness.}
#'   \item{\code{var_outlyingness}}{a vector of variation of directional outlyingness.}
#'   \item{\code{var_outlyingness}}{if \code{return_distance} = T, a vector of }
#' }  the , , distance values
#'
#'
#' @importFrom stats mad mahalanobis median var

dir_out <- function(data, data_depth = c( "random_projections", "mahalanobis", "simplicial", "half_space"),
                    return_distance = T, dirout_matrix = FALSE){
  # library used: Mass::cov_rob, fda.usc::mdepth.RP, fda.usc::mdepth.MhD, fda.usc::mdepth.SD, fda.usc::mdepth.HS
  data_dim  <-  dim(data)
  data_depth <- match.arg(data_depth)
  if(!is.array(data))
    stop("data must be a 2-dimensional or 3-dimensional array")

  if (any(is.na(data)) || any(is.infinite(data)))
    stop("missing or infinite values are not allowed")

  if (length(data_dim) == 2){
    #############################################################3
    #median_vec <- apply(dt, 2, median)
    #mad_vec <- apply(dt, 2, mad)
    #dir_out_matrix <- sweep(sweep(dt, 2, median_vec), 2, mad_vec, FUN = "/")
    ################################################
    data  <- t(data)
    median_vec <- apply(data, 1, median)
    mad_vec <- apply(data, 1, mad)
    dir_out_matrix <- t((data-median_vec)/(mad_vec))
    mean_dir_out <- apply(dir_out_matrix, 1, mean, na.rm = T) # rowMeans(x, na.rm = T, dims = 1)
    var_dir_out <- apply(dir_out_matrix, 1, var, na.rm = T)

    if(return_distance){
      ms_matrix <- cbind(mean_dir_out, var_dir_out)
      mcd_obj  <- MASS::cov.rob(ms_matrix, method = "mcd", nsamp = "best")
      robust_cov <- mcd_obj$cov
      robust_mean <- unname(mcd_obj$center)
      distance <- mahalanobis(ms_matrix, robust_mean, robust_cov)
    }
  } else if (length(data_dim) == 3) {
    n <- data_dim[1]
    p <- data_dim[2]
    d <- data_dim[3]
    dir_out_matrix  <- array(0, dim = c(n, p, d))  # to cpp
    for(j in 1:p){ ## if before loop
      if (data_depth == "random_projections"){
        outlyingness <- (1/fda.usc::mdepth.RP(data[,j,],proj=200)$dep) - 1
      } else if (data_depth == "mahalanobis"){
        outlyingness  <- (1/fda.usc::mdepth.MhD(data[,j,])$dep) - 1
      } else if (data_depth == "simplicial") {
        outlyingness  <- (1/fda.usc::mdepth.SD(data[,j,])$dep) - 1
      } else if (data_depth == "half_space") {
        outlyingness  <- (1/fda.usc::mdepth.HS(data[,j,])$dep) - 1
      }
      median_vec  <-  data[order(outlyingness)[1],j,]
      median_dev <- sweep(data[,j,], 2, median_vec ) #t(data[,j,])-median_obs
      dir <- rowSums((median_dev)^2)^(1/2)
      dir <- median_dev/dir
      dir[!is.finite(dir[,1]), ] <- 0 # check which row has an nan or infinite
      dir_out_matrix[,j,] <- dir * outlyingness
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
      ms_matrix <- cbind(mean_dir_out, var_dir_out)
      mcd_obj  <- MASS::cov.rob(ms_matrix, method = "mcd", nsamp = "best")
      robust_cov <- mcd_obj$cov
      robust_mean <- unname(mcd_obj$center)
      distance <- mahalanobis(ms_matrix, robust_mean, robust_cov)
    }
  }
  else{
    stop("a 2-dimensional or 3-dimensional array is required")
  }
  if (return_distance){
    if (dirout_matrix){
      return(list(mean_outlyingness = mean_dir_out, var_outlyingness = var_dir_out, distance = distance,
                  ms_matrix = ms_matrix, mcd_obj = mcd_obj, dirout_matrix = dir_out_matrix))
    } else{
      return(list(mean_outlyingness = mean_dir_out, var_outlyingness = var_dir_out, distance = distance,
                  ms_matrix = ms_matrix, mcd_obj = mcd_obj))
    }
  }
  else{
    if(dirout_matrix){
      return(list(mean_outlyingness = mean_dir_out,
                  var_outlyingness = var_dir_out,
                  dirout_matrix = dir_out_matrix))
    }
    else{
      return(list(mean_outlyingness = mean_dir_out,
                  var_outlyingness = var_dir_out))
    }

  }

}
