#' Compute total variation depth
#'
#' @param data
#' @param n_curve
#' @param n_points
#'
#' @return
#' @export
#'
#' @examples
total_variation_depth <- function(data,
                                  n_curves = nrow(data),
                                  n_points = ncol(data)){
  data_t <- t(data)
  pointwise_ranks <- t(apply(data_t,1,rank))/n_curves
  total_variation <- pointwise_ranks * (1 - pointwise_ranks)
  tempres <- .Call(C_totalVariationDepth,
                          as.double(data), as.double(data_t),
                          n_curves, n_points,
                          PACKAGE = "fdalite")
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

