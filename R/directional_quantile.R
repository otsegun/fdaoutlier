
#' Title
#'
#' @param dt A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#' @param quantiles A numeric vector of length 2 specifying the probabilities of the lower and upper quantiles.
#'   Defaults to \code{c(0.025, 0.975)}
#'
#' @return A numeric vector containing the negative of the directional quantile.
#'
#' @export
#'
#' @examples
#' @importFrom stats dist quantile
directional_quantile <- function(dt, quantiles = c(0.025, 0.975)){
  # data dimention
  dm <- dim(dt)
  n <- dm[1]
  p <- dm[2]
  # compute pointwise mean
  pwise_mean <- colMeans(dt)
  # center data
  centered <- dt - rep(pwise_mean, rep(n, p))
  # find absolute value of quantiles - mean
  #quants <- abs(apply(dt, 2L, quantile, probs = quantiles) - rep(pwise_mean, rep(2, p)))
  quants <- 1/abs(apply(centered, 2L, quantile, probs = quantiles))
  quants[!is.finite(quants)] <- 0
  #
  lower_quant <- quants[1,]
  upper_quant <- quants[2,]
  q_matrix <- matrix(0, n, p)

  upper_ <- centered >= 0
  lower_ <- !upper_
  q_matrix[upper_] <- (centered*rep(upper_quant, rep(n,p)))[upper_]
  q_matrix[lower_] <- (centered*rep(lower_quant, rep(n,p)))[lower_]

  return(-apply(q_matrix, 2L, max))
}

