#' Compute directional quantile outlyingness for a sample of discretely observed curves
#'
#' @param dt A matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#' @param quantiles A numeric vector of length 2 specifying the probabilities of the lower and upper quantiles.
#' Values must be between 0 and 1. Defaults to \code{c(0.025, 0.975)} as specified in Dai et al. (2020).
#'
#'@details
#' The method computes the directional quantile of a sample of curves discretely observed on common points.
#' The directional quantile of a function/curve \eqn{X_i(t)} is the maximum pointwise scaled outlyingness of
#' \eqn{X_i(t)}. The scaling is done using the pointwise absolute difference between the mean and the lower (and upper)
#' quantiles. See Dai et al. (2020) and Myllymäki et al. (2017) for more details.
#'
#' @return A numeric vector containing the the directional quantiles of each observation of \code{dt}.
#'
#' @export
#'
#' @references
#' Dai, W., Mrkvička, T., Sun, Y., & Genton, M. G. (2020). Functional outlier detection and taxonomy by sequential transformations.
#'  \emph{Computational Statistics & Data Analysis}, 106960.
#'
#' Myllymäki, M., Mrkvička, T., Grabarnik, P., Seijo, H., & Hahn, U. (2017).
#'  Global envelope tests for spatial processes. \emph{J. R. Stat. Soc. B}, 79:381-404.
#'
#' @examples
#' data(sim_data1)
#' dq <- directional_quantile(sim_data1$data)
#'
#' @importFrom stats dist quantile
directional_quantile <- function(dt, quantiles = c(0.025, 0.975)){
  l <- length(quantiles)
  if (is.data.frame(dt)) {
    dt <- as.matrix(dt)
  }

  if (!is.array(dt) || !is.numeric(dt))
    stop("Argument \"dt\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dt))) {
    stop("Missing or infinite values are not allowed in argument \"dt\"")
  }

  if (nrow(dt) < 3)
    stop("The number of curves must be greater than 2")

  if(l != 2 || !is.numeric(quantiles) || any(is.na(quantiles))|| any(quantiles < 0) || any(quantiles > 1) )
    stop("Argument \'quantiles\' must be a numeric vector of length 2 with values between 0 and 1.")

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

  return(apply(abs(q_matrix), 1L, max))
}

