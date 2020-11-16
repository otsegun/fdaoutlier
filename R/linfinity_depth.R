
#' Compute the L-infinity depth a sample of curves/functions.
#'
#' The L-infinity depth is a simple generalization of the \eqn{L^p} multivariate depth to
#' functional data proposed in \href{https://arxiv.org/abs/1506.01332}{Long and Huang (2015)}.
#'
#' @param dt A matrix or data frame of size \eqn{n} functions/curves by \eqn{p} domain/evaluation points.
#'
#' @return A numeric vector of size \code{nrow(dt)} containing the band depth values of each curve.
#'
#' @references
#' Long, J. P., & Huang, J. Z. (2015). A study of functional depths. \emph{arXiv preprint arXiv}:1506.01332.
#'
#' @export
#'
#' @examples
#' linf <- linfinity_depth(sim_data1$data)
#'
linfinity_depth <- function(dt){
  if (is.data.frame(dt)) {
    dt <- as.matrix(dt)
  }

  if (!is.array(dt) || !is.numeric(dt))
    stop("Argument \"dt\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dt))) {
    stop("Missing or infinite values are not allowed in argument \"dt\"")
  }

  # distance very large if n is big. to cpp?
  distances <- as.matrix(dist(dt, method = "maximum", diag = T, upper = T))
  unname(1/(1+colMeans(distances)))
}

# l_infinity_depth(sim_data1$data)
