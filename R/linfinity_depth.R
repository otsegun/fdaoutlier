
#' Compute the L-infinity depth of a sample of curves/functions.
#'
#' The L-infinity depth is a simple generalization of the \eqn{L^p} multivariate depth to
#' functional data proposed in Long and Huang (2015)
#'  \href{https://arxiv.org/abs/1506.01332}{<arXiv:1506.01332} and also
#' used in Dai et al. (2020) \doi{10.1016/j.csda.2020.106960}.
#'
#' @param dt A matrix or data frame of size \eqn{n} functions/curves by \eqn{p} domain/evaluation points.
#'
#' @return A numeric vector of size \code{nrow(dt)} containing the band depth values of each curve.
#'
#' @references
#' Long, J. P., & Huang, J. Z. (2015). A study of functional depths. \emph{arXiv preprint arXiv}:1506.01332.
#'
#' Dai, W., Mrkvička, T., Sun, Y., & Genton, M. G. (2020). Functional outlier detection and taxonomy by
#' sequential transformations. \emph{Computational Statistics & Data Analysis}, 106960.
#'
#' @export
#'
#' @examples
#' dt1 <- simulation_model1()
#' linf <- linfinity_depth(dt1$data)
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
