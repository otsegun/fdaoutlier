#' Compute the modified band depth for a sample of curves/functions.
#'
#' This function computes the modified band depth of López-Pintado and Romo (2009)
#' \doi{10.1198/jasa.2009.0108}. Bands of 2 functions are always used and the
#' fast algorithm of Sun et al. (2012) \doi{10.1002/sta4.8} is used in computing
#' the depth values.
#'
#' @param dt A matrix or data frame of size \eqn{n} functions/curves by \eqn{p} domain/evaluation points.
#'
#' @return A numeric vector of size \code{nrow(dt)} containing the band depth values of each curve.
#'
#' @references
#' López-Pintado, S., & Romo, J. (2009). On the Concept of Depth for Functional Data.
#' \emph{Journal of the American Statistical Association}, 104(486), 718-734.
#'
#' Sun, Y., Genton, M. G., & Nychka, D. W. (2012). Exact fast computation of band depth for large functional datasets:
#' How quickly can one million curves be ranked?. \emph{Stat}, 1(1), 68-74.
#'
#' @export
#'
#' @examples
#' dt1 <- simulation_model1()
#' mbd2 <- modified_band_depth(dt1$data)
#'
modified_band_depth <- function(dt){

  if (is.data.frame(dt)) {
    dt <- as.matrix(dt)
  }

  if (any(!is.finite(dt))) {
    stop("Missing or infinite values are not allowed in argument \"dt\"")
  }

  if (!is.array(dt) || !is.numeric(dt))
    stop("Argument \"dt\" must be a non empty numeric matrix or dataframe.")




  dm <- dim(dt)
  p <- dm[2]
  n <- dm[1]
  if(n < 2) stop("Number of row of argument 'dt' must be greater than 1")
  rnkmat <- apply(dt,2,rank) # switches matrix to max(p,n) by min(p,n)
  down <- rnkmat-1
  up <- n-rnkmat
  unname((rowSums(up*down)/p+n-1)/choose(n,2))
}

# dts <- sim_data1$data[1:9, 1:10]
# all.equal(modified_band_depth(dts),fMBD(t(dts)))
# all.equal(band_depth(dts),fBD2(t(dts)))
