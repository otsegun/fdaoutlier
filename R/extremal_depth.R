#' Compute extremal depth for functional data
#'
#'
#'
#' @param dts A numeric matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#'
#' @details
#' This function computes the extremal depth of a univariate functional data. The extremal depth of a function
#' \eqn{g} with respect to a set of function \eqn{S} denoted by \eqn{ED(g, S)} is the proportion
#' of functions in \eqn{S} that is more extreme than \eqn{g}. The functions are ordered using depths cumulative
#' distribution functions (d-CDFs). Extremal depth like the name implies is based on extreme outlyingness and it
#' penalizes functions that are outliers even for a small part of the domain. Proposed/mentioned in
#' Narisetty and Nair (2016) \doi{10.1080/01621459.2015.1110033}.
#'
#' @return A vector containing the extremal depths of the rows of \code{dts}.
#'
#' @author Oluwasegun Ojo
#' @references
#' Narisetty, N. N., & Nair, V. N. (2016). Extremal depth for functional data and applications.
#' \emph{Journal of the American Statistical Association}, 111(516), 1705-1714.
#'
#'  @seealso \code{\link{total_variation_depth}} for functional data.
#'
#' @export
#'
#' @examples
#' dt3 <- simulation_model3()
#' ex_depths <- extremal_depth(dts = dt3$data)
#' # order functions from deepest to most outlying
#' order(ex_depths, decreasing = TRUE)
extremal_depth <- function(dts){
  if(is.data.frame(dts)){
    dts <- as.matrix(dts)
  }

  if(!is.array(dts) || !is.numeric(dts))
    stop("Argument \"dts\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dts))){
    stop("Missing or infinite values are not allowed in argument \"data\"")
  }
  if(nrow(dts) < 3) stop("The number of curves must be greater than 2")

  ddim <- dim(dts)
  n <- ddim[1]
  p <- ddim[2]
  pwdepth <- pwise_depth(dt = dts, n = n) # matrix of n by p
  pmfs <- apply(pwdepth, 1, function(x){
    pmf <- table(x)/p
    return(c(as.numeric(names(pmf[1])), # depth level and mass
             pmf[1]))
  })

  depth_levels <- pmfs[1,]
  masses <- pmfs[2, ]
  # order functions according to depth_levels and mass
  ordered_functions <- sapply(sort(unique(depth_levels),
                                   method = "quick"),
                              function(x){
    fns_depth_level <- which(depth_levels == x)
    if(length(fns_depth_level) > 1){
      fns_depth_level[order(masses[fns_depth_level], decreasing = T)]
    }else{
      fns_depth_level
    }
  })
  ordered_functions <- unlist(ordered_functions)
  depth_values <- ((1:n)/n)[order(ordered_functions)]
}

pwise_depth <- function(dt, n) {
  pdepth <- apply(dt, 2, function(i){
    (1 - abs(2*rank(i) - n -1)/n) # for rank r, d = ((r - 1) + (n - r))/n
  })
  return(pdepth)
}
