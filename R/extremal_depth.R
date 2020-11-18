#' Compute extremal depth for functional data
#'
#'
#'
#' @param dt A numeric matrix or dataframe of size \eqn{n} observations/curves by \eqn{p} domain/evaluation
#'   points.
#'
#' @details
#' This function computes the extremal depth a univariate functional data. The extremal depth of a function
#' \eqn{g} with respect to a set of function \eqn{S} denoted by \eqn{ED(g, S)} is the proportion
#' of functions in \eqn{S} that is more extreme than \eqn{g}. The functions are ordered using depths cummulative
#' distribution functions (d-CDFs). Extremal depth like the name implies is based on extreme outlyingness and it
#' penalizes functions that are outliers even for a small part of the domain. Proposed/mentioned in
#' Narisetty and Nair (2016)\href{https://doi.org/10.1080/01621459.2015.1110033}{<doi:10.1080/01621459.2015.1110033>}.
#'
#' @return A vector containing the extremal depths of the rows of \code{dt}.
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
#' data(sim_data1)
#' ex_depths <- extremal_depth(sim_data1$data)
#' # order functions from deepest to most outlying
#' order(ex_depths, decreasing = TRUE)
extremal_depth <- function(dt){
  if(is.data.frame(dt)){
    dt <- as.matrix(dt)
  }

  if(!is.array(dt) || !is.numeric(dt))
    stop("Argument \"dt\" must be a numeric matrix or dataframe.")

  if (any(!is.finite(dt))){
    stop("Missing or infinite values are not allowed in argument \"data\"")
  }
  if(nrow(dt) < 3) stop("The number of curves must be greater than 2")

  n <- nrow(dt)
  p <- ncol(dt)
  pwdepth <- pwise_depth(dt = dt, n = n) # matrix of n by p
  pmfs <- apply(pwdepth, 1, function(x){
    pmf <- table(x)/p
    return(c(as.numeric(names(pmf[1])), # depth level and mass
             pmf[1]))
  })

  depth_levels <- pmfs[1,]
  masses <- pmfs[2, ]
  # order functions accordint to depth_levels and mass
  ordered_functions <- sapply(unique(sort(depth_levels)), function(x){
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
    (1 - abs(2*rank(i)-n -1)/n)
  })
  return(pdepth)
}
