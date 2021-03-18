
croux_hesbroeck_asymptotic <- function(n, dimension){
  h <- floor((n+dimension+1)/2)
  alpha <- (n-h)/n
  q_alpha <- qchisq(1-alpha, dimension)
  c_alpha <- (1 - alpha)/pchisq(q_alpha, dimension + 2)
  c2 <- -pchisq(q_alpha, dimension+2)/2
  c3 <- -pchisq(q_alpha, dimension + 4)/2
  c4 <- 3*c3
  b1 <- c_alpha*(c3-c4)/(1-alpha)
  b2 <- 0.5 + c_alpha/(1-alpha)*(c3-q_alpha*(c2+(1-alpha)/2)/dimension)
  v1  <- (1-alpha)*(b1^2)*
    (alpha*(c_alpha*q_alpha/dimension-1)^2-1)-2*c3*c_alpha^2*
    (3*(b1-dimension*b2)^2+(dimension+2)*b2*(2*b1-dimension*b2))
  v2 <- n*(b1*(b1-dimension*b2)*(1-alpha))^2*c_alpha^2
  v <- v1/v2
  m_asy <- 2/(c_alpha^2*v)
  m <- m_asy*exp(0.725-0.00663*dimension-0.078*log(n))
  if (m < dimension){ #if m is >= dimension, then line 18 works, if not change m to m_asy
    m <- m_asy
  }
  a1 <- rchisq(10000,dimension + 2)
  a2 <- rchisq(10000,dimension, h/n)
  c <- sum(a1 < a2)/(10000*h/n)
  factors <- c * (m - dimension + 1)/(dimension * m)
  cutoff <- qf(0.993, dimension, m - dimension + 1)
  list(factor1 = factors, factor2 = cutoff)
}

#' Compute F distribution factors for approximating the tail of the distribution of robust MCD distance.
#'
#' Computes asymptotically, the factors for F approximation cutoff for (MCD) robust
#' mahalanobis distances according to Hardin and Rocke (2005)
#' \doi{10.1198/106186005X77685}.
#'
#' @param n A numeric value indicating the number of observations of the data.
#' @param dimension A numeric value indicating the number of variables of the data.
#'
#' @details This function computes the two factors needed for the determining an appropriate
#' cutoff for robust mahalanobis distances computed using the MCD method.
#'
#' The F approximation according to Hardin and Rocke (2005) \doi{10.1198/106186005X77685}
#' is given by: \deqn{c(m-p+1)/(pm) * RMD^2 ~ F_{p, m-p+1}} where \eqn{m} is a parameter for finding the degree of freedom of the
#' \eqn{F} distribution, \eqn{c} is a scaling constant and \eqn{p} is the dimension. The first factor
#' returned by this function (\code{factor1}) is \eqn{c(m-p+1)/(pm)} and the second factor (\code{factor2}) is \eqn{F_{p, m-p+1}}.
#'
#' @return Returns a list containing: \item{factor1}{ then estimated value of
#'   \eqn{c(m-p+1)/(pm)} based on \code{n} and \code{dimension}.} \item{factor2}{ the
#'   value of \eqn{F_{p, m-p+1}}.}
#'
#' @references Hardin, J., and Rocke, D. M. (2005). The distribution of robust distances.
#'   \emph{Journal of Computational and Graphical Statistics}, 14(4), 928-946.
#'
#'
#' @importFrom stats pchisq qchisq qf rchisq
#'
hardin_factor_numeric <- function(n, dimension){
  if (dimension == 2){
    if(n < 1000){
      k  <- floor(n / 5) + 1
      factor1 = hardin_factor_numeric_dimen_2$factor1[k]
      factor2 = hardin_factor_numeric_dimen_2$factor2[k]
    }
    if (n >= 1000){
      asymp_result <- croux_hesbroeck_asymptotic(n = n, dimension = dimension)
      factor1  <- asymp_result$factor1
      factor2 = asymp_result$factor2
    }

  } else if (dimension == 3){
    if (n < 1000){
      k  <- floor(n / 5) + 1
      factor1  <- hardin_factor_numeric_dimen_3$factor1[k]
      factor2  <- hardin_factor_numeric_dimen_3$factor2[k]
    }
    if (n >= 1000){
      asymp_result  <- croux_hesbroeck_asymptotic(n = n, dimension = dimension)
      factor1 <- asymp_result$factor1
      factor2 <- asymp_result$factor2
    }
  } else if (dimension > 3){
    asymp_result  <- croux_hesbroeck_asymptotic(n = n, dimension = dimension)
    factor1 <- asymp_result$factor1
    factor2 <- asymp_result$factor2
  } else{
    stop("Argument \'dimension\' must be greater than or equal to 2.")
  }
  return(list(factor1 = factor1, factor2 = factor2))
}


