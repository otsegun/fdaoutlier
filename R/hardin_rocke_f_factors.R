## find the degrees of freedom for F distribution according Hardin and Rocke 2005
hardin_factor_sim <- function(n, dimesnion, n_iter = 100){
  sims_result <- cm(n, dimension, n_inter, 100) # check this call and function.
  factors <- sims_result[1] * (sims_result[2] - dimension + 1)/(dimension * sims_result[2])
  cutoff <- qf(.993, dimension, sims_result[2] - dimension +1)
  return(list(factor1 = factors, factor2 = cutoff))
}


hardin_factor_asymptotic <- function(n, dimension){
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
  a1 <- rchisq(10000,dimension + 2)
  a2 <- rchisq(10000,dimension, h/n)
  c <- sum(a1 < a2)/(10000*h/n)
  factors <- c * (m - dimension + 1)/(dimension * m)
  cutoff <- qf(0.993, dimension, m - dimension + 1)
  list(factor1 = factors, factor2 = cutoff)
}


hardin_factor_numeric <- function(n, dimension){
  if (dimension == 2){
    if(n < 1000){
      k  <- floor(n / 5) + 1
      factor1 = hardin_factor_numeric_dimen_2$factor1[k]
      factor2 = hardin_factor_numeric_dimen_2$factor2[k]
    }
    if (n >= 1000){
      asymp_result <- hardin_factor_asymptotic(n = n, dimension = dimension)
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
      asymp_result  <- hardin_factor_asymptotic(n = n, dimension = dimension)
      factor1 <- asymp_result$factor1
      factor2 <- asymp_result$factor2
    }
  } else if (dimension > 3){
    asymp_result  <- hardin_factor_asymptotic(n = n, dimension = dimension)
    factor1 <- asymp_result$factor1
    factor2 <- asymp_result$factor2
  } else{
    stop("dimension must be at least 2")
  }
  return(list(factor1 = factor1, factor2 = factor2))
}


