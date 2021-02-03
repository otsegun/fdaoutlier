smodel1 <- function(n = 100, p = 50, out_rate = 0.05,
                    qcoeff = 8, kprob = 0.5,
                    deterministic = TRUE,
                    seed = NULL){
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  covfun <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(covfun)
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)

  # Generate Data
  y <- mu+t(L)%*%e

  # set seed
  if(!is.null(seed)){
    set.seed(seed)
  }
  # check deterministic or probabilistic
  if(deterministic) {
    true_outliers <- sort(sample(1:n, round(n*out_rate)))
    n_outliers <- length(true_outliers)
  } else{
    # find outliers using binomial distribution
    true_outliers <- which(runif(n) > (1 - out_rate))
    n_outliers <- length(true_outliers)
  }

  # generate outliers
  e <- matrix(rnorm(p*n_outliers), nrow = p)
  qcoeffk <- rbinom(n_outliers, 1, kprob)
  qcoeffk[qcoeffk == 0] <- -1
  qcoeffk <- qcoeffk*qcoeff
  y[, true_outliers] <- (mu+ t(L)%*%e) + rep(qcoeffk, rep(p, n_outliers))

  return(list(data = t(y), true_outliers = true_outliers) )

}

simulation_model2 <- function(n = 100, p = 50, out_rate = 0.05,
                              qcoeff = 8, a = 0.1, b = .9, l = 0.05,
                              deterministic = TRUE, seed = NULL,
                              plotdt = T){
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  t.cov <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  # set seed
  if(!is.null(seed)){
    set.seed(seed)
  }
  # check deterministic or probabilistic
  if(deterministic) {
    true_outliers <- sort(sample(1:n, round(n*out_rate)))
    n_outliers <- length(true_outliers)
  } else{
    # find outliers using binomial distribution
    true_outliers <- which(runif(n) > (1 - out_rate))
    n_outliers <- length(true_outliers)
  }

  # generate outliers
  e <- matrix(rnorm(p*n_outliers), nrow = p)
  qcoeffk <- rbinom(n_outliers, 1, 0.5)
  qcoeffk[qcoeffk == 0] <- -1
  qcoeffk <- qcoeffk*qcoeff
  indicator <- sapply(runif(n_outliers, a, b), function(x) (t >= x)*(t <= x + l) )
  y[, true_outliers] <- (mu+ t(L)%*%e) + (indicator*rep(qcoeffk, rep(p, n_outliers)))
  return(list(data = t(y), true_outliers = true_outliers))
}

simulation_model3 <- function(n = 100, p = 50, out_rate = 0.05,
                              qcoeff = 6, a = 0.1, b = .9,
                              deterministic = TRUE, seed = NULL,
                              plotdt = T){
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  t.cov <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  # set seed
  if(!is.null(seed)){
    set.seed(seed)
  }
  # check deterministic or probabilistic
  if(deterministic) {
    true_outliers <- sort(sample(1:n, round(n*out_rate)))
    n_outliers <- length(true_outliers)
  } else{
    # find outliers using binomial distribution
    true_outliers <- which(runif(n) > (1 - out_rate))
    n_outliers <- length(true_outliers)
  }

  # generate outliers
  e <- matrix(rnorm(p*n_outliers), nrow = p)
  qcoeffk <- rbinom(n_outliers, 1, 0.5)
  qcoeffk[qcoeffk == 0] <- -1
  qcoeffk <- qcoeffk*qcoeff
  indicator <- sapply(runif(n_outliers, a, b), function(x) (t >= x) )
  y[, true_outliers] <- (mu+ t(L)%*%e) + (indicator*rep(qcoeffk, rep(p, n_outliers)))
  return(list(data = t(y), true_outliers = true_outliers))
}
