library(tidyr)
library(dplyr)
library(ggplot2)


###### Muod paper models##
model1 <- function(n, p = 50, out.rate = 0.05, plotdt = F){
  f.out1 <- function(u = runif(1,0,1)){            #cont model 2Model
    return(ifelse(u < 0.5, 1,-1))
  }
  
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #  t.cov <- cov.fun2(d.matrix,0.3,1/0.3) 
  t.cov <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  
  ## Generate 
  
  ### Generate Outliers
  true_outliers <- sort(sample(1:n, n*out.rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    y[,i]= (4*t) + (f.out1()*8) +t(L)%*%e                     #Outliers for Model 1
    #y[,i]=f.out1(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 2   
    #y[,i]=f.out2(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 3                       
  }
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
      geom_line(data = filter(dt_t, outlier == "normal"),
                aes(x =x,  y = value, group = key),
                color = 8, size = .3 ) +
      geom_line(data = filter(dt_t, outlier == "outlier"),
                aes(x = x,  y = value, group = key),
                color = "#D55E00", size = .3) +
      
      xlab("t") +
      ylab("X(t)")+
      ggtitle("Model 1") +
      scale_color_manual(values=c("#f03b20", "grey")) +
      scale_alpha_manual(values = c(5, .2)) +
      theme_bw()+
        theme(plot.title = element_text(hjust = 0.5, size = 8),
              axis.text=element_text(size = 7),
              axis.title=element_text(size = 8) ))
  } else(
    p <- NULL
  )
  return(list(data = t(y), true_outliers = true_outliers, plot = p))
  
}

model2 <- function(n, p = 60, out.rate = 0.05, plotdt = F){
  f.out2 <- function(u = runif(1,0,1),
                     T1 = runif(1,0.1,0.9),
                     x = t){        
    return(list(u = ifelse(u < 0.5, 1,-1),
                i = (x>T1)*(x<(T1+.05))) )
  }
  
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #  t.cov <- cov.fun2(d.matrix,0.3,1/0.3) 
  t.cov <- exp(-d_matrix)
  mu <- 4*t
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  
  ## Generate 
  
  ### Generate Outliers
  true_outliers <- sort(sample(1:n, n*out.rate)) #deterministic
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    temp <- f.out2(x = t)
    y[,i]= (4*t) + (8 * temp$u * temp$i) + t(L)%*%e                     #Outliers for Model 1
    #y[,i]=f.out1(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 2   
    #y[,i]=f.out2(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 3                       
  }
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
            geom_line(data = filter(dt_t, outlier == "normal"),
                      aes(x =x,  y = value, group = key),
                      color = 8, size = .3) +
            geom_line(data = filter(dt_t, outlier == "outlier"),
                      aes(x = x,  y = value, group = key),
                      color = "#D55E00", size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 2") +
            scale_color_manual(values=c("#f03b20", "grey")) +
            scale_alpha_manual(values = c(5, .2)) +
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  }else{
    p <- NULL
  }
  
  return(list(data = t(y), true_outliers = true_outliers, plot = p) )
  
}
model3 <- function(n, p = 60, out.rate = 0.05, plotdt = F){ #ana model 1
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #  t.cov <- cov.fun2(d.matrix,0.3,1/0.3) 
  t.cov <- 0.3*exp(-(1/.3)*d_matrix)
  mu <- 30*t*(1-t)^(3/2)
  L <- chol(t.cov)
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  
  ## Generate 
  
  ### Generate Outliers
  true_outliers <- sort(sample(1:n, n*out.rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- 0.5*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    y[,i]= (30*(1-t)*t^(3/2))+t(L)%*%e                     #Outliers for Model 1
    #y[,i]=f.out1(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 2   
    #y[,i]=f.out2(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 3                       
  }
  
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
            geom_line(data = filter(dt_t, outlier == "normal"),
                      aes(x =x,  y = value, group = key),
                      color = 8, size = .3) +
            geom_line(data = filter(dt_t, outlier == "outlier"),
                      aes(x = x,  y = value, group = key),
                      color = "#D55E00", size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 3") +
            scale_color_manual(values=c("#f03b20", "grey")) +
            scale_alpha_manual(values = c(5, .2)) +
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  } else{
    p <- NULL
  }
  
  return(list(data = t(y), true_outliers = true_outliers, plot = p) )
  
}
model4 <- function(n, p = 50, out.rate = 0.05, plotdt = F){

  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #Models 2
  #  t.cov=cov.fun(d.matrix,1,1,1)  #covariance function in time
  t.cov <-  1*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition
  mu <- 4*t
  
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  ## Generate outlier
  true_outliers <- sort(sample(1:n, n*out.rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- 5*exp(-2*(d_matrix)^(.5))
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    #y[,i]= (30*(1-t)*t^(3/2))+t(L)%*%e                     #Outliers for Model 1
    #y[,i] <- f.out1(t) +t(L)%*%e   #Outliers for Model 2   
    y[,i] <- (4*t)+t(L)%*%e   #Outliers for Model 3                       
  }
  
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
            geom_line(data = filter(dt_t, outlier == "normal"),
                      aes(x =x,  y = value, group = key),
                      color = 8, , size = .3) +
            geom_line(data = filter(dt_t, outlier == "outlier"),
                      aes(x = x,  y = value, group = key),
                      color = "#D55E00", , size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 4") +
            scale_color_manual(values=c("#f03b20", "grey")) +
            scale_alpha_manual(values = c(5, .2)) +
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  }else{
    p <- NULL
  }
  
  return(list(data = t(y), true_outliers = true_outliers, plot = p) )
}
model5 <- function(n, p = 50, out.rate = 0.05, plotdt = F){ #ana model2
  f.out1 <- function(t, m = runif(1,0.25,0.75),
                     u = runif(1,0,1)){            
    if (u < 0.5){
      f <- 4*t+exp(-(t-m)^2/0.02)/sqrt(2*pi*0.01)-1.8
    }
    else{
      f <- 4*t-exp(-(t-m)^2/0.02)/sqrt(2*pi*0.01)+1.8
    }
    f
  }
  
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #Models 2
  #  t.cov=cov.fun(d.matrix,1,1,1)  #covariance function in time
  t.cov <-  1*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition
  mu <- 4*t
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  ## Generate outlier
  true_outliers <- sort(sample(1:n, n*out.rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- 0.5*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    #y[,i]= (30*(1-t)*t^(3/2))+t(L)%*%e                     #Outliers for Model 1
    y[,i] <- f.out1(t) +t(L)%*%e   #Outliers for Model 2   
    #y[,i]=f.out2(t,runif(1,0.25,0.75))+t(L)%*%e   #Outliers for Model 3                       
  }
  
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
            geom_line(data = filter(dt_t, outlier == "normal"),
                      aes(x =x,  y = value, group = key),
                      color = 8, size = .3) +
            geom_line(data = filter(dt_t, outlier == "outlier"),
                      aes(x = x,  y = value, group = key),
                      color = "#D55E00", size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 5") +
            scale_color_manual(values=c("#f03b20", "grey")) +
            scale_alpha_manual(values = c(5, .2)) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  } else{
    p <- NULL
  }
  
  return(list(data = t(y), true_outliers = true_outliers, plot = p))
}
model6 <- function(n, p = 50, out.rate = 0.05, plotdt = F){# ana model3
  f.out2 <- function(t, th = runif(1,0.25,0.75)){
    4*t + 2*sin(4*(t+th)*pi)
  }
  
  t <- (1:p)/p
  d <- dist(t, upper = T, diag = T)
  d_matrix <- as.matrix(d)
  #Models 2
  #  t.cov=cov.fun(d.matrix,1,1,1)  #covariance function in time
  t.cov <-  1*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition
  mu <- 4*t
  
  ### Generate Data
  e <- matrix(rnorm(n*p), nrow = p, ncol = n)
  y <- mu+t(L)%*%e
  ## Generate outlier
  true_outliers <- sort(sample(1:n, n*out.rate))
  #  t.cov <- cov.fun(d.matrix,0.5,1,1)  #covariance function in time
  t.cov <- 0.5*exp(-d_matrix)
  L <- chol(t.cov) # Cholesky Decomposition     
  
  for (i in true_outliers){
    e <- rnorm(p)
    #y[,i]= (30*(1-t)*t^(3/2))+t(L)%*%e                     #Outliers for Model 1
    #y[,i] <- f.out1(t) +t(L)%*%e   #Outliers for Model 2   
    y[,i] <- f.out2(t)+t(L)%*%e   #Outliers for Model 3                       
  }
  
  if(plotdt){
    dt <- as.data.frame.matrix(t(y))
    dt <- as.data.frame.matrix(t(as.matrix(dt)))
    dt$x <- seq(0,1, length.out = p)
    out <-  rep(0,n)
    out[true_outliers] <- 1
    
    dt_t <- dt %>% gather(key = "key", value = "value", -x)
    dt_t$outlier <- factor(rep(out, each = p), levels = c(1, 0),
                           labels = c( "outlier", "normal"))
    dt_t$key <- factor(dt_t$key)
    
    #col=c(rep("#D55E00",m1/2),rep("#009E73",m1/2),rep(8,n-m1))
    
    print(p <- ggplot() +
            geom_line(data = filter(dt_t, outlier == "normal"),
                      aes(x =x,  y = value, group = key),
                      color = 8, size = .3) +
            geom_line(data = filter(dt_t, outlier == "outlier"),
                      aes(x = x,  y = value, group = key),
                      color = "#D55E00", size = .3) +
            
            xlab("t") +
            ylab("X(t)")+
            ggtitle("Model 6") +
            scale_color_manual(values=c("#f03b20", "grey")) +
            scale_alpha_manual(values = c(5, .2)) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 8),
                  axis.text=element_text(size = 7),
                  axis.title=element_text(size = 8) ))
  }else{
    p <- NULL
  }
  return(list(data = t(y), true_outliers = true_outliers, plot = p) )
}

model7 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                              out.rate = 0.05, plotdt = F){# muod amplitude
  ## generate a mass with n 
  y_mix <- t(replicate(n, Xt_mass(t = t)))
  
  #is curve an outlier or not?
  out_row <- which((purrr::rbernoulli(n, out.rate))) # change this from purr
  
  # genrate mag
  if (length(out_row) != 0) {
    y_amp <- t(replicate(length(out_row), Xt_amp(ts = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_amp
  }
  # create outlier type variable
  
  #ind_type <- rep("normal", n)
  #ind_type[out_row] <- "amplitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }
      
    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}


model7_illus <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                   out.rate = 0.05, plotdt = F){# muod amplitude
  ## generate a mass with n 
  y_mix <- t(replicate(n, Xt_mass(t = t)))
  
  #is curve an outlier or not?
  out_row <- sample(1:n, size = floor(n*out.rate)) #which((purrr::rbernoulli(n, out.rate))) # change this from purr
  
  # genrate mag
  if (length(out_row) != 0) {
    y_amp <- t(replicate(length(out_row), Xt_amp_illus(ts = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_amp
  }
  # create outlier type variable
  
  #ind_type <- rep("normal", n)
  #ind_type[out_row] <- "amplitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }
      
    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}

model7_illus2 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                         out.rate = 0.05, plotdt = F){# muod amplitude
 
   ## generate a mass with n 
  
  out_row <- sample(1:n, size = floor(n*out.rate))
  y_mix <- t(replicate(n - length(out_row), Xt_mass(t = t)))
  
  #is curve an outlier or not?
   #which((purrr::rbernoulli(n, out.rate))) # change this from purr
  
  # genrate mag
  if (length(out_row) != 0) {
    tt <- sample(c(T,F), length(out_row), replace = T)
    
    high_out <- sum(tt)
    low_out <- sum(tt == F)
    
    if(high_out != 0){
      y_amp <- t(replicate(high_out, Xt_amp_illus(ts = t)))
    } 
    
    if(low_out!=0) {
      y_amp2 <- t(replicate(low_out, Xt_amp_illus2(ts = t)))
    }
    
    # replace outliers in original data
    
    y_mix <- rbind(y_mix, y_amp)
    y_mix <- rbind(y_mix, y_amp2)
    
    out_row <- ((n - length(out_row))+1) : n
    out_row <- out_row *  c(rep(1, high_out), rep(-1, low_out))
      
  }
  # create outlier type variable
  
  #ind_type <- rep("normal", n)
  #ind_type[out_row] <- "amplitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% abs(out_row)){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }
      
    }
  }

  return(list(data = y_mix, true_outliers = out_row))
}

model8 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                              out.rate = 0.05, plotdt = F){#  muod magnitude
  ## generate a mass with n
  y_mix <- t(replicate(n, Xt_mass(t = t)))

  #is curve an outlier or not?
  out_row <- sample(1:n, size = floor(n*out.rate)) #which((purrr::rbernoulli(n,  out.rate)))

  # genrate mag
  if (length(out_row) != 0) {
    y_mag <- t(replicate(length(out_row), Xt_mag(t = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_mag
  }
  # create outlier type variable

  # ind_type <- rep("normal", n)
  # ind_type[out_row] <- "magnitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }

    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}
model8_illus <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                   out.rate = 0.05, plotdt = F){#  muod magnitude
  ## generate a mass with n
  y_mix <- t(replicate(n, Xt_mass(t = t)))
  
  #is curve an outlier or not?
  out_row <- sample(1:n, size = floor(n*out.rate)) #which((purrr::rbernoulli(n,  out.rate)))
  
  # genrate mag
  if (length(out_row) != 0) {
    y_mag <- t(replicate(length(out_row), Xt_mag_illus(t = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_mag
  }
  # create outlier type variable
  
  # ind_type <- rep("normal", n)
  # ind_type[out_row] <- "magnitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }
      
    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}
model8_illus2 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                         out.rate = 0.05, plotdt = F){#  muod magnitude
  ## generate a mass with n
  y_mix <- t(replicate(n, Xt_mass(t = t)))
  
  #is curve an outlier or not?
  out_row <- sample(1:n, size = floor(n*out.rate)) #which((purrr::rbernoulli(n,  out.rate)))
  
  # genrate mag
  if (length(out_row) != 0) {
    y_mag <- t(replicate(length(out_row), Xt_mag_illus2(t = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_mag
  }
  # create outlier type variable
  
  # ind_type <- rep("normal", n)
  # ind_type[out_row] <- "magnitude"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }
      
    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}
model9 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                              out.rate = 0.05, plotdt = F){ # muod shape
  ## generate a mass with n
  y_mix <- t(replicate(n, Xt_mass(t = t)))

  #is curve an outlier or not?
  
  out_row <-  sample(1:n, size = floor(n*out.rate))
    

  # genrate mag
  if (length(out_row) != 0) {
    y_sha <- t(replicate(length(out_row), Xt_sha(ts = t)))
    # replace outliers in original data
    y_mix[out_row, ] <- y_sha
  }
  # create outlier type variable

  # ind_type <- rep("normal", n)
  # ind_type[out_row] <- "shape"
  if(plotdt){
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
    for (i in 1:nrow(y_mix)) {
      if(i %in% out_row){
        lines(t, y_mix[i,], col = "red", lwd = 0.3)
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }

    }
  }
  return(list(data = y_mix, true_outliers = out_row))
}

model10 <- function(n, p = 50, t = seq(0, 2*pi, length.out = p),
                              out.rate = 0.05, plotdt = T){ # muod mix
  ## generate a mass with n
  y_mix <- t(replicate(n, Xt_mass(t = t)))

  #is curve an outlier or not?
  
  out_mix_row <- which((purrr::rbernoulli(n, out.rate)))

  outlier_types <- assign_outliers(mix_row = out_mix_row)
  mag_row_mix <- out_mix_row[outlier_types == "magnitude"]
  amp_row_mix <- out_mix_row[outlier_types == "amplitude"]
  sha_row_mix <- out_mix_row[outlier_types == "shape"]

  # genrate mag, amp, and sha outliers
  if (length(mag_row_mix) != 0) {
    y_mag_mix <- t(replicate(length(mag_row_mix), Xt_mag(t = t)))
    # replace outliers in original data
    y_mix[mag_row_mix, ] <- y_mag_mix
  }

  if (length(amp_row_mix) != 0) {
    y_amp_mix <- t(replicate(length(amp_row_mix), Xt_amp(t = t)))
    y_mix[amp_row_mix, ] <- y_amp_mix
  }

  if (length(sha_row_mix) != 0) {
    y_sha_mix <- t(replicate(length(sha_row_mix), Xt_sha(t = t)))
    y_mix[sha_row_mix, ] <- y_sha_mix
  }

  # create outlier type variable

  # ind_mix_type <- rep("normal", n)
  # ind_mix_type[mag_row_mix] <- "magnitude"
  # ind_mix_type[amp_row_mix] <- "amplitude"
  # ind_mix_type[sha_row_mix] <- "shape"
  if (plotdt) {
    plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))

    for (i in 1:nrow(y_mix)) {
      if(i %in% sha_row_mix){
        lines(t, y_mix[i,], col = "#fc8d59")
      }else if(i %in% amp_row_mix){
        lines(t, y_mix[i,], col = "#91cf60")
      } else if(i %in% mag_row_mix){
        lines(t, y_mix[i,], col = "#67a9cf")
      } else{
        lines(t, y_mix[i,], lwd = .2)
      }

    }

  }
  return(list(dt = y_mix, true_outliers = out_mix_row))
}



###################legacy mixture models############################################
# helper functions to generate mass, amp, mag, and sha
mass_func <- function(t, a1, a2){
  return((a1 * sin(t)) + (a2 * cos(t)))
}
mag_out <- function(t, a1, a2, k){
  return((a1 * sin(t)) + (a2 * cos(t)) + k*.5 )
}
amp_out <- function(t, b1, b2){
  return((b1 * sin(t)) + (b2 * cos(t)))
}
sha_out <- function(t, a1, a2, e_t = rnorm(1, 0, 1/2)){
  return((a1 * sin(t)) + (a2 * cos(t)) + e_t )
}


Xt_mass <- function(t){
  a1_param = runif(1,.75, 1.25)
  a2_param = runif(1,.75, 1.25)
  sapply(t, mass_func, a1 = a1_param, a2 = a2_param)
}
Xt_mag <- function(t){
  a1_param = runif(1,.75, 1.25)
  a2_param = runif(1,.75, 1.25)
  k_param <- ifelse(purrr:::rbernoulli(1), -1,1)
  sapply(t, mag_out, a1 = a1_param, a2 = a2_param, k = k_param)
}

Xt_mag_illus <- function(t){
  a1_param = runif(1,.75, 1.25)
  a2_param = runif(1,.75, 1.25)
  k_param <- 2 #ifelse(purrr:::rbernoulli(1), -1,1)
  sapply(t, mag_out, a1 = a1_param, a2 = a2_param, k = k_param)
}

Xt_mag_illus2 <- function(t){
  a1_param = runif(1,.75, 1.25)
  a2_param = runif(1,.75, 1.25)
  k_param <- 2*sample(c(-1,1),1)
    sapply(t, mag_out, a1 = a1_param, a2 = a2_param, k = k_param)
}

Xt_amp <- function(ts){
  b1_param = runif(1,1.3, 1.5)
  b2_param = runif(1,1.3, 1.5)
  sapply(ts, amp_out, b1 = b1_param, b2 = b2_param)
}

Xt_amp_illus <- function(ts){
  b1_param = runif(1,1.7, 2.0)
  b2_param = runif(1,1.7, 2.0)
  sapply(ts, amp_out, b1 = b1_param, b2 = b2_param)
}


Xt_amp_illus2 <- function(ts){
  b1_param = runif(1,.2, .4)
  b2_param = runif(1,.2, .4)
  sapply(ts, amp_out, b1 = b1_param, b2 = b2_param)
}


Xt_sha <- function(ts){
  a1_param <- runif(1,.75, 1.75)
  a2_param <- runif(1,.75, 1.75)
  sapply(ts, sha_out, a1 = a1_param, a2 = a2_param)
}

assign_outliers <- function(mix_row = out_mix_row,
                            candidates = c("magnitude", "amplitude", "shape")){
  sapply(mix_row, function(x) sample(candidates, 1))
}

#function to generate
# mixture_model_mag <- function(n = n_arg, d_arg = 25, t = seq(0, 2*pi, length.out = d_arg),
#                               alpha = 0.05, plott = T){
#   ## generate a mass with n 
#   y_mix <- t(replicate(n, Xt_mass(t = t)))
#   
#   #is curve an outlier or not?
#   out_row <- which((purrr::rbernoulli(n, alpha)))
#   
#   # genrate mag
#   if (length(out_row) != 0) {
#     y_mag <- t(replicate(length(out_row), Xt_mag(t = t)))
#     # replace outliers in original data
#     y_mix[out_row, ] <- y_mag
#   }
#   # create outlier type variable
#   
#   ind_type <- rep("normal", n)
#   ind_type[out_row] <- "magnitude"
#   if(plott){
#     plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
#     for (i in 1:nrow(y_mix)) {
#       if(i %in% out_row){
#         lines(t, y_mix[i,], col = "red", lwd = 0.3)
#       } else{
#         lines(t, y_mix[i,], lwd = .2)
#       }
#       
#     }
#   }
#   return(list(dt = y_mix, index = ind_type))
# }
# 
# mixture_model_amp <- function(n = n_arg, d_arg = 25, t = seq(0, 2*pi, length.out = d_arg),
#                               alpha = 0.05, plott = T){
#   ## generate a mass with n 
#   y_mix <- t(replicate(n, Xt_mass(t = t)))
#   
#   #is curve an outlier or not?
#   out_row <- which((purrr::rbernoulli(n, alpha)))
#   
#   # genrate mag
#   if (length(out_row) != 0) {
#     y_amp <- t(replicate(length(out_row), Xt_amp(ts = t)))
#     # replace outliers in original data
#     y_mix[out_row, ] <- y_amp
#   }
#   # create outlier type variable
#   
#   ind_type <- rep("normal", n)
#   ind_type[out_row] <- "amplitude"
#   if(plott){
#     plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
#     for (i in 1:nrow(y_mix)) {
#       if(i %in% out_row){
#         lines(t, y_mix[i,], col = "red", lwd = 0.3)
#       } else{
#         lines(t, y_mix[i,], lwd = .2)
#       }
#       
#     }
#   }
#   return(list(dt = y_mix, index = ind_type))
# }
# mixture_model_sha <- function(n = n_arg, d_arg = 25, t = seq(0, 2*pi, length.out = d_arg),
#                               alpha = 0.05, plott = T){
#   ## generate a mass with n 
#   y_mix <- t(replicate(n, Xt_mass(t = t)))
#   
#   #is curve an outlier or not?
#   out_row <- which((purrr::rbernoulli(n, alpha)))
#   
#   # genrate mag
#   if (length(out_row) != 0) {
#     y_sha <- t(replicate(length(out_row), Xt_sha(ts = t)))
#     # replace outliers in original data
#     y_mix[out_row, ] <- y_sha
#   }
#   # create outlier type variable
#   
#   ind_type <- rep("normal", n)
#   ind_type[out_row] <- "shape"
#   if(plott){
#     plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
#     for (i in 1:nrow(y_mix)) {
#       if(i %in% out_row){
#         lines(t, y_mix[i,], col = "red", lwd = 0.3)
#       } else{
#         lines(t, y_mix[i,], lwd = .2)
#       }
#       
#     }
#   }
#   return(list(dt = y_mix, index = ind_type))
# }
# mixture_model_mix <- function(n = n_arg, d_arg = 25, t = seq(0, 2*pi, length.out = d_arg),
#                               alpha = 0.05, plott = T){
#   ## generate a mass with n 
#   y_mix <- t(replicate(n, Xt_mass(t = t)))
#   
#   #is curve an outlier or not?
#   out_mix_row <- which((purrr::rbernoulli(n, alpha)))
#   
#   outlier_types <- assign_outliers(mix_row = out_mix_row)
#   mag_row_mix <- out_mix_row[outlier_types == "magnitude"]
#   amp_row_mix <- out_mix_row[outlier_types == "amplitude"]
#   sha_row_mix <- out_mix_row[outlier_types == "shape"]
#   
#   # genrate mag, amp, and sha outliers
#   if (length(mag_row_mix) != 0) {
#     y_mag_mix <- t(replicate(length(mag_row_mix), Xt_mag(t = t)))
#     # replace outliers in original data
#     y_mix[mag_row_mix, ] <- y_mag_mix
#   }
#   
#   if (length(amp_row_mix) != 0) {
#     y_amp_mix <- t(replicate(length(amp_row_mix), Xt_amp(t = t)))
#     y_mix[amp_row_mix, ] <- y_amp_mix 
#   }
#   
#   if (length(sha_row_mix) != 0) {
#     y_sha_mix <- t(replicate(length(sha_row_mix), Xt_sha(t = t)))
#     y_mix[sha_row_mix, ] <- y_sha_mix 
#   }
#   
#   # create outlier type variable
#   
#   ind_mix_type <- rep("normal", n)
#   ind_mix_type[mag_row_mix] <- "magnitude"
#   ind_mix_type[amp_row_mix] <- "amplitude"
#   ind_mix_type[sha_row_mix] <- "shape"
#   if (plott) {
#     plot(x = t, y = y_mix[1,], type = "n", ylim = c(-2,2))
#     
#     for (i in 1:nrow(y_mix)) {
#       if(i %in% sha_row_mix){
#         lines(t, y_mix[i,], col = "#fc8d59")
#       }else if(i %in% amp_row_mix){
#         lines(t, y_mix[i,], col = "#91cf60")
#       } else if(i %in% mag_row_mix){
#         lines(t, y_mix[i,], col = "#67a9cf")
#       } else{
#         lines(t, y_mix[i,], lwd = .2)
#       }
#       
#     }
#     
#   }
#   return(list(dt = y_mix, index = ind_mix_type))
# }







##########################dev tests with plots #######################
#### model 1   ####
##############






## model 3 ####
# sample_model3 <- model3(n = 100, p = 60, out.rate = 0.15)
# 
# dt <- as.data.frame.matrix(sample_model3$data)
# dt <- as.data.frame.matrix(t(as.matrix(dt)))
# dt$x <- seq(0,1, length.out = 60)
# out <-  rep(0,100)
# out[sample_model3$true_outliers] <- 1
# 
# dt_t <- dt %>% gather(key = "key", value = "value", -x)
# dt_t$outlier <- factor(rep(out, each = 60), levels = c(1, 0),
#                        labels = c( "outlier", "normal"))
# 
# 
# ggplot() +
#   geom_line(data = filter(dt_t, outlier == "normal"),
#             aes(x =x,  y = value, group = key),
#             color = "grey") +
#   geom_line(data = filter(dt_t, outlier == "outlier"),
#             aes(x = x,  y = value, group = key),
#             color = "#D55E00") +
#   xlab("t") + 
#   ylab("X(t)")+
#   scale_color_manual(values=c("#f03b20", "grey")) +
#   scale_alpha_manual(values = c(5, .2)) +
#   theme_bw()





### model 4 ####
# sample_model4 <- model4(n = 100, p = 60, out.rate = 0.15)
# dt <- as.data.frame.matrix(sample_model4$data)
# dt <- as.data.frame.matrix(t(as.matrix(dt)))
# dt$x <- seq(0,1, length.out = 60)
# out <-  rep(0,100)
# out[sample_model4$true_outliers] <- 1
# 
# dt_t <- dt %>% gather(key = "key", value = "value", -x)
# dt_t$outlier <- factor(rep(out, each = 60), levels = c(1, 0),
#                        labels = c( "outlier", "normal"))
# 
# 
# ggplot() +
#   geom_line(data = filter(dt_t, outlier == "normal"),
#             aes(x =x,  y = value, group = key),
#             color = "grey") +
#   geom_line(data = filter(dt_t, outlier == "outlier"),
#             aes(x = x,  y = value, group = key),
#             color = "#D55E00") +
#   xlab("t") + 
#   ylab("X(t)")+
#   scale_color_manual(values=c("#f03b20", "grey")) +
#   scale_alpha_manual(values = c(5, .2)) +
#   theme_bw()
##### model 5 #####

# sample_model5 <- model5(n = 100, p = 60, out.rate = 0.15)
# dt <- as.data.frame.matrix(sample_model5$data)
# dt <- as.data.frame.matrix(t(as.matrix(dt)))
# dt$x <- seq(0,1, length.out = 60)
# out <-  rep(0,100)
# out[sample_model5$true_outliers] <- 1
# 
# dt_t <- dt %>% gather(key = "key", value = "value", -x)
# dt_t$outlier <- factor(rep(out, each = 60), levels = c(1, 0),
#                        labels = c( "outlier", "normal"))
# 
# 
# ggplot() +
#   geom_line(data = filter(dt_t, outlier == "normal"),
#             aes(x =x,  y = value, group = key),
#             color = "grey") +
#   geom_line(data = filter(dt_t, outlier == "outlier"),
#             aes(x = x,  y = value, group = key),
#             color = "#D55E00") +
#   xlab("t") + 
#   ylab("X(t)")+
#   scale_color_manual(values=c("#f03b20", "grey")) +
#   scale_alpha_manual(values = c(5, .2)) +
#   theme_bw()

