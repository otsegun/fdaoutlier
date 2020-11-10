functional_boxplot <- function(dt,
                               depth_method = c("mbd", "tvd", "extremal", "dirout",
                                                "linfinity", "bd", "erld"),
                               depth_values = NULL,
                               emp_factor = 1.5,
                               central_region = 0.5){
  if(is.null(depth_values)){
    depth_method <- match.arg(depth_method)
    if(depth_method == "mbd"){
      depth_values <- modified_band_depth(dt)
    }else if( depth_method == "tvd"){
      depth_values <- total_variation_depth(dt)
    }else if(depth_method == "extremal"){
      depth_values <- extremal_depth(dt)
    }else if(depth_method == "dirout"){
      depth_values <- -dir_out(dt)$distance
    }else if(depth_method == "linfinity"){
      depth_values <- l_infinity_depth(dt)
    }else if(depth_method == "bd"){
      depth_values <- band_depth(dt)
    } else if(depth_method == "erld"){
      depth_values <- extreme_rank_length(dt)
    }else{
      cat(depth_method, " not supported \n")
    }
  }

  sorted_depths <- sort(depth_values, decreasing = T, index.r = T)
  index_sorted_depth <- sorted_depths$ix
  sorted_depths <- sorted_depths$x
  median_curve <- index_sorted_depth[1]

  n <- nrow(dt)
  #p <- ncol(dt)

  n_obs_central <- ceiling(n*central_region) # at least 50%
  center <- dt[index_sorted_depth[1:n_obs_central], ]
  #out=fit[,index[(m+1):n]]## replaced downwards
  inf <- apply(center,2,min)
  sup <- apply(center,2,max)
  dist <- emp_factor*(sup-inf)
  upper <- sup + dist
  lower <- inf - dist
  dt_t <- t(dt)
  outlier_test <- (dt_t <= lower) + (dt_t >= upper)
  outliers <- which(colSums(outlier_test) > 0)
  return(list(outliers = outliers,
              depth_values = depth_values,
              median_curve = median_curve))
}

band_depth <- function(dt){
  n <- nrow(dt)
  rank_matrix <- apply(dt, 2, rank)
  down <- apply(rank_matrix, 1, min) - 1
  up <- n-apply(rank_matrix, 1, max)
  unname((up*down+n-1)/choose(n,2))
}


modified_band_depth <- function(dt){
  p <- ncol(dt)
  n <- nrow(dt)
  rnkmat <- apply(dt,2,rank) # switches matrix to max(p,n) by min(p,n)
  down <- rnkmat-1
  up <- n-rnkmat
  unname((rowSums(up*down)/p+n-1)/choose(n,2))
}

# dts <- sim_data1$data[1:9, 1:10]
# all.equal(modified_band_depth(dts),fMBD(t(dts)))
# all.equal(band_depth(dts),fBD2(t(dts)))

l_infinity_depth <- function(dt){
  # distance very large if n is big.
  # to cpp for memory?
  distances <- as.matrix(dist(dt, method = "maximum", diag = T, upper = T))
  unname(1/(1+colMeans(distances)))
}

# l_infinity_depth(sim_data1$data)
