# for outlyingness transformation, only one-sided DQ and other one sided
# depth should be allowed
seq_transform <- function(dt, sequence = c("T0", "T1", "T2"),
                          depth = c("mbd", "tvd", "extremal", "dirout",
                                    "linfinity", "bd", "erld"),
                          save_data = T,
                          seed = NULL,
                          n_projections = 500L){
  depth <- match.arg(depth)
  outliers <- list()
  if(save_data){
    transformed_data <- list()
  }

  for (transformation in sequence) {
    if (transformation == "T0" || transformation == "D0"){
      ## apply functional boxplot here
      t0_outliers <- functional_boxplot(dt, depth_method = depth)$outliers
      outliers[[transformation]] <- t0_outliers
      if(save_data) transformed_data[[transformation]] <- dt
    }else if(transformation == "T1"){
      dt <- center_curves(dt)
      t1_outliers <- functional_boxplot(dt, depth_method = depth)$outliers
      outliers[[transformation]] <- t1_outliers
      if(save_data) transformed_data[[transformation]] <- dt
    } else if(transformation == "T2"){
      dt <- normalize_curves(dt)
      t2_outliers <- functional_boxplot(dt, depth_measure = depth)$outliers
      outliers[[transformation]] <- t2_outliers
      if(save_data) transformed_data[[transformation]] <- dt
    } else if(transformation == "D1"|| transformation == "D2"){
      dt <- difference_curves(dt)
      d1_outliers <- functional_boxplot(dt, depth_measure = depth)$outliers
      outliers[[transformation]] <- d1_outliers
      if(save_data) transformed_data[[transformation]] <- dt
    } else if(transformation == "O"){
      # implement directional quantile!
      dt <- outlyingness_curves(dt,  n_projections = 500L, seed = NULL)
      #o_outliers <- functional_boxplot(dt, depth_measure = "mbd")
      #outliers[[transformation]] <- o_outliers
      #if(save_data) transformed_data[[transformation]] <- dt

    } else if(transformation == "R"){
      #dt <- warp_transform(dt)
      #r_outliers <- functional_boxplot(dt, depth_measure = "mbd")
      #outliers[[transformation]] <- r_outliers
      #if(save_data) transformed_data[[transformation]] <- dt
    }

  }

}


center_curves <- function(dt){
  centered <- dt - rowMeans(dt)
}


normalize_curves <- function(dt){
  return(dt/sqrt(rowSums(dt^2)))
}

difference_curves <- function(dt){
  p <- ncol(dt)
  dt[,2:p] - dt[, 1:(p-1)]
}

outlyingness_curves <- function(dt, n_projections = 500L, seed = NULL){
  dim_data <- dim(dt)
  if(length(dim_data) == 2){# data is univariate
    data  <- t(dt)
    median_vec <- apply(data, 1, median)
    mad_vec <- apply(data, 1, mad)
    return(t(abs(data-median_vec)/mad_vec))
  } else if (length(dim_data) == 3) { # data is multivariate
    outlyingness <- apply(dt, 2, function(x){
      (1/projection_depth(x, n_projections = n_projections, seed = seed)) - 1
    })
    return(outlyingness)
  } else{
    stop("Argument \"dt\" must be a 2 or 3 dimensional array.")
  }

}

warp_curves <- function(dt){

}
