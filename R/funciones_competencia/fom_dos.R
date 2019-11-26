

rhoHuber = function(x,c=2.1){
  # x is a univariate sample
  # c is the tuning constant
  # output is rho(x)
  #
  rho = (x/c)^2
  rho[rho > 1] = 1
  1.54^2*rho
}

loc1StepM = function(x,c1=3,precScale=1e-12) {
  # Computes the first step of an algorithm for
  # a location M-estimator using the biweight.
  # Note that c1 is expressed in unnormalized MAD units.
  # In the usual units it is thus c1/qnorm(3/4).
  #
  x = x[!is.na(x)] # we always take out NAs
  medx = median(x)
  ax = abs(x - medx)
  denom = c1 * median(ax)
  mu = if(denom > precScale) {
    ax = ax/denom
    w = 1 - ax * ax
    w = ((abs(w) + w)/2)^2
    sum(x * w)/sum(w)
  }
  else medx
  return(mu)
}

scale1StepM = function(x,rhofunction=rhoHuber,precScale=1e-10) {
  # Computes the first step of an algorithm for
  # a scale M-estimator using the given rho function. 
  # The scatter is computed relative to zero.
  #
  x = x[!is.na(x)] # we always take out NAs
  n = length(x)
  if(n == 0) { return(0.0)
  } else {
    sigma0 = 1.4826*fastMedian(abs(x))
    if(sigma0 < precScale) { return(0.0)
    } else {
      rho = rhofunction(x/sigma0)
      return(sigma0 * sqrt(sum(rho)*2/n))
    }
  }
}

corrGK = function(x,y,rhofunction=rhoHuber,precScale=1e-12) {
  # Computes correlation between x and y after normalization of data
  # x and y should be vectors of the same length
  #
  a = scale1StepM(x+y,precScale=precScale,rhofunction = rhofunction)^2 
  b = scale1StepM(x-y,precScale=precScale,rhofunction = rhofunction)^2
  corr = (a-b)/(a+b)
  return(corr)
}

CorrGK = function(x,rhofunction=rhoHuber,precScale=1e-12){
  # Computes a correlation matrix of the variables in x
  # Assumes x is normalized
  #
  d = dim(x)[2]
  cormat = array(0.0,dim = c(d,d))
  cormat[lower.tri(cormat)] = combn(d, 2,
                                    FUN=function(ind) corrGK(x[,ind[1]],x[,ind[2]]))
  cormat = t(cormat)+cormat+diag(d)
  return(cormat)
}

fastSplitSample = function(x){
  # Centers sample by median, and divides in 2 equal halves.
  # Assumes that NAs have already been removed.
  # This function has time complexity O(n).
  #
  med = fastMedian(x) # takes only O(n) time
  x = x - med # centering
  n = length(x)  
  h = n %/% 2   # = integer part of n/2
  xa = x[x > 0] # length(xa) <= h
  xb = x[x < 0] # length(xa) <= h 
  xa = c(rep(0,(n - h - length(xa))),xa)
  xb = c(rep(0,(n - h - length(xb))),abs(xb)) # abs() ! 
  return(list(xa=xa,xb=xb,med=med))
}


compScales = function(x,robScale=scale1StepM,rhofunction=rhoHuber,
                      rmZeroes=FALSE,maxRatio=NULL,precScale=1e-10){
  # Computes the scales sa and sb (above and below the median).
  # Assumes that x is an array of numbers.
  #
  x = x[!is.na(x)] # we always take out NAs
  temp = fastSplitSample(x)
  xa   = temp$xa
  xb   = temp$xb
  med  = temp$med
  sall = robScale((x-med),rhofunction=rhofunction,precScale=precScale)
  if(rmZeroes){ # reduces breakdown value but yields fewer implosions
    xa = xa[xa > precScale]
    xb = xb[xb > precScale]
  }
  sa = robScale(xa,rhofunction=rhofunction,precScale=precScale)
  sb = robScale(xb,rhofunction=rhofunction,precScale=precScale)
  if(!is.null(maxRatio)){ 
    if(maxRatio < 2) stop("maxRatio must be at least 2")
    sa = min(c(max(sa,sall/maxRatio,na.rm = TRUE),sall*maxRatio),
             na.rm = TRUE)
    sb = min(c(max(sb,sall/maxRatio,na.rm=TRUE),sall*maxRatio),
             na.rm = TRUE)
  } 
  return(list(sa=sa,sb=sb,med=med))
}


DO_univ = function(x1,x2=NULL,robScale=scale1StepM,rhofunction=rhoHuber,
                   rmZeroes=FALSE,maxRatio=NULL,precScale=1e-10){
  # Assumes that x1,x2 are arrays of numbers
  # Computes the directional outlyingness of each element in x2
  #          with respect to x1
  # "rmZeroes" reduces the breakdown value but yields fewer implosions
  # "maxRatio" increases the breakdown value but reduces adaptation  
  #            to skewness.
  #
  if(is.null(x2)){x2=x1}
  y    = array(NA,dim=c(length(x2),3))
  temp = compScales(x1,robScale=robScale,rhofunction=rhofunction,
                    rmZeroes=rmZeroes,maxRatio=maxRatio,precScale=precScale)
  sa   = temp$sa
  sb   = temp$sb
  med  = temp$med
  inda = which(x2 > med)
  indb = which(x2 < med)
  indm = which(x2 == med)
  y[inda,1] = x2[inda]-med
  y[inda,2] = sa
  y[inda,3] = y[inda,1]/y[inda,2]
  y[indb,1] = med-x2[indb]
  y[indb,2] = sb
  y[indb,3] = y[indb,1]/y[indb,2]
  y[indm,1] = 0
  y[indm,2] = 0 
  y[indm,3] = 0   
  return(y) # columns contain: DO numerator, DO denominator and DO
}


generdir=function(X,ndir){
  # Generates `ndir' directions (with unit norm),
  # orthogonal to d-subsets of X
  # Assumes dim(X) = n x d
  #
  X = data.matrix(X)
  n = dim(X)[1]
  d = dim(X)[2]
  i = 0 
  j = 0 
  B = array(0.0,dim=c(ndir,d))
  while(i<ndir){
    j = j+1
    set.seed(j)
    indices = sample(n,d)
    Atemp   = X[indices,,drop=FALSE]
    E       = matrix(1, d, 1)
    if ((qrP = qr(Atemp, tol = 1e-12))$rank == d) {
      B[i+1,] = solve(qrP, E, tol = 1e-12)
      i = i + 1L
    }
  }
  Bnorm  = sqrt(rowSums(B^2))
  Nx     = mean(abs(unname(X)))
  keep   = Bnorm * Nx > 1e-12
  Bnormr = Bnorm[keep]
  B = B[keep, , drop = FALSE]
  A = B/matrix(data = Bnormr, ncol=ncol(B),nrow=nrow(B))
  # rows contain unit norm vectors
  negatives = which(A[,1]<0)
  A[negatives,] = -A[negatives,]
  return(A)
}


DO_multiv = function(X1,X2=NULL,rhofunction=rhoHuber,
                     robScale=scale1StepM,rmZeroes=FALSE,
                     maxRatio=NULL,precScale=1e-10,lb=NULL,
                     algo="compWise"){
  # Computes the directional outlyingness in the multivariate
  #   setting, of each d-variate vector in X
  # Assumes that dim(X) = n x d
  # "lb" puts a lower bound on the denominator of the DO, to avoid
  #   implosion. 
  # algo specifies the algorithm used and is one of 
  # {"compWise","PP","spearman","GK"}
  #
  if(is.null(X2)){
    n    = dim(X1)[1]
    d    = dim(X1)[2]
    if (prod(apply(X1,1,identical,X1[1,]),na.rm=TRUE)==1){
      return(list(don=rep(0,n), dod=rep(0,n),do=rep(0,n)))
    }else{
      if(algo=="compWise"){
        out_temp      = apply(X1,MARGIN = 2,FUN = function(x) 
          DO_univ(x1=x,rhofunction=rhofunction,robScale=robScale,
                  rmZeroes=rmZeroes,maxRatio=maxRatio,
                  precScale=precScale)[,1:2])
        dim(out_temp) = c(n,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n,d))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo  = don_temp/dod_temp}
        do_temp       = apply(tempdo,MARGIN = 1,
                              FUN = function(x) sqrt(sum(x^2)))
        dod_temp      = apply(dod_temp,MARGIN = 1,
                              FUN = function(x) abs(prod(x)))
        don_temp      = do_temp*dod_temp
      }else if(algo=="PP"){
        ndir = 250*d
        A    = generdir(X1,ndir=ndir) # generates `ndir' directions
        Y    = X1 %*% t(A)
        out_temp      = apply(X = Y,MARGIN = 2,FUN= function(x) 
          DO_univ(x1=x,rhofunction=rhofunction,robScale=robScale,
                  rmZeroes=rmZeroes,maxRatio=maxRatio,
                  precScale=precScale)[,1:2])
        dim(out_temp) = c(n,2,ndir)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n,ndir))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo = don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1, 
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indix    = cbind(1:n,indexmax) 
        don_temp = don_temp[indix] 
        dod_temp = dod_temp[indix] 
        do_temp  = tempdo[indix]
      }else if(algo=="spearman"){
        # Robust standardization
        locX     = apply(X1,2,FUN=loc1StepM,precScale=precScale)
        Z        = sweep(X1,2,locX)
        scaleX   = apply(Z,2,FUN=scale1StepM,precScale=precScale)
        Z        = sweep(Z,2,scaleX,"/")
        # Orthogonalization
        R = cor(x = Z,method = "spearman") # correlation matrix
        Lambda   = eigen(x = R,symmetric = "TRUE")$vectors 
        # matrix containing eigenvectors in columns
        Zo       = Z%*%Lambda # orthogonalized Z
        out_temp = apply(Zo,MARGIN = 2,FUN = function(x) 
          DO_univ(x1=x,rhofunction=rhofunction,robScale=robScale,
                  rmZeroes=rmZeroes,maxRatio=maxRatio,
                  precScale=precScale)[,1:2])
        dim(out_temp) = c(n,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n,d))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo=don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1, 
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indices  = cbind(1:n,indexmax)
        do_temp  = tempdo[indices]
        dod_temp = dod_temp[indices]
        don_temp = don_temp[indices]
      }else if(algo=="GK"){
        # Robust standardization
        locX     = apply(X1,2,FUN=loc1StepM,precScale=precScale)
        Z        = sweep(X1,2,locX)
        scaleX   = apply(Z,2,FUN=scale1StepM,precScale=precScale)
        Z        = sweep(Z,2,scaleX,"/")
        # Orthogonalization
        R        = CorrGK(x = Z,rhofunction = rhofunction,
                          precScale = precScale) # correlation matrix
        Lambda   = eigen(x = R,symmetric = "TRUE")$vectors
        # matrix containing eigenvectors in columns
        Zo       = Z%*%Lambda # orthogonalized Z
        out_temp = apply(Zo,MARGIN = 2,FUN = function(x) 
          DO_univ(x1=x,rhofunction=rhofunction,robScale=robScale,
                  rmZeroes=rmZeroes,maxRatio=maxRatio,
                  precScale=precScale)[,1:2])
        dim(out_temp) = c(n,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n,d))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo=don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1,
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indices  = cbind(1:n,indexmax)
        do_temp  = tempdo[indices]
        dod_temp = dod_temp[indices]
        don_temp = don_temp[indices]
      }
      else{stop(paste("algo = ",algo," is not valid",sep=""))}
      return(list(don=don_temp, dod=dod_temp,do=do_temp))
    }
  }else{
    n1 = dim(X1)[1]
    d  = dim(X1)[2]
    n2 = dim(X2)[1]
    if (prod(apply(X1,1,identical,X1[1,]),na.rm=TRUE)==1){
      return(list(don=rep(0,n2), dod=rep(0,n2)))
    }else{
      if(algo=="compWise"){
        out_temp = apply(rbind(X1,X2),MARGIN = 2,FUN = function(t) 
          DO_univ(x1=t[1:n1],x2 = t[(n1+1):(n1+n2)],rhofunction
                  =rhofunction,robScale=robScale,rmZeroes=rmZeroes,
                  maxRatio=maxRatio,precScale=precScale)[,1:2])
        dim(out_temp) = c(n2,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n2,d))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo         = don_temp/dod_temp}
        do_temp  = apply(tempdo,MARGIN = 1,FUN =
                           function(x)  sqrt(sum(x^2)))
        dod_temp = apply(dod_temp,MARGIN = 1,FUN =
                           function(x) abs(prod(x)))
        don_temp = do_temp*dod_temp
      }else if(algo=="PP"){
        ndir = 250*d
        A    = generdir(X1,ndir=ndir)
        Y1   = X1 %*% t(A)
        Y2   = X2 %*% t(A)
        out_temp = apply(rbind(Y1,Y2),MARGIN = 2,FUN = function(t) 
          DO_univ(x1=t[1:n1],x2 = t[(n1+1):(n1+n2)],rhofunction=
                    rhofunction,robScale=robScale,rmZeroes=rmZeroes,
                  maxRatio=maxRatio,precScale=precScale)[,1:2])
        dim(out_temp) = c(n2,2,ndir)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n2,ndir))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo = don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1, 
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indix    = cbind(1:n2,indexmax) 
        don_temp = don_temp[indix] 
        dod_temp = dod_temp[indix] 
        do_temp  = tempdo[indix]
      }else if(algo=="spearman"){
        # Robust standardization
        locX1     = apply(X1,2,FUN=loc1StepM,precScale=precScale)
        Z1        = sweep(X1,2,locX1)
        scaleX1   = apply(Z1,2,FUN=scale1StepM,precScale=precScale)
        Z1        = sweep(Z1,2,scaleX1,"/")
        Z2        = sweep(X2,2,locX1)
        Z2        = sweep(Z2,2,scaleX1,"/")
        # Orthogonalization
        R      = cor(x = Z1,method = "spearman") # correlation matrix
        Lambda = eigen(x = R,symmetric = "TRUE")$vectors
        # matrix containing eigenvectors in columns
        Zo1    = Z1%*%Lambda # orthogonalized Z1
        Zo2    = Z2%*%Lambda # orthogonalized Z1
        
        out_temp = apply(rbind(Zo1,Zo2),MARGIN = 2,FUN = function(t) 
          DO_univ(x1=t[1:n1],x2 = t[(n1+1):(n1+n2)],rhofunction=
                    rhofunction,robScale=robScale,rmZeroes=rmZeroes,
                  maxRatio=maxRatio,precScale=precScale)[,1:2])
        dim(out_temp) = c(n2,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp=pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n2,ndir))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo = don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1,
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indix    = cbind(1:n2,indexmax) 
        don_temp = don_temp[indix] 
        dod_temp = dod_temp[indix] 
        do_temp  = tempdo[indix]
      }else if(algo=="GK"){
        # Robust standardization
        locX1     = apply(X1,2,FUN=loc1StepM,precScale=precScale)
        Z1        = sweep(X1,2,locX1)
        scaleX1   = apply(Z1,2,FUN=scale1StepM,precScale=precScale)
        Z1        = sweep(Z1,2,scaleX1,"/")
        Z2        = sweep(X2,2,locX1)
        Z2        = sweep(Z2,2,scaleX1,"/")
        # Orthogonalization
        R         = CorrGK(x = Z1,rhofunction = rhofunction,
                           precScale = precScale) # correlation matrix
        Lambda    = eigen(x = R,symmetric = "TRUE")$vectors
        # matrix containing eigenvectors in columns
        Zo1       = Z1%*%Lambda # orthogonalized Z1
        Zo2       = Z2%*%Lambda # orthogonalized Z1
        out_temp = apply(rbind(Zo1,Zo2),MARGIN = 2,FUN = function(t) 
          DO_univ(x1=t[1:n1],x2 = t[(n1+1):(n1+n2)],rhofunction=
                    rhofunction,robScale=robScale,rmZeroes=rmZeroes,
                  maxRatio=maxRatio,precScale=precScale)[,1:2])
        dim(out_temp) = c(n2,2,d)
        don_temp      = out_temp[,1,] 
        dod_temp      = out_temp[,2,] 
        if(!is.null(lb)){dod_temp = pmax(dod_temp,lb,na.rm = TRUE)}
        tempdo        = array(0.0,dim=c(n2,ndir))
        zeroindices   = which(don_temp<precScale)
        if(!identical(zeroindices,integer(0))){
          tempdo[-zeroindices] = don_temp[-zeroindices]/
            dod_temp[-zeroindices]
        }else{tempdo = don_temp/dod_temp}
        indexmax = unlist(apply(tempdo,1, 
                                function(x) min(which(x==max(x,na.rm=TRUE))))) 
        indix    = cbind(1:n2,indexmax) 
        don_temp = don_temp[indix] 
        dod_temp = dod_temp[indix] 
        do_temp  = tempdo[indix]
      }
      return(list(don=don_temp, dod=dod_temp,do=do_temp))
    }
  }
}


DO_functional = function(data,robScale=scale1StepM,weights=NULL,
                         rhofunction=rhoHuber,univ=FALSE,
                         rmZeroes=FALSE,maxRatio=NULL,
                         precScale=1e-10,lb="elog",algo="compWise"){
  # Computes the directional outlyingness of each function in data
  # Assumes dim(data) =
  # n x t          for univariate functions with univariate domain
  # n x k x j      for univariate functions with bivariate domain
  # n x t x d      for multivariate functions with univariate domain
  # n x k x j x d  for multivariate functions with bivariate domain
  # Put univ = TRUE if your functions are univariate
  # "algo" determines the algorithm used for multivariate data
  # 
  if(!is.null(weights)){weights[weights!=0] = 1}
  if(univ){
    dims = dim(data)
    n    = dims[1]
    t    = dims[2]
    if (length(dims)==3){
      data    = apply(data,c(1),as.vector)
      weights = as.vector(weights)
      data    = aperm(data,c(2,1))
      t       = dims[2]*dims[3]
    }
    if(is.null(weights)){
      # Give zero weight to gridpoints with mad < precScale 
      weights = apply(data,MARGIN = 2,
                      FUN = function(y) mad(y,na.rm=TRUE)>precScale)
    }
    OUTPUT                = array(NA,dim = c((n*2),t))
    OUTPUT[,(weights==1)] = apply(X=data[,(weights==1)],MARGIN = 2,
                                  FUN=function(x) DO_univ(x1 = x,rhofunction=rhofunction,
                                                          robScale=robScale,rmZeroes=rmZeroes,maxRatio=maxRatio,
                                                          precScale=precScale)[,1:2])
    dim(OUTPUT)           = c(n,2,t)
    don                   = OUTPUT[,1,]
    dod                   = OUTPUT[,2,]
    if(!is.null(lb)){
      tempdo = don/dod
      index  = rep(NA,t)
      index[(weights==1)] = unlist(apply(tempdo[,(weights==1)],2,
                                         FUN = function(y)  min(which(y==max(y,na.rm=TRUE)),
                                                                na.rm = TRUE)))
      indices = cbind(index,1:t)
      lowb    = dod[indices]
      lowb    = lowb[!is.na(lowb)]
      if(lb=="elog"){lowerBound = exp((median(log(lowb[lowb>0]),
                                              na.rm = TRUE)-2*mad(log(lowb[lowb>0]),na.rm = TRUE)))
      }else{lowerBound = lb}
      dod = pmax(dod,lowerBound,na.rm=TRUE)
    }
    DO_out = don/dod
    if(length(dims)==3){dim(DO_out)=dims[1:3]}
    return(DO_out)
  }else{ # if not univariate response, i.e. d>1
    dims = dim(data)
    n    = dims[1]
    t    = dims[2]
    d    = dims[length(dims)]
    if (length(dims)==4){
      data    = apply(data,c(1,4),as.vector)
      if(!is.null(weights)){weights = as.vector(weights)}
      data    = aperm(data,c(2,1,3))
      t       = dims[2]*dims[3]
    }
    if(is.null(weights)){
      # Give zero weight to gridpoints with mad < precScale
      # in at least one dimension 
      weights = apply(data,MARGIN = 2,FUN = function(y) 
        prod(apply(X=y,MARGIN=2,FUN = mad,na.rm=TRUE)>precScale))
    }
    if(algo!="compWise"){
      OUTPUT = apply(X = data[,(weights==1),],MARGIN=2,FUN = DO_multiv,
                     X2=NULL,robScale=robScale,rhofunction=rhofunction,
                     lb=NULL,rmZeroes=rmZeroes,maxRatio=maxRatio,
                     precScale=precScale,algo=algo)
      don    = sapply(OUTPUT,"[[",1)
      dod    = sapply(OUTPUT,"[[",2)
      do     = sapply(OUTPUT,"[[",3)
      if(!is.null(lb)){
        if(lb=="elog"){
          lowb   = as.vector(dod)
          lowerBound = exp((median(log(lowb[lowb>0]),na.rm = TRUE)
                            -2*mad(log(lowb[lowb>0]),na.rm = TRUE)))
        }else{lowerBound = lb}
        zerodods = apply(dod,2,function(x) any(x==0,na.rm=TRUE)) 
        zerodods = which(zerodods==TRUE)
        dod      = pmax(dod,lowerBound,na.rm=TRUE)
        DO_temp  = don/dod 
        if(length(zerodods)!=0){
          data_zerodods = data[,zerodods,]
          if(length(zerodods)==1){dim(data_zerodods) = c(n,1,d)}
          OUTPUT_zerodods = apply(X = data_zerodods,MARGIN=2,
                                  FUN=DO_multiv,X2=NULL,robScale=robScale,
                                  rhofunction=rhofunction,lb=lowerBound,
                                  rmZeroes=rmZeroes,maxRatio=maxRatio,
                                  precScale=precScale,algo=algo) 
          # This time with lb = lowerBound
          DO_zerodods        = sapply(OUTPUT_zerodods,"[[",3)
          DO_temp[,zerodods] = DO_zerodods
        }
      }else{DO_temp = don/dod}
      DO_out = array(NA,dim   = c(n,t))
      DO_out[,(weights==1)] = DO_temp
    }else{
      OUTPUT = apply(X = data[,(weights==1),],MARGIN = c(2,3),
                     FUN = function(y) DO_univ(x1 = y,robScale=robScale,
                                               rhofunction=rhofunction,rmZeroes=rmZeroes,
                                               maxRatio=maxRatio,precScale=precScale)[,1:2])
      dim(OUTPUT) = c(n,2,sum(weights),d)
      donperdim   = OUTPUT[,1,,]
      dodperdim   = OUTPUT[,2,,]
      if(!is.null(lb)){
        if(lb=="elog"){
          lowerBounds = apply(dodperdim,3,FUN = function(y) 
            exp((median(log(as.vector(y)[as.vector(y)>0]),na.rm=TRUE)
                 -2*mad(log(as.vector(y)[as.vector(y)>0]),na.rm=TRUE))))
          dodperdim = mapply(FUN=function(x,y) pmax(dodperdim[,,x],y),
                             1:d,lowerBounds)
          dim(dodperdim) = c(n,sum(weights),d)
        }else{
          lowerBound = lb
          dodperdim  = pmax(lowerBound,dodperdim)
        }}
      doperdim = donperdim/dodperdim
      DO_temp  = apply(doperdim,MARGIN = c(1,2),
                       FUN = function(y) sqrt(sum(y^2)))
      DO_out   = array(NA,dim   = c(n,t))
      DO_out[,(weights==1)] = DO_temp
    }
    if(length(dims)==4){dim(DO_out) = dims[1:3]}
    return(DO_out)
  }
} 


findoutliersDO = function(x,quant=0.995){
  # Finds outliers in x using directional outlyingness
  # x can be univariate or multivariate
  # Assumes that dim(x) = n x d if multivariate
  #  
  d = dim(x)[2]
  if(is.null(d)){temp = DO_univ(x1 = x)[,3]}
  else{
    temp = DO_multiv(X1 = x,algo = "PP")$do
  }
  Ltemp   = log(0.1+temp)
  cutoff  = exp(qnorm(quant)*mad(Ltemp)+median(Ltemp))-0.1
  indices = which(temp>cutoff)
  return(list(indices=indices,DO=temp,cutoff=cutoff))
}


SDO = function(x,z=NULL){
  # Computes the Stahel-Donoho outlyingness of every element in x
  # x can be univariate or multivariate
  # Assumes that dim(x) = n x d if multivariate
  #
  if(is.null(z)){
    if(is.null(dim(x))){
      out = abs(x-median(x))/mad(x)
      return(list(SDO=out))
    }else{
      n = dim(x)[1]
      d = dim(x)[2]
      ndir = 250*d
      A = generdir(x,ndir=ndir) # generates `ndir' directions
      Y = x %*% t(A)
      out_temp = apply(X=Y,MARGIN=2,
                       FUN= function(t) abs(t-median(t))/mad(t))
      out = apply(out_temp,1,max)
      indexmax=unlist(apply(out_temp,1, 
                            function(x) min(which(x==max(x,na.rm=TRUE)))))
      # minimum takes first index in case of ties
      directions=A[indexmax,]
      return(list(SDO=out,directions=directions))
    }
  }else{
    n = dim(x)[1]
    d = dim(x)[2]
    n2 = dim(z)[1]
    ndir = 250*d
    A = generdir(x,ndir=ndir) # generates `ndir' directions
    Y = x %*% t(A)
    Z = z %*% t(A) # Each column is a dataset
    medians = apply(Y,MARGIN = 2,FUN = median)
    mads = apply(Y,MARGIN = 2,FUN = mad) # mads of all columns
    out_temp = abs(Z-matrix(data=medians,nrow = n2,ncol=ndir,
                            byrow=TRUE))/matrix(data=mads,nrow=n2,ncol=ndir,byrow=TRUE)
    out = apply(out_temp,1,max)
    return(list(SDO=out))
  }
}


findoutliersSDO = function(x,quant=0.995){
  # Finds outliers in x using Stahel-Donoho outlyingness
  # x can be univariate or multivariate
  # Assumes that dim(x) = n x d if multivariate
  #
  temp    = SDO(x)$SDO
  # if(!is.null(dim(x)[2])){d = dim(x)[2]} else {d=1}
  Ltemp   = log(0.1+temp)
  cutoff  = exp(qnorm(quant)*mad(Ltemp)+median(Ltemp))-0.1
  indices = which(temp>cutoff)
  return(list(indices=indices,SDO=temp,cutoff=cutoff))
}


DO_heatmap_image = function(DO,cap=15){
  # Constructs the DO heatmap
  # Assumes that dim(DO) = j x k 
  #  
  DO[is.na(DO)] = 0
  DO[DO>cap]    = cap
  dim           = dim(DO)
  x             = 1:dim[1]
  y             = 1:dim[2]
  xy            = expand.grid(x=x,y=y)
  PlotData      = as.data.frame(cbind(xy,as.vector(DO)))
  colnames(PlotData) = c("x","y","DO")
  Plot = ggplot()
  Plot = Plot + geom_tile(data=PlotData,aes(x,y,fill=DO))+
    coord_fixed()
  Plot = Plot + scale_fill_gradientn(colours= c(low='white',"red",
                                                high="darkred"),limits=c(0, cap),guide=FALSE)
  Plot = Plot + theme(axis.ticks=element_blank(),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())
  Plot = Plot + theme(legend.key.width=unit(0,"null"),
                      legend.key.height=unit(0,"null"))
  Plot = Plot + theme(panel.margin=unit(0,"null"),
                      axis.ticks.length = unit(0,"null"),
                      legend.margin=unit(0,"null"))
  Plot = Plot + scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  Plot = Plot + theme( panel.border = element_rect(colour = 'black',
                                                   fill = 'transparent'))
  return(Plot)
}


fDO = function(DO,weights=NULL,cutoff=FALSE){
  # Computes functional directional outlyingness
  # cutoff=TRUE returns the cutoff of the fDO values
  #
  if(is.null(weights)){
    weights=apply(DO,MARGIN = 2:(length(dim(DO))),FUN = function(y) 
      (prod(is.na(y))==0)) 
    # gridpoints with only NA receive weight 0
  }
  weights = weights/sum(weights,na.rm = TRUE) 
  # makes sure sum(weights) == 1
  fDOvalues = apply(DO,1,FUN= function(y) sum(weights*y,na.rm=TRUE))
  if(cutoff){
    LfDOvalues = log(0.1+fDOvalues)
    cut  = exp(qnorm(0.995)*mad(LfDOvalues)+median(LfDOvalues))-0.1
    return(list(fDOvalues=fDOvalues,Cutoff=cut))
  }else{return(fDOvalues)}
}


vDO = function(DO,weights=NULL,cutoff=FALSE){
  # Computes functional directional outlyingness
  # cutoff=TRUE returns the cutoff of the vDO values
  #
  if(is.null(weights)){
    weights = apply(DO,MARGIN = 2:(length(dim(DO))),
                    FUN = function(y) (prod(is.na(y))==0)) 
    # gridpoints with only NAs receive weight 0
  }
  weights     = weights 
  fDO_temp    = fDO(DO = DO,weights = weights)
  sDOw = apply(DO,1,FUN = function(y)
    sqrt(sum(weights*(y-sum(weights*y,na.rm=TRUE)/
                        sum(weights,na.rm = TRUE))^2,na.rm = TRUE)/
           (sum(weights)-1))) # weighted sd
  vDOvalues   = sDOw/(1 + fDO_temp)
  if(cutoff){
    LvDOvalues = log(0.1+vDOvalues)
    cut = exp(qnorm(0.995)*mad(LvDOvalues)+median(LvDOvalues))-0.1
    return(list(vDOvalues=vDOvalues,Cutoff=cut))
  }else{return(vDOvalues)}
}


FOM = function(DO,weights=NULL,cfDO=FALSE,cvDO=FALSE){
  # Constructs the functional outlier map
  # Assumes that dim(DO) = 
  # n x t     for functional data with univariate domain
  # n x j x k for functional data with bivariate domain
  # cfDO = TRUE draws extra cutoff on fDO
  # cvDO = TRUE draws extra cutoff on vDO
  #
  dims = dim(DO)
  if(is.null(weights)){
    weights = apply(DO,MARGIN = 2:(length(dim(DO))),
                    FUN = function(y) (prod(is.na(y))==0)) 
    # gridpoints with only NAs receive weight 0
  }
  DO[is.infinite(DO)] = 0
  temp      = fDO(DO = DO,weights = weights,cutoff = TRUE)
  fDOvalues = temp$fDOvalues
  cutfDO    = temp$Cutoff
  DO[DO==0] = NA
  temp      = vDO(DO = DO , weights = weights,cutoff = TRUE)
  vDOvalues = temp$vDOvalues
  cutvDO    = temp$Cutoff
  CFO       = log(0.1+sqrt((fDOvalues/median(fDOvalues))^2+
                             (vDOvalues/median(vDOvalues))^2))
  Fence     = qnorm(0.995)*mad(CFO)+median(CFO)
  theta     = seq(0,  pi/2, length=(100))
  x         = median(fDOvalues)*(exp(Fence)-0.1) * cos(theta) 
  y         = median(vDOvalues)*(exp(Fence)-0.1) * sin(theta) 
  colorvec                   = rep("black",length(CFO))
  colorvec[which(CFO>Fence)] = "red"
  shapevec                   = rep(16,length(CFO)) #
  shapevec[which(CFO>Fence)] = 15 
  PlotData     = data.frame(row.names = 1:dims[1])
  PlotData$fDO = fDOvalues
  PlotData$vDO = vDOvalues
  Plot   = ggplot(PlotData)
  Plot   = Plot + geom_point(mapping = aes_string(x = "fDO",
                                                  y = "vDO"), color = colorvec, size = 3,
                             shape=shapevec)
  xrange = ggplot_build(Plot)$panel$ranges[[1]]$x.range
  yrange = ggplot_build(Plot)$panel$ranges[[1]]$y.range 
  Plot   = Plot + xlab("fDO") + ylab("vDO")
  Plot   = Plot + geom_path(mapping = aes(x=x,y=y,shape=NULL),
                            data =cbind.data.frame(x,y),
                            color="black",linetype=2,size=1)
  if(cfDO){
    Plot = Plot + geom_vline(xintercept=cutfDO,linetype=3,size=1)
  }
  if(cvDO){
    Plot = Plot + geom_hline(yintercept=cutvDO,linetype=3,size=1)
  }
  Plot   = Plot + coord_cartesian(ylim = yrange,xlim = c(xrange))
  Plot   = Plot + scale_x_continuous(expand = c(0, 0))  
  Plot   = Plot + scale_y_continuous(expand = c(0, 0)) 
  Plot   = Plot + theme_classic(base_size = 12, base_family = "")+
    theme(panel.background = element_rect(fill = NA, 
                                          colour = "black", size = 1),
          axis.title.x = element_text(size = 20,vjust=-2),
          axis.title.y = element_text(size = 20,angle=90,vjust=2),
          text = element_text(size=20),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.line = element_line(size = 0))
  return(Plot)
}


DO_heatmap_sample = function(DO,subset=NULL,sortfDO=FALSE,cap=10,
                             weights=NULL,XLAB="gridpoint",
                             YLAB="Observation no.",breaks=NULL,
                             labels=NULL){
  # Constructs a heatmap of functions with a univariate domain
  # sortfDO=TRUE sorts the functions based on their fDO value 
  #         (in descending order)
  # subset is an optional vector of indices limiting the plot to
  #         only these functions
  #
  avoid_overlap = function(x,spacing){
    # Used by DO_heatmap_sample for the layout of the labels
    # 
    ind = seq_along(x) %% 2 == 0
    x[ind] = paste0(x[ind], spacing)
    x
  }
  if(sortfDO){
    fDOvalues = fDO(DO = DO,weights = weights)
    index     = order(fDOvalues,decreasing = TRUE)
  }else{index = 1:(dim(DO)[1])}
  if(!is.null(subset)){
    index = index[index %in% subset]
  }else{subset=1:(dim(DO)[1])}
  DO[is.na(DO)] = 0
  DO[DO>cap] = cap
  dim=dim(DO[subset,])
  x = 1:dim[2]
  y = length(subset):1
  xy=expand.grid(x=x,y=y)
  PlotData = as.data.frame(cbind(xy,as.vector(t(DO[index,]))))
  colnames(PlotData) = c("x","y","DO")
  Plot = ggplot()
  Plot = Plot + geom_tile(data=PlotData, aes(x, y,fill=DO))
  Plot = Plot + scale_fill_gradientn(colours=c(low='white',"red",
                                               high="darkred"),limits=c(0, cap),guide=FALSE)
  if(is.null(breaks) & is.null(labels)){
    Plot = Plot + scale_y_continuous(expand=c(0,0),
                                     breaks=1:length(index),
                                     labels = avoid_overlap(rev(index),"    "))
  }else{
    Plot = Plot + scale_y_continuous(expand=c(0,0),
                                     breaks=breaks,
                                     labels = avoid_overlap(rev(labels),"    "))}
  Plot = Plot + scale_x_continuous(expand = c(0, 0))
  Plot = Plot + theme(panel.margin=unit(0,"null"),
                      axis.ticks.length = unit(0,"null"),
                      legend.margin=unit(0,"null"))
  Plot = Plot + theme(axis.text.x =element_blank(),
                      axis.text.y = element_blank())
  Plot = Plot + theme(panel.border = element_rect(colour = 'black',
                                                  fill = 'transparent'))
  Plot = Plot + xlab(XLAB)+ylab(YLAB)
  Plot = Plot + theme_classic(base_size = 12, base_family = "")+
    theme(panel.background = element_rect(fill = NA,
                                          colour = "black", size = 1),
          axis.title.x = element_text(size = 20,vjust=-2),
          axis.title.y = element_text(size = 20,angle=90,vjust=2),
          text = element_text(size=20),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.line = element_line(size = 0))
  return(Plot)
}