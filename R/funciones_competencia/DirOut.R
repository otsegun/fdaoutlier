
DirOut=function(data,DirOutmatrix=FALSE,depth.dir="RP",D_value=TRUE,quantile.used=floor((n + p + 1)/2),...)
{
  temp=dim(data)
  #####################
  #Univariate cases   #
  #####################
  
  if (length(temp)==2)
  {
    
    data=t(data)
    p=dim(data)[1]
    n=dim(data)[2]
    Dirout=matrix(0,n,p)
    dmat=matrix(0,p,n)
    if ((depth.dir=="MhD")||(depth.dir=="RP"))
    {
      medvec=apply(data,1,median)
      madvec=apply(data,1,mad)
      outmat=abs((data-medvec)/(madvec))
      signmat=sign((data-medvec))
      Dirout=t(outmat*signmat)
    }
    if ((depth.dir=="SD")||(depth.dir=="HS"))
    {
      medvec=apply(data,1,median)
      madvec=apply(data,1,mad)
      outmat=abs((data-medvec)/(madvec))
      signmat=sign((data-medvec))
      Dirout=t(outmat*signmat)
    }
    out_avr=apply(Dirout,1,FUN=function(y) mean(y,na.rm=TRUE))
    out_var=apply(Dirout,1,FUN=function(y) var(y,na.rm=TRUE))
    if (D_value)
    {
      M=cbind(out_avr,out_var)
      ans=cov.rob(M,method="mcd",nsamp="best",...)
      cov=ans$cov
      me=ans$center
      D=mahalanobis(M,me,cov)
    }

  }
  
  #####################
  #Multivariate cases #
  #####################
  
  
  if (length(temp)==3)
  {
    n=temp[1]
    p=temp[2]
    d=temp[3]
    Dirout=array(0,dim=c(n,p,d))
    for (j in 1:p)
    {
      
      
      if (depth.dir=="RP")
      {
        out=1/mdepth.RP(data[,j,],proj=200)$dep-1
      }
      
      if (depth.dir=="MhD")
      {
        out=1/mdepth.MhD(data[,j,])$dep-1
      }
      
      if (depth.dir=="SD")
      {
        out=1/mdepth.SD(data[,j,])$dep-1
      }

      if (depth.dir=="HS")
      {
        out=1/mdepth.HS(data[,j,])$dep-1
      }
      
      me=data[order(out)[1],j,]
      
      for (i in 1:n)
      {
        if ((sum((data[i,j,]-me)^2))^(1/2)==0)
        {
          Dirout[i,j,]=rep(0,d)
        }
        else{
          dir=(data[i,j,]-me)/(sum((data[i,j,]-me)^2))^(1/2)
          Dirout[i,j,]=dir*out[i]
        }
      }
    }
    out_avr=apply(Dirout,c(1,3),FUN=function(y) mean(y,na.rm=TRUE))
    out_var=apply(Dirout^2,1,FUN=function(y) mean(y,na.rm=TRUE))*d-apply(out_avr^2,1,FUN=function(y) sum(y,na.rm=TRUE))
    if (D_value)
    {
      M=cbind(out_avr,out_var)
      ans=cov.rob(M,method="mcd",nsamp="best",...)
      cov=ans$cov
      me=ans$center
      D=mahalanobis(M,me,cov)  
    }
  }
  
  if (D_value)
  {
    if (DirOutmatrix)
      return(list(D=D,Dirout=Dirout,out_avr=out_avr,out_var=out_var,center=me))
    else 
      return(list(D=D,out_avr=out_avr,out_var=out_var,center=me))
  }
  if (!D_value)
  {
    if (DirOutmatrix)
      return(list(Dirout=Dirout,out_avr=out_avr,out_var=out_var))
    else 
      return(list(out_avr=out_avr,out_var=out_var))
  }

}
