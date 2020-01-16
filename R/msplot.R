ms_plot <- function(data, plot = F, plot_type = c("scatter", "parallel"),
                    return_outliers = T,
                    data_depth = c("RP", "MhD", "SD", "HS"),
                    normal_col = "blue",
                    outlier_col = "red",  col = "grey") {
  ###pairwise plots of variation of outlyingness (VO) against mean outlyingness (MO)###
  data_dim  <- dim(data)
  data_depth <- match.arg(data_depth)
  if (plot) plot_type <- match.arg(plot_type)

  if (length(data_dim) == 2){
    n <- data_dim[1]
    dir_result <- dir_out(data, data_depth = data_depth)
    if (return_outliers){
      dist <- dir_result$distance
      rocke_factors <- facCal_num(n,2) ## check this function next
      rocke_factor1 <- rocke_factors$factor1
      rocke_cutoff <- rocke_factors$cutoff # C in paper
      number_outliers <- sum((rocke_factor1 * dist) > rocke_cutoff)
      cutoff_value <- rocke_cutoff/rocke_factor1

      # mean_dir_out <- dir_result$mean_outlyingness
      # var_dir_out <- dir_result$var_outlyingness

      outliers_index <- which(dist > cutoff_value)
      median_curve <- which.min(dist)

      if(plot){
        mcd_object <- dir_result$mcd_obj
        L <- solve(chol.default(solve(mcd_object$cov)))
        theta <- (1:200)/199
        circle <- cbind(sin(theta*2*pi), cos(theta*2*pi)) # ch
        x_coords <- mcd_object$center[1] + (cutoff_value^(1/2)*L%*% t(circle))[1,]
        y_coords <- mcd_object$center[2] + (cutoff_value^(1/2)*L%*%t(circle))[2,]
        elip_data <- data.frame(x_coords = x_coords, y_coords  = y_coords)

        point_col <- rep(normal_col, n)
        point_col[outliers_index] <- outlier_col
        pch <- rep("N",n)
        pch[outliers_index] <- "O"

        msplot_data <- data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                  var_outlingness = dir_result$var_outlingness,
                                  outliers = point_col,
                                  pch = pch)

        p <- ggplot(data = msplot_data, mapping = aes(x = mean_outlyingness, y = var_outlyingness)) +
          geom_point(col = point_col)+
          geom_path(data = elip_data , mapping = aes(x = x_coords, y = y_coords),
                    show.legend = FALSE, colour = "lightblue") +
          xlab("mean directional outlyingness") +
          ylab("variational directional outlyingness") +
          labs(title = "MS-Plot") +
          theme_bw()
          theme(plot.title = element_text(hjust = 0.5))

        return(list(outliers_index = outliers_index,
                    pplot_object = p,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlingness,
                    median_curve = median_curve))
      }
      else{
        return(list(outliers_index = outliers_index,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlingness,
                    median_curve = median_curve))
        }


    }
    else{
      #mo <-result$out_avr
      #xlim <- c(min(mo)-0.1*(max(mo)-min(mo)),max(mo)+0.1*(max(mo)-min(mo)))
      #vo <-result$out_var
      #ms.data=data.frame(x=mo,y=vo,col=col)

      if (plot){
        p <- ggplot(data = data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                      var_outlyingness = dir_result$var_outlingness,
                                      colour = col),
                    mapping = aes(x = mean_outlyingness,
                                  y = var_outlyingness)) +
          geom_point(colour = col, show.legend = FALSE) +
          xlab("mean directional outlyingess") +
          ylab("variational directional outlyingness") +
          labs(title="MS-Plot") +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme_bw()

        return(list(plot_object = p,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlingness))
        }
      else
      {return(list(mo=mo,vo=vo))}
    }
  }

  if (length(temp)==3)
  {

    factor=facCal_num(temp[1],dim=temp[3]+1)
    fac2=factor$fac1
    cutoff2=factor$fac2   #cut off value for testing/outlier detection#

    d=temp[3]
    n=temp[1]

    if (d==2)
    {

      result=DirOut(data,depth.dir=depth.dir)
      mo <-result$out_avr
      vo <-result$out_var

      if (dirout)
      {
        cutoff=cutoff2/fac2
        out.dir=which(result$D>cutoff)
        medcurve=which.min(result$D)


        if (plot)

        {

          M=cbind(mo,vo)
          ans=cov.rob(M,method="mcd")
          L=solve(chol.default(solve(ans$cov)))
          theta=0:200/200
          v=(0:200-100)/100
          #circle=abind((1-v^2)^(1/2)*sin(theta*2*pi),(1-v^2)^(1/2)*cos(theta*2*pi),v,along=2)
          x=as.vector((1-v^2)^(1/2)%*%t(sin(theta*2*pi)))
          y=as.vector((1-v^2)^(1/2)%*%t(cos(theta*2*pi)))
          z=as.vector(v%*%t(rep(1,201)))
          circle=abind(x,y,z,along=2)

          x1=ans$center[1]+(cutoff^(1/2)*L%*%t(circle))[1,]
          y1=ans$center[2]+(cutoff^(1/2)*L%*%t(circle))[2,]
          z1=ans$center[3]+(cutoff^(1/2)*L%*%t(circle))[3,]

          col.point=rep(col.normal,n)
          col.point[out.dir]=col.out
          pch=rep(20,n)
          pch[out.dir]=19

          xlim=c(min(x1,M[,1]),max(x1,M[,1]))
          ylim=c(min(y1,M[,2]),max(y1,M[,2]))
          zlim=c(min(z1,M[,3]),max(z1,M[,3]))

          #scatter3D(x1,y1,z1,xlim=xlim,ylim=ylim,zlim=zlim,type="p",pch=1,mar=c(2.5,2.5,2,1.5),
          #          col=rgb(173/255,216/255,230/255,1),cex.symbols=0.1,angle=45,
          #          cex.lab=0.6,axis=TRUE,bty="g",colkey=FALSE,...)
          #scatter3D(M[,1],M[,2],M[,3],color=col.point,pch=pch,type="p",add=TRUE,colkey=FALSE)


          #plot3d(c(x1,M[,1]),c(y1,M[,2]),c(z1,M[,3]),col=c(rep("lightblue",length(x1)),col.point),
          #pch=c(rep(1,length(x1)),pch))
          #plot3d(c(M[,1]),c(M[,2]),c(M[,3]),col=col.point,pch=pch)
          #ellips<- ellipse3d(ans$cov,centre=ans$center,t=cutoff^(1/2))
          #shade3d(ellips, col = "lightblue", alpha = 0.3, lit = FALSE)
          #wire3d(ellips, col = "lightblue",  lit = FALSE)
          #play3d(spin3d(axis = c(0, 0, 1)), duration =10,dir = getwd())
          #movie3d(spin3d(axis = c(0, 0, 1)), duration =5,dir = getwd())

          if (plot.type=="parallel")
          {
            data.ms=data.frame(MO=mo,VO=vo,col=col.point)
            p<-ggparcoord(data.ms,columns=1:3,groupColumn =4,showPoints = TRUE)
            return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve,p=p))
          }

          if (plot.type=="scatter")
          {
            s3d <- scatterplot3d(x1,y1,z1,xlim=xlim,ylim=ylim,zlim=zlim,type="p",pch=1,mar=c(2.5,2.5,2,2.5),
                                 color=rgb(173/255,216/255,230/255,1),cex.symbols=0.1,angle=45,
                                 cex.lab=0.8,axis=TRUE,box=FALSE,...)
            s3d$points3d(M[,1],M[,2],M[,3],col=col.point,pch=pch,type="p")
            return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve))
          }

        }
        return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve))

      }
      if (!dirout)
      {
        mo1 <-result$out_avr[,1]
        mo2 <-result$out_avr[,2]
        xlim=c(min(mo1)-0.1*(max(mo1)-min(mo1)),max(mo1)+0.1*(max(mo1)-min(mo1)))
        ylim=c(min(mo2)-0.1*(max(mo2)-min(mo2)),max(mo2)+0.1*(max(mo2)-min(mo2)))
        vo <-result$out_var
        fo <-mo1^2+mo2^2+vo

        if (plot==TRUE)
        {
          if (plot.type=="parallel")
          {
            data.ms=data.frame(MO1=mo1,MO2=mo2,VO=vo,col=col)
            p<-ggparcoord(data.ms,columns=1:3,groupColumn = 4,showPoints=TRUE)
            return(r1=list(mo=mo,vo=vo,p=p))
          }

          if (plot.type=="scatter")
          {
            scatterplot3d(mo1,mo2,vo,type="h",pch=19,color=col,cex.symbols=1,angle=45,mar=c(2.5,2.5,2,1.5),
                          cex.lab=0.8,axis=TRUE,xlim=xlim,ylim=ylim,box=FALSE,...)
            return(r1=list(mo=mo,vo=vo))
          }
        }
        #scatter3D(mo1,mo2,vo,type="h",pch=19,col=col,cex.symbols=1,angle=45,mar=c(2.5,2.5,2,1.5),
        #          cex.lab=0.8,axis=TRUE,xlim=xlim,ylim=ylim,bty="g",colkey=FALSE,...)
        return(r1=list(mo=mo,vo=vo))
      }
    }

    if (d>=3)
    {
      result=DirOut(data,depth.dir=depth.dir)

      if (dirout)
      {
        cutoff=cutoff2/fac2
        mo <-result$out_avr
        vo <-result$out_var
        out.dir=which(result$D>cutoff)
        medcurve=which.min(result$D)


        if (plot)
        {
          M=cbind(mo,vo)

          xlim=c(min(x1,M[,1]),max(x1,M[,1]))
          ylim=c(min(y1,M[,2]),max(y1,M[,2]))
          zlim=c(min(z1,M[,3]),max(z1,M[,3]))

          col.point=rep(col.normal,n)
          col.point[out.dir]=col.out
          pch=rep(20,n)
          pch[out.dir]=19

          if (plot.type=="parallel")
          {
            data.ms=data.frame(MO=mo,VO=vo,col=col.point)
            p<-ggparcoord(data.ms,columns=columns,groupColumn =(d+2),showPoints = TRUE)
            return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve,p=p))

          }

          if (plot.type=="scatter")
          {
            MO<-(apply(mo^2,1,sum))^(1/2)

            ms.data=data.frame(x=MO,y=vo,col=col.point)
            p<-ggplot(data=ms.data,aes(x,y))+geom_point(aes(colour=col),show.legend=FALSE)+
              scale_colour_manual(values=c(2,3))+
              xlab("||MO||")+ylab("VO")+labs(title="MS-Plot")+theme(plot.title = element_text(hjust = 0.5))
            return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve,p=p))
          }

          #plot(MO,vo,xlab="MO",ylab="VO",type="n",pch=pch,col=col,cex=0.1,...)
          #points(MO,vo,pch=19,cex=1,col=col.point,...)
        }
        return(r1=list(mo=mo,vo=vo,out.dir=out.dir,medcurve=medcurve))
      }
      if (!dirout)
      {
        mo<-result$out_avr
        MO <-apply(mo^2,1,sum)^(1/2)
        xlim=c(0,max(mo)*1.05)
        vo <-result$out_var

        if (plot==TRUE)
        {
          if (plot.type=="parallel")
          {
            data.ms=data.frame(MO=mo,VO=vo)
            p<-ggparcoord(data.ms,columns=1:(d+1),showPoints = TRUE)
          }

          if (plot.type=="scatter")
          {
            ms.data=data.frame(x=MO,y=vo,col=col)
            p<-ggplot(data=ms.data,aes(x,y))+geom_point(colour=col,show.legend=FALSE)+
              xlab("||MO||")+ylab("VO")+labs(title="MS-Plot")+theme(plot.title = element_text(hjust = 0.5))
          }

          return(list(mo=mo,vo=vo,p=p))
        }
        return(list(mo=mo,vo=vo))

        #plot(mo,vo,type="p",pch=19,xlim=xlim,col=col,...)
      }
    }

  }
}
