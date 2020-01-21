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
      rocke_factors <- facCal_num(n, 2) ## check this function next
      rocke_factor1 <- rocke_factors$factor1
      rocke_cutoff <- rocke_factors$cutoff # C in paper
      #number_outliers <- sum((rocke_factor1 * dist) > rocke_cutoff)
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
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5))

        return(list(outliers_index = outliers_index,
                    plot_object = p,
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
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))


        return(list(plot_object = p,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlingness))
        }
      else{
        return(list(mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlingness))
        }
    }
  }

  if (length(data_dim) == 3){
    n <- data_dim[1]
    d <- data_dim[3]

    rocke_factors  <- facCal_num(n, dim = d + 1)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$cutoff

    if (d == 2){
      dir_result = dir_out(data, data_depth = data_depth)
      dist <- dir_result$distance

      if (return_outliers){
        cutoff_value  <- rocke_cutoff/rocke_factor1
        outliers_index <- which(dist > cutoff_value)
        median_curve <- which.min(dist)

        if(plot){
          mcd_object <- dir_result$mcd_obj
          L <- solve(chol.default(solve(mcd_object$cov)))
          theta <- (0:200)/200
          v  <-  ((0:200)-100)/100
          #circle=abind((1-v^2)^(1/2)*sin(theta*2*pi),(1-v^2)^(1/2)*cos(theta*2*pi),v,along=2)
          x_coords <- as.vector((1-v^2)^(1/2)%*%t(sin(theta*2*pi)))
          y_coords <- as.vector((1 - v^2) ^(1/2) %*% t(cos(theta * 2 * pi)))
          z_coords <- as.vector(v %*% t(rep(1,201)))
          circle <- abind(x_coords, y_coords , z_coords, along=2) # replace with array or cbind

          x_1 <- mcd_object$center[1] + (cutoff_value^(1/2) * L %*% t(circle))[1,]
          y_1 <- mcd_object$center[2] + (cutoff_value^(1/2) * L %*% t(circle))[2,]
          z_1 <- mcd_object$center[3] + (cutoff_value^(1/2) * L %*% t(circle))[3,]

          point_col  <-  rep(normal_col, n)
          point_col[outliers_index] <- outlier_col
          point_pch  <- rep(20, n)
          point_pch[outliers_index] = 19

          x_lim <- c(min(x_1, dir_result$ms_matrix[,1]), max(x_1, dir_result$ms_matrix[,1]))
          y_lim <- c(min(y_1, dir_result$ms_matrix[,2]), max(y_1, dir_result$ms_matrix[,2]))
          z_lim <- c(min(z_1, dir_result$ms_matrix[,3]), max(z_1, dir_result$ms_matrix[,3]))

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

          if (plot.type == "parallel"){
            msplot_data <- data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                      var_outlyingness = dir_result$var_outlyingness,
                                      col = point_col)
            p <- ggparcoord(msplot_data, columns = 1:3, groupColumn = 4, showPoints = TRUE) # replace with ggplot2
            return(list(outliers_index = outliers_index,
                        plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          } else if (plot_type == "scatter"){
            s3d <- scatterplot3d(x_1, y_1, z_1, xlim = x_lim, ylim = y_lim, zlim = z_lim,
                                 type = "p", pch = 1, mar=c(2.5,2.5,2,2.5),
                                 color = rgb(173/255, 216/255, 230/255, 1),
                                 cex.symbols = 0.1, angle=45,
                                 cex.lab = 0.8, axis = TRUE, box = FALSE)
            s3d$points3d(dir_result$ms_matrix[,1], dir_result$ms_matrix[,2],
                         dir_result$ms_matrix[,3], col = point_col, pch = pch, type = "p")
            return(list(outliers_index = outliers_index,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          }

        } else{
          return(list(outliers_index = outliers_index,
                      mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness,
                      median_curve = median_curve))
        }


      }else{
        mean_dir_out1 <-dir_result$mean_outlyingness[, 1]
        mean_dir_out2 <-dir_result$mean_outlyingness[, 2]
        # mo1 <-result$out_avr[,1]
        # mo2 <-result$out_avr[,2]
        x_lim = c(min(mean_dir_out1) - 0.1 * (max(mean_dir_out1) - min(mean_dir_out1)),
               max(mean_dir_out1) + 0.1 * (max(mean_dir_out1) - min(mean_dir_out1)))
        y_lim = c(min(mean_dir_out2) - 0.1 * (max(mean_dir_out2) - min(mean_dir_out2)),
                  max(mean_dir_out2) + 0.1 * (max(mean_dir_out2) - min(mean_dir_out1)))
        var_dir_out <- dir_result$var_outlyingness
        #fo <- mo1^2+mo2^2+vo

        if (plot){
          if (plot_type == "parallel"){
            msplot_data = data.frame(mean_outlyingness1 = mean_dir_out1,
                                     mean_outlyingness2 = mean_dir_out2,
                                     var_outlyingness = var_dir_out,
                                     col = col)
            p <- ggparcoord(ms_plot, columns = 1:3, groupColumn = 4, showPoints = TRUE)
            return(list(plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness))
          } else if (plot_type == "scatter"){
            scatterplot3d(mean_dir_out1, mean_dir_out2, var_dir_out,  # why type = h
                          type = "h", pch = 19, color = col, cex.symbols = 1,
                          angle = 45, mar = c(2.5, 2.5, 2, 1.5),
                          cex.lab = 0.8, axis = TRUE, xlim = x_lim, ylim = y_lim, box = FALSE)
            return(list(mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness))
          }
        } else{
          return(list(mean_outlyingness = dir_result$mean_outlyingness,
                         var_outlyingness = dir_result$var_outlyingness))
        }
        #scatter3D(mo1,mo2,vo,type="h",pch=19,col=col,cex.symbols=1,angle=45,mar=c(2.5,2.5,2,1.5),
        #          cex.lab=0.8,axis=TRUE,xlim=xlim,ylim=ylim,bty="g",colkey=FALSE,...)

      }
    } else if (d >= 3) {
      dir_result = dir_out(data, data_depth = data_depth)
      if (return_outliers){
        cutoff_value  <- rocke_cutoff/rocke_factor1
        outliers_index <- which(dist > cutoff_value)
        median_curve <- which.min(dist)

        # cutoff=cutoff2/fac2
        # mo <-result$out_avr
        # vo <-result$out_var
        # out.dir=which(result$D>cutoff)
        # medcurve=which.min(result$D)
        if (plot) {
          x_lim = c(min(x1, dir_result$ms_natrix[,1]), max(x1, dir_result$ms_natrix[, 1])) # x1 not computed antes! check ! in this section
          y_lim = c(min(y1, dir_result$ms_natrix[,2]), max(y1, dir_result$ms_natrix[, 2]))
          z_lim = c(min(z1, dir_result$ms_natrix[,3]), max(z1, dir_result$ms_natrix[, 3]))

          point_col  <-  rep(normal_col, n)
          point_col[outliers_index] <- outlier_col
          point_pch  <- rep(20, n)
          point_pch[outliers_index] = 19

          if(plot.type == "parallel"){
            msplot_data  <- data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                       var_outlyingness = dir_result$var_outlyingness,
                                       col = point_col)
            p <- ggparcoord(ms_plot, columns = columns,
                            groupColumn = (d + 2), showPoints = TRUE) # columns not defined earlier! check!

            return(list(outliers_index = outliers_index,
                        plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          } else {
            mean_out_norm_sq <-(apply(dir_result$mean_outlyingness^2,1,sum))^(1/2)

            msplot_data <- data.frame(x = mean_out_norm_sq, y = dir_result$var_outlyingness,
                                      col = point_col)
            p <- ggplot(data = msplot_data, aes(x = x, y = y)) +
              geom_point(aes(colour = col), show.legend = FALSE) +
              scale_colour_manual(values = c(2,3)) + # why
              xlab("||MO||") + ylab("VO")+
              labs(title = "MS-Plot") + theme(plot.title = element_text(hjust = 0.5))
            return(list(outliers_index = outliers_index,
                        plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          }

          #plot(MO,vo,xlab="MO",ylab="VO",type="n",pch=pch,col=col,cex=0.1,...)
          #points(MO,vo,pch=19,cex=1,col=col.point,...)
        } else{
          return(list(outliers_index = outliers_index,
                      mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness,
                      median_curve = median_curve))
        }

      } else {
        mean_out_dir <- dir_result$mean_outlyingness
        mean_out_norm_sq <-apply(mean_out_dir^2, 1, sum)^(1/2)
        x_lim  <- c(0, max(mean_out_dir) * 1.05)
        var_out_dir <-result$var_outlyingness
        if (plot) {
          if (plot_type == "parallel"){
            msplot_data  <- data.frame(mean_outlyingness = mean_out_dir,
                                       var_outlyingess =  var_out_dir)
            p <- ggparcoord(msplot_data, columns = 1:(d+1), showPoints = TRUE)
          } else{
            msplot_data = data.frame(x = mean_out_norm_sq, y = var_out_dir, col = col)
            p <- ggplot(data = msplot_data, aes(x = x, y = y)) +
              geom_point(colour = col, show.legend = FALSE) +
              xlab("||MO||") + ylab("VO") + labs(title = "MS-Plot") +
              theme(plot.title = element_text(hjust = 0.5))
          }
          return(list(plot_object = p,
                      mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness))
        } else{
          return(list(mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness))
        }


        #plot(mo,vo,type="p",pch=19,xlim=xlim,col=col,...)
      }
    }

  }
}
