#' Magniude-Shape Plot (MS-Plot) based on the directional outlyingness for functional data.
#'
#' This functiona creates the MS-Plot, an outlier detection tool for univariate and
#' multivariate functional data proposed in Wenlin and Genton (2019). Indices of observations
#' flagged as outliers are returned, together with the corresponding plot which depends on the
#'  dimension of the data (see details).
#'
#' @param data A matrix for univariate functional data (of size n observations
#'   by p domain points) or a 3-dimensional array for multivariate functional
#'   data (of size n observations by p domain points by d dimension).
#' @param plot A logical scalar. If TRUE, makes the MS-Plot
#' @param plot_type The type of plot to produce if \code{data} is a multivariate functional data.
#' Can be one of "scatter" or "parallel". Default is "Scatter". For univariate functional data, the
#' scatter plot is always produced.
#' @param return_outliers A logical scalar. IF TRUE, returns the indices of the outliers. Furthermore, if
#' \code{plot} is TRUE, differentiates the outlying points of the MS-Plot from those of the non-outlying
#' points.
#' @param data_depth The depth used in the computation of the directional outlyingness if \code{data} is a
#' multivariate functional data. Can be one of "mahalanobis", "random_projections", "simplicial", or "half_space" depth.
#' Default is "random_projections". For univariate functional data, the projection depth is always used.
#'
#' @param normal_col The color of non-outlying points in the MS-Plot. Must be a character vector
#' #' containing one name of a standard color in R or the Hex code of a color.
#'
#' @param outlier_col The color of outlying points in the MS-Plot. Must be a character vector
#' #' containing one name of a standard color in R or the Hex code of a color.
#'
#' @param col The color for plotting points in the MS-Plot when \code{return_outliers} is FALSE and
#' \code{plot} is TRUE.
#'
#' @details
#'
#' This function implements the Magnitude-Shape Plot of Dai and Genton (2018). MS-Plot is a plot of the variation
#' of directional outlyingness (VO) against the mean directional outlyingness (MO). MO and VO are computed based on
#' the directional outlyingness defined in Dai and Genton (2019). For univariate functional data,
#' the projection depth based on Zuo (2003) is always used (as suggested by Dai and Genton (2019)) for
#' computing the directional outlyingess while for multivariate functional data, any of "mahalanobis", "random_projections",
#' "simplicial", or "half_space" depths can be used.
#'
#' For univariate functional data, a 2-d scatter plot is always produced using ggplot2. This plot is one of the objects
#' contained in the list returned by the function. For bivariate functional data, a 3-d scatter plot (made with the scatter3d package)
#' or a parallel plot (made with ggplot2) is produced. For multivariate functional data (with dimensions >= 2), a 2-d scatter plot
#' (of VO against the norm of MO) or a parallel plot (both returned as a ggplot2 object) is produced.
#'
#' @return Returns a list containing:
#'   \item{outliers_index}{an integer vector containing the indices of the outliers if \code{return_outlier} is TRUE.}
#'   \item{plot_object}{a ggplot2 object containing the plot of the MS-Plot if \code{plot} is TRUE, except when \code{plot_type}
#'    is "scatter" and \code{data} is a bivariate functional data. The MS-Plot is made with the \code{scatterplot3d) package,
#'    hence a ggplot2 object is not returned in this case.}
#'   \item{mean_outlyingness}{an n x d matrix of the mean of directional outlyingness.}
#'   \item{var_outlyingness}{a vector of length n containing the variation of directional outlyingness.}
#'   \item{median_curve}{the indices of the median observation if \code{return_outlier} is TRUE. The median observation is the
#'   observation with the smallest mahalanobis distance computed from the matrix whose columns are made up of MO and VO.}
#' @author
#' Version created by Oluwasegun Taiwo Ojo based on the original code written by Wenlin Dai and Marc G. Genton.
#'
#' @references
#' Dai, W., and Genton, M. G. (2018). Multivariate functional data visualization and outlier detection. \emph{Journal of Computational and Graphical Statistics}, 27(4), 923-934.
#'
#' Dai, W., and Genton, M. G. (2019). Directional outlyingness for multivariate functional data. \emph{Computational Statistics & Data Analysis}, 131, 50-65.
#' @seealso
#'
#' @examples
#'
#' @export
#' @import ggplot2
#' @importFrom grDevices rgb
ms_plot <- function(data, plot = F, plot_type = c("scatter", "parallel"),
                    return_outliers = T,
                    data_depth = c("random_projections", "mahalanobis",
                                   "simplicial", "half_space"),
                    normal_col = "blue",
                    outlier_col = "red",  col = "grey") {
  ### pairwise plots of variation of outlyingness (VO) against mean outlyingness (MO)###
  data_dim  <- dim(data)
  data_depth <- match.arg(data_depth)
  if(plot) plot_type <- match.arg(plot_type)

  # univariate
  if (length(data_dim) == 2){
    n <- data_dim[1]
    dir_result <- dir_out(data, data_depth = data_depth)
    if (return_outliers){
      dist <- dir_result$distance
      rocke_factors <- hardin_factor_numeric(n, 2)
      rocke_factor1 <- rocke_factors$factor1
      rocke_cutoff <- rocke_factors$factor2 # C in paper
      cutoff_value <- rocke_cutoff/rocke_factor1 #rocke_cutoff/rocke_factor1
      outliers_index <- which(dist > cutoff_value)
      median_curve <- which.min(dist)

      # mean_dir_out <- dir_result$mean_outlyingness
      # var_dir_out <- dir_result$var_outlyingness

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


        msplot_data <- data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                  var_outlyingness = dir_result$var_outlyingness,
                                  outliers = point_col)

        (p <- ggplot(data = msplot_data, mapping = aes(x = mean_outlyingness, y = var_outlyingness)) +
          geom_point(colour = point_col) +
          geom_path(data = elip_data, mapping = aes(x = x_coords, y = y_coords),
                    show.legend = FALSE, colour = "lightblue") +
          xlab("mean directional outlyingness (MO)") +
          ylab("variational directional outlyingness (VO)") +
          labs(title = "MS-Plot") +
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5)))

        return(list(outliers_index = outliers_index,
                    plot_object = p,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlyingness,
                    median_curve = median_curve))
      }
      else{
        return(list(outliers_index = outliers_index,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlyingness,
                    median_curve = median_curve))
        }


    }
    else{
      if (plot){
        (p <- ggplot(data = data.frame(mean_outlyingness = dir_result$mean_outlyingness,
                                      var_outlyingness = dir_result$var_outlyingness),
                    mapping = aes(x = mean_outlyingness,
                                  y = var_outlyingness)) +
          geom_point(colour = col, show.legend = FALSE) +
          xlab("mean directional outlyingess (MO)") +
          ylab("variational directional outlyingness (VO)") +
          labs(title="MS-Plot") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)))


        return(list(plot_object = p,
                    mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlyingness))
        }
      else{
        return(list(mean_outlyingness = dir_result$mean_outlyingness,
                    var_outlyingness = dir_result$var_outlyingness))
        }
    }
  }

  if (length(data_dim) == 3){
    n <- data_dim[1]
    d <- data_dim[3]

    rocke_factors  <- hardin_factor_numeric(n = n, dimension = d + 1)
    rocke_factor1 <- rocke_factors$factor1
    rocke_cutoff <- rocke_factors$factor2

    if (d == 2){
      dir_result = dir_out(data, data_depth = data_depth)
      #dir_result2 = dir_out(data, data_depth = data_depth)
      #dist <- dir_result$distance

      if (return_outliers){
        cutoff_value  <- rocke_cutoff/rocke_factor1
        outliers_index <- which(dir_result$distance > cutoff_value)
        median_curve <- which.min(dir_result$distance)

        if(plot){
          if (plot_type == "parallel"){
            point_col  <-  rep("normal", n)
            point_col[outliers_index] <- "outlier"
            msplot_data <- data.frame(id = 1:n,
                                      mean_outlyingness = dir_result$mean_outlyingness,
                                      var_outlyingness = dir_result$var_outlyingness,
                                      outlier = point_col)
            (p <- ggplot(data = reshape::melt(msplot_data, id.vars = c("id", "outlier")),
                   mapping = aes(x = variable, y = value, group = id, colour = outlier)) +
              geom_line() + geom_point() +
              labs(title = "MS-Plot with parallel coordinates") +
              theme_bw() +
              scale_color_manual(values = c(outlier_col, normal_col)) +
              theme(plot.title = element_text(hjust = 0.5)))

            return(list(outliers_index = outliers_index,
                        plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          } else if (plot_type == "scatter"){
            mcd_object <- dir_result$mcd_obj
            L <- solve(chol.default(solve(mcd_object$cov)))
            theta <- (0:200)/200
            v  <-  ((0:200)-100)/100
            x_coords <- as.vector((1-v^2)^(1/2)%*%t(sin(theta*2*pi)))
            y_coords <- as.vector((1 - v^2) ^(1/2) %*% t(cos(theta * 2 * pi)))
            z_coords <- as.vector(v %*% t(rep(1,201)))
            circle <- cbind(x_coords, y_coords , z_coords) # replace with array or cbind

            x_1 <- mcd_object$center[1] + (cutoff_value^(1/2) * L %*% t(circle))[1,]
            y_1 <- mcd_object$center[2] + (cutoff_value^(1/2) * L %*% t(circle))[2,]
            z_1 <- mcd_object$center[3] + (cutoff_value^(1/2) * L %*% t(circle))[3,]

            x_lim <- c(min(x_1, dir_result$ms_matrix[,1]), max(x_1, dir_result$ms_matrix[,1]))
            y_lim <- c(min(y_1, dir_result$ms_matrix[,2]), max(y_1, dir_result$ms_matrix[,2]))
            z_lim <- c(min(z_1, dir_result$ms_matrix[,3]), max(z_1, dir_result$ms_matrix[,3]))
            point_col  <-  rep(normal_col, n)
            point_col[outliers_index] <- outlier_col
            point_pch  <- rep(20, n)
            point_pch[outliers_index] <- 19
            s3d <- scatterplot3d::scatterplot3d(x_1, y_1, z_1, xlim = x_lim, ylim = y_lim, zlim = z_lim,
                                 type = "p", pch = 1, mar=c(2.5,2.5,2,2.5),
                                 color = rgb(173/255, 216/255, 230/255, 1),
                                 cex.symbols = 0.1, angle=45,
                                 cex.lab = 0.8, axis = TRUE, box = FALSE, xlab = "mean_outlyingness1",
                                 ylab = "mean_outlyingness2", zlab = "var_outlyingness",
                                 main = "MS-Plot")

            s3d$points3d(dir_result$ms_matrix[,1], dir_result$ms_matrix[,2],
                         dir_result$ms_matrix[,3], col = point_col, pch = point_pch, type = "p")
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
        var_dir_out <- dir_result$var_outlyingness
        x_lim = c(min(mean_dir_out1) - 0.1 * (max(mean_dir_out1) - min(mean_dir_out1)),
               max(mean_dir_out1) + 0.1 * (max(mean_dir_out1) - min(mean_dir_out1)))
        y_lim = c(min(mean_dir_out2) - 0.1 * (max(mean_dir_out2) - min(mean_dir_out2)),
                  max(mean_dir_out2) + 0.1 * (max(mean_dir_out2) - min(mean_dir_out1)))

        if (plot){
          if (plot_type == "parallel"){
            msplot_data <- data.frame(id = 1:n,
                                     mean_outlyingness1 = mean_dir_out1,
                                     mean_outlyingness2 = mean_dir_out2,
                                     var_outlyingness = var_dir_out)
            (p <- ggplot(data = reshape::melt(msplot_data,id.vars = c("id")),
                         mapping = aes(x = variable, y = value, group = id)) +
                geom_line(colour = col) + geom_point(col = col) +
                labs(title = "MS-Plot with parallel coordinates") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5)))

            return(list(plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness))
          } else if (plot_type == "scatter"){
            scatterplot3d::scatterplot3d(mean_dir_out1, mean_dir_out2, var_dir_out,
                          type = "h", pch = 19, color = col, cex.symbols = 1,
                          angle = 45, mar = c(2.5, 2.5, 2, 1.5),
                          cex.lab = 0.8, axis = TRUE, xlim = x_lim, ylim = y_lim, box = FALSE,
                          xlab = "mean_outlyingness1",
                          ylab = "mean_outlyingness2", zlab = "var_outlyingness",
                          main = "MS-Plot")
            return(list(mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness))
          }
        } else{
          return(list(mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness))
        }


      }
    } else if (d >= 3) {
      dir_result = dir_out(data, data_depth = data_depth)
      if (return_outliers){
        cutoff_value  <- rocke_cutoff/rocke_factor1
        outliers_index <- which(dir_result$distance > cutoff_value)
        median_curve <- which.min(dir_result$distance)
        if (plot) {
          if(plot_type == "parallel"){
            point_col  <-  rep("normal", n)
            point_col[outliers_index] <- "outlier"
            msplot_data <- data.frame(id = 1:n,
                                      mean_outlyingness = dir_result$mean_outlyingness,
                                      var_outlyingness = dir_result$var_outlyingness,
                                      outlier = point_col)
            (p <- ggplot(data = reshape::melt(msplot_data,id.vars = c("id", "outlier")),
                         mapping = aes(x = variable, y = value, group = id, colour = outlier)) +
                geom_line() + geom_point() +
                labs(title = "MS-Plot with parallel coordinates") +
                theme_bw() +
                scale_color_manual(values = c(outlier_col, normal_col)) +
                theme(plot.title = element_text(hjust = 0.5)))

            return(list(outliers_index = outliers_index,
                        plot_object = p,
                        mean_outlyingness = dir_result$mean_outlyingness,
                        var_outlyingness = dir_result$var_outlyingness,
                        median_curve = median_curve))
          } else {
            point_col  <-  rep("normal", n)
            point_col[outliers_index] <- "outlier"

            mean_out_norm_sq <-(apply(dir_result$mean_outlyingness^2,1,sum))^(1/2)

            msplot_data <- data.frame(x = mean_out_norm_sq, y = dir_result$var_outlyingness,
                                      outlier = point_col)
            p <- ggplot(data = msplot_data, aes(x = x, y = y, colour = outlier)) +
              geom_point() +
              scale_colour_manual(values = c(normal_col, outlier_col)) + # why
              xlab("norm mean directional outlyingness (||MO||)") +
              ylab("variational directional outlyingness (VO)")+
              labs(title = "MS-Plot") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5))

            return(list(outliers_index = outliers_index,
                        plot_object = p,
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

      } else {
        mean_dir_out <- dir_result$mean_outlyingness
        x_lim  <- c(0, max(mean_dir_out) * 1.05)
        var_dir_out <- dir_result$var_outlyingness
        if (plot) {
          if (plot_type == "parallel"){
            msplot_data <- data.frame(id = 1:n,
                                      mean_outlyingness = mean_dir_out,
                                      var_outlyingness = var_dir_out)
            (p <- ggplot(data = reshape::melt(msplot_data, id.vars = c("id")),
                         mapping = aes(x = variable, y = value, group = id)) +
                geom_line(colour = col) + geom_point(colour = col) +
                labs(title = "MS-Plot with parallel coordinates") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5)))

          } else{


            mean_out_norm_sq <-apply(mean_dir_out^2, 1, sum)^(1/2)
            msplot_data = data.frame(x = mean_out_norm_sq, y = var_dir_out)
            p <- ggplot(data = msplot_data, aes(x = x, y = y)) +
              geom_point(colour = col) +
              xlab("norm mean directional outlyingness (||MO||)") +
              ylab("variational directional outlyingness (VO)") +
              labs(title = "MS-Plot") +
              theme_bw()+
              theme(plot.title = element_text(hjust = 0.5))
          }
          return(list(plot_object = p,
                      mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness))
        } else{
          return(list(mean_outlyingness = dir_result$mean_outlyingness,
                      var_outlyingness = dir_result$var_outlyingness))
        }
      }
    }

  }
}
