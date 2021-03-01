#' Find and classify outliers functional outliers using Sequential Transformation
#'
#' This method finds and classify outliers using sequential transformations
#' proposed in Algorithm 1 of Dai et al. (2020) \doi{10.1016/j.csda.2020.106960}.
#' A sequence of transformations are applied to the functional data and after each
#' transformation, a functional boxplot is applied on the transformed data and outliers
#' flagged by the functional data are noted. A number of transformations mentioned in
#' Dai et al. (2020) \doi{10.1016/j.csda.2020.106960} are supported including vertical
#' alignment ("T1(X)(t)"), normalization ("T2(X)(t)"), one order of differencing
#' ("D1(X)(t)" and "D2(X)(t)") and point-wise outlyingness data ("O(X)(t)").
#' The feature alignment transformation based on warping/curve registration is not yet
#' supported.
#'
#'
#' @param dts A matrix for univariate functional data (of size \code{n} observations by \code{p} domain
#'  points) or a 3-dimensional array for multivariate functional data (of size \code{n}
#'  observations by \code{p} domain points by \code{d} dimension). Only the outlyingness transformation ("O(X)(t)")
#'  supports multivariate functional data so the sequence of transformation must always start with outlyingness ("O(X)(t)")
#'  whenever a multivariate functional data is parsed to \code{dts}.
#'
#' @param sequence A character vector usually of length between 1 and 6 containing any of the strings: \code{"T0", "D0", "T1", "T2",
#' "D1", "D2"} and \code{"O"} (in any order). These sequence of strings specifies the sequence of transformations to be applied
#'  on the data and their meanings are described as follows:
#'  \describe{
#'   \item{\code{"T0"} and \code{"D0"}}{Functional boxplot applied on raw data (no transformation is applied).}
#'    \item{\code{"T1"}}{Apply vertical alignment on data, i.e. subtract from each curve its expectation over the domain of evaluation.}
#'    \item{\code{"T2"}}{Apply normalization on data, i.e. divide each curve by its L-2 norm.}
#'    \item{\code{"D1"} and \code{"D2" }}{Apply one order of differencing on data.}
#'    \item{\code{"O"}}{Find the pointwise outlyingness of data. For multivariate functional data, this transformation replaces the multivariate
#'    functional data with a univariate functional data of pointwise outlyingness.}
#'  }
#'  Examples of sequences of transformations include: \code{"T0"},  \code{c("T0", "T1", "D1")}, \code{c("T0", "T1", "T2")},
#'   \code{c("T0", "D1", "D2")} and \code{c("T0", "T1", "T2", "D1", "D2")}. See Details for their meaning.
#'
#' @param depth_method A character value specifying depth/outlyingness method to use in the functional boxplot applied after each stage of transformation.
#' Note that the same depth/outlyingness method is used in the functional boxplot applied after each transformation in the sequence. The following methods
#' are currently supported:
#' \describe{
#'   \item{"mbd":}{The modified band depth with bands defined by 2 functions.
#'   Uses the algorithm of Sun et al. (2012).}
#'   \item{"tvd"}{The total variation depth of Huang and Sun (2019).}
#'   \item{"extremal"}{The extremal depth of Narisetty and Nair (2016).}
#'   \item{"dirout"}{Uses the robust distance of the mean and variation of directional outlyingness (\code{\link{dir_out}})
#'   defined in Dai and Genton (2018). Since this method is a measure of outlyingness of a function the negative of the
#'   computed robust distance is used in ordering the functions.}
#'   \item{"linfinity"}{The L-infinity depth defined in Long and Huang (2015) is used in ordering functions.}
#'   \item{"bd"}{Uses the band depth with bands defined by 2 functions according to the algorithm of Sun et al. (2012)}
#'   \item{erld}{Uses the extreme rank length depth defined in Myllymäki et al. (2017) and mentioned in Dai et al. (2020).}
#'   \item{"dq"}{Uses the directional quantile (DQ) defined in Myllymäki et al. (2017) and mentioned in Dai et al. (2020).
#'    Since DQ is a measure of outlyingness, the negative of the DQ values is used in ordering the functions.}
#'    }
#'
#' @param save_data A logical. If TRUE, the intermediate transformed data are returned in a list.
#' @param emp_factor The empirical factor for functional boxplot. Defaults to 1.5.
#' @param central_region A value between 0 and 1 indicating the central region probability for functional_boxplot. Defaults to 0.5.
#' @param erld_type If \code{depth_method = "erld"}, the type of ordering to use in computing the extreme rank length depth (ERLD).
#' Can be one of \code{"two_sided"}, \code{"one_sided_left"} or \code{"one_sided_right"}. A \code{"two_sided"} ordering is used by
#' default if \code{erld_type} is not specified and \code{depth_method = "erld"}. The \code{"one_sided_right"} ERLD is especially useful for
#' ordering functions of outlyingness (the output of the \code{"O"} transformation) since it considers only large values as extreme.
#' See \link{extreme_rank_length} for details.
#' @param dq_quantiles If \code{depth_method = "dq"}, a numeric vector of length 2 specifying the probabilities
#'  of upper and lower quantiles. Defaults to \code{c(0.025, 0.975)} for the upper and lower 2.5\% quantiles.
#'  See \link{directional_quantile} for details.
#' @param n_projections An integer indicating the number of random projections to use in computing the point-wise outlyingness if a 3-d array
#'  is specified in \code{dts} i.e. (multivariate functional data), and the transformation \code{"O"} is part of the sequence of transformations
#'  parsed to \code{sequence}. Defaults to 200L.
#' @param seed The random seed to set when generating the random directions in the computation of the point-wise outlyingness. Defaults to NULL.
#' in which case a seed is not set.
#' @return A list containing two lists are returned. The contents of the returned list are:
#' \item{outliers:}{A named list of length \code{length(sequence)} containing the index of outliers found after each
#' transformation. The names of the elements of this list are the sequence strings supplied to \code{sequence} and the
#'  outliers found after each stage of transformation are not necessarily mutually exclusive. }
#'\item{transformed_data}{If \code{save_data = TRUE} a named list of length \code{length(sequence)} containing the transformed matrix after each
#' transformation. The names of the elements of this list are the sequence strings supplied to \code{sequence}. \code{NULL} otherwise (if
#'  \code{save_data = FALSE}).}
#'
#' @details
#'This function implements outlier detection using sequential transformations
#'described in Algorithm 1 of Dai et al. (2020) \doi{10.1016/j.csda.2020.106960}.
#'A sequence of transformations are applied consecutively with the functional
#'boxplot applied on the transformed data after each transformation. The following
#'example sequences (and their meaning) suggested in Dai et al. (2020)
#'\doi{10.1016/j.csda.2020.106960} can be parsed to argument \code{sequence}.
#'
#'\describe{
#'   \item{\code{"T0"}}{Apply functional boxplot on raw data (no transformation is applied).}
#'    \item{\code{c("T0", "T1", "D1")}}{Apply functional boxplot on raw data, then apply vertical alignment on data followed by applying
#'    functional boxplot again. Finally apply one order of differencing on the vertically aligned data and apply functional boxplot again.}
#'    \item{\code{c("T0", "T1", "T2")}}{Apply functional boxplot on raw data, then apply vertical alignment on data followed by applying
#'    functional boxplot again. Finally apply normalization using L-2 norm on the vertically aligned data and apply functional boxplot again.}
#'    \item{\code{c("T0", "D1", "D2")}}{Apply functional boxplot on raw data, then apply one order of difference on data followed by applying
#'    functional boxplot again. Finally apply another one order of differencing on the differenced data and apply functional boxplot again.
#'    Note that this sequence of transformation can also be (alternatively) specified by \code{c("T0", "D1", "D1")}, \code{c("T0", "D2", "D2")}, and
#'     \code{c("T0", "D2", "D1")} since \code{"D1"} and \code{"D2" } do the same thing which is to apply one order lag-1 difference on the data.}
#'    \item{\code{"O"}}{Find the pointwise outlyingness of the multivariate or univariate functional data and then apply functional boxplot
#'    on the resulting univariate functional data of pointwise outlyingness. Care must be taken to specify a one sided ordering function (i.e.
#'    "one_sided_right" extreme rank length depth) in the functional boxplot used on the data of point-wise outlyingness. This is because only
#'    large values should be considered extreme in the data of the point-wise outlyingness.}
#'  }
#' For multivariate functional data (when a 3-d array is supplied to \code{dts}), the sequence of transformation must always begin with \code{"O"}
#' so that the multivariate data can be replaced with the univariate data of point-wise outlyingness which the functional boxplot can subsequently process
#' because the \code{\link{functional_boxplot}} function only supports univariate functional data.
#'
#' If repeated transformations are used in the sequence (e.g. when \code{sequence = c("T0", "D1", "D1")}), a warning message is thrown
#' and the labels of the output list are changed (e.g. for \code{sequence = c("T0", "D1", "D1")}, the labels of the output lists
#' become \code{"T0", "D1_1", "D1_2"}, so that outliers are accessed with \code{output$outlier$D1_1} and \code{output$outlier$D1_2}).
#' See examples for more.
#'
#' @export
#'
#' @examples
#' # same as running a functional boxplot
#' dt1 <- simulation_model1()
#' seqobj <- seq_transform(dt1$data, sequence = "T0", depth_method = "mbd")
#' seqobj$outliers$T0
#' functional_boxplot(dt1$data, depth_method = "mbd")$outliers
#'
#' # more sequences
#' dt4 <- simulation_model4()
#' seqobj <- seq_transform(dt4$data, sequence = c("T0", "D1", "D2"), depth_method = "mbd")
#' seqobj$outliers$T0 # outliers found in raw data
#' seqobj$outliers$D1 # outliers found after differencing data the first time
#' seqobj$outliers$D2 # outliers found after differencing the data the second time
#'
#' # saving transformed data
#' seqobj <- seq_transform(dt4$data, sequence = c("T0", "D1", "D2"),
#'  depth_method = "mbd", save_data = TRUE)
#' seqobj$outliers$T0 # outliers found in raw data
#' head(seqobj$transformed_data$T0)  # the raw data
#' head(seqobj$transformed_data$D1) # the first order differenced data
#' head(seqobj$transformed_data$D2) # the 2nd order differenced data
#'
#' # double transforms e.g. c("T0", "D1", "D1")
#' seqobj <- seq_transform(dt4$data, sequence = c("T0", "D1", "D1"),
#'  depth_method = "mbd", save_data = TRUE) # throws warning
#' seqobj$outliers$T0 # outliers found in raw data
#' seqobj$outliers$D1_1 #found after differencing data the first time
#' seqobj$outliers$D1_2 #found after differencing data the second time
#' head(seqobj$transformed_data$T0)  # the raw data
#' head(seqobj$transformed_data$D1_1) # the first order differenced data
#' head(seqobj$transformed_data$D1_2) # the 2nd order differenced data
#'
#' # multivariate data
#' dtm <- array(0, dim = c(dim(dt1$data), 2))
#' dtm[,,1] <- dt1$data
#' dtm[,,2] <- dt1$data
#' seqobj <- seq_transform(dtm, sequence = "O", depth_method = "erld",
#'  erld_type = "one_sided_right", save_data = TRUE)
#' seqobj$outliers$O # multivariate outliers
#' head(seqobj$transformed_data$O) # univariate outlyingness data
#'

seq_transform <- function(dts, sequence = c("T0", "T1", "T2"),
                          depth_method = c("mbd", "tvd", "extremal", "dirout",
                                    "linfinity", "bd", "erld", "dq"),
                          save_data = FALSE,
                          emp_factor = 1.5,
                          central_region = 0.5,
                          erld_type = NULL,
                          dq_quantiles = NULL,
                          n_projections = 200L,
                          seed = NULL){

  outliers <- list()
  if(save_data){
    transformed_data <- list()
  }


  for (i in seq_along(sequence)) {
    transformation = sequence[i]
    if (transformation == "T0" || transformation == "D0"){
      ## apply functional boxplot here
      t0_outliers <- functional_boxplot(dts, depth_method = depth_method,
                                        central_region = central_region,
                                        emp_factor = emp_factor,
                                        erld_type = erld_type,
                                        dq_quantiles = dq_quantiles)$outliers
      outliers[[i]] <- t0_outliers
      if(save_data) transformed_data[[i]] <- dts
    }else if(transformation == "T1"){
      dts <- center_curves(dts)
      t1_outliers <- functional_boxplot(dts, depth_method = depth_method,
                                        emp_factor = emp_factor,
                                        central_region = central_region,
                                        erld_type = erld_type,
                                        dq_quantiles = dq_quantiles)$outliers
      outliers[[i]] <- t1_outliers
      if(save_data) transformed_data[[i]] <- dts
    } else if(transformation == "T2"){
      dts <- normalize_curves(dts)
      t2_outliers <- functional_boxplot(dts, depth_method = depth_method,
                                        emp_factor = emp_factor,
                                        central_region = central_region,
                                        erld_type = erld_type,
                                        dq_quantiles = dq_quantiles)$outliers
      outliers[[i]] <- t2_outliers
      if(save_data) transformed_data[[i]] <- dts
    } else if(transformation == "D1"|| transformation == "D2"){
      dts <- difference_curves(dts)
      d1_outliers <- functional_boxplot(dts, depth_method = depth_method,
                                        emp_factor = emp_factor,
                                        central_region = central_region,
                                        erld_type = erld_type,
                                        dq_quantiles = dq_quantiles)$outliers
      outliers[[i]] <- d1_outliers
      if(save_data) transformed_data[[i]] <- dts
    } else if(transformation == "O"){
      # implement directional quantile!
      dts <- outlyingness_curves(dts,  n_projections = n_projections, seed = seed)
      o_outliers <- functional_boxplot(dts, depth_method = depth_method,
                                       emp_factor = emp_factor,
                                       central_region = central_region,
                                       erld_type = erld_type,
                                       dq_quantiles = dq_quantiles)$outliers
      outliers[[i]] <- o_outliers
      if(save_data) transformed_data[[i]] <- dts

    }else {
      stop("Transformation ", transformation, ' not supported. \n')
    } #else if(transformation == "R"){
      #dt <- warp_transform(dt)
      #r_outliers <- functional_boxplot(dt, depth_measure = "mbd")
      #outliers[[transformation]] <- r_outliers
      #if(save_data) transformed_data[[transformation]] <- dt
    #}

  }

  # fix duplicated transforms
  if(any(duplicated(sequence))){
    tt <- sequence[duplicated(sequence)][1] # which transform is duplicated
    sequence[sequence == tt] <- paste0(tt, "_",
                                       1:length(sequence[sequence == tt]))
    warning("Duplicated transforms found in argument \'sequence\',
            changing output labels to: ", paste(sequence, collapse = " "), ".")
  }

  names(outliers) <- sequence
  if(save_data) names(transformed_data) <- sequence

  if(save_data){
    return(list(outliers = outliers,
                transformed_data = transformed_data))
    } else{
    return(list(outliers = outliers,
                transformed_data = NULL))
  }

}


center_curves <- function(dt){
  return(dt - rowMeans(dt))
}

normalize_curves <- function(dt){
  return(dt/sqrt(rowSums(dt^2)))
}

difference_curves <- function(dt){
  p <- dim(dt)[2]
  dt[,2:p] - dt[, 1:(p-1)]
}

outlyingness_curves <- function(dt, n_projections = 500L, seed = NULL){
  dm <- dim(dt)
  repnp <- rep(dm[1], dm[2])
  if(length(dm) == 2){# data is univariate
    median_vec <- apply(dt, 2, median)
    mad_vec <- apply(dt, 2, mad)
    return(abs(dt- rep(median_vec, repnp))/rep(mad_vec, repnp))
  } else if (length(dm) == 3) { # data is multivariate
    outlyingness <- apply(dt, 2, function(x){
      (1/projection_depth(x, n_projections = n_projections, seed = seed)) - 1
    })
    return(outlyingness)
  } else{
    stop("Argument \"dt\" must be a 2 or 3 dimensional array.")
  }

}


# warp_curves <- function(dt){
#
# }

