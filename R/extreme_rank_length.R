#' Compute the Extreme Rank Length Depth.
#'
#'This function computes the extreme rank length depth (ERLD) of a sample of curves or functions.
#'Functions have to be discretely observed on common domain points. In principle, the ERLD of a function \eqn{X_i}
#'is the proportion of functions in the sample that is considered to be more extreme
#'than \eqn{X_i}, an idea similar to \code{\link{extremal_depth}}.
#'To determine which functions are more extreme, pointwise ranks of the functions are computed and compared pairwise.
#'
#'
#'@param dts A matrix or data frame of size \eqn{n} observations/curves by \eqn{p} domain/evaluation points.
#'
#'@param type A character value. Can be one of \code{"two_sided"}, \code{"one_sided_left"} or \code{"one_sided_right"}.
#'  If \code{"two_sided"} is specified, small and large values in \code{dts} will be considered extreme. If \code{"one_sided_left"} is specified,
#'  then only small values in \code{dts} are considered to be extreme while for \code{"one_sided_right"}, only large values  in
#'  \code{dts} are considered to be extreme. \code{"two_sided"} is the default. See \code{Details} for more details.
#'
#'@details
#'
#'There are three possibilities in the (pairwise) comparison of the pointwise ranks of the functions.
#'First possibility is to consider only small values as extreme (when \code{type = "one_sided_left"}) in which case the raw pointwise ranks
#'\eqn{r_{ij}} are used. The second possibility is to consider only large values as extreme (when \code{type = "one_sided_right"}) in which
#'case the pointwise ranks used are computed as \eqn{R_{ij} = n + 1 - r_{ij} } where \eqn{r_{ij}} is the raw pointwise rank of the function
#'\eqn{i} at design point \eqn{j} and \eqn{n} is the number of functions in the sample. Third possibility is to consider both small and
#'large values as extreme (when \code{type = "two_sided"}) in which case the pointwise ranks used is computed as
#'\eqn{R_{ij} = min(r_ij, n + 1 - r_{ij})}. In the computation of the raw pointwise ranks \eqn{r_{ij}}, ties are broken using
#'an average. See Dai et al. (2020) \doi{10.1016/j.csda.2020.106960} and Myllymäki et al. (2017) \doi{10.1111/rssb.12172} for more details.
#'
#'@return A numeric vector containing the depth of each curve
#'
#'@author Oluwasegun Ojo
#'
#'@references
#' Dai, W., Mrkvička, T., Sun, Y., & Genton, M. G. (2020). Functional outlier detection and taxonomy by sequential transformations.
#'  \emph{Computational Statistics & Data Analysis}, 106960.
#'
#' Myllymäki, M., Mrkvička, T., Grabarnik, P., Seijo, H., & Hahn, U. (2017).
#'  Global envelope tests for spatial processes. \emph{J. R. Stat. Soc. B}, 79:381-404.
#'
#'@export
#'
#'@examples
#' dt3 <- simulation_model3()
#' erld <- extreme_rank_length(dt3$data)
#'
extreme_rank_length <-
  function(dts,
           type = c("two_sided", "one_sided_left", "one_sided_right")) {
    if (is.data.frame(dts)) {
      dts <- as.matrix(dts)
    }

    if (!is.array(dts) || !is.numeric(dts))
      stop("Argument \"dts\" must be a numeric matrix or dataframe.")

    if (any(!is.finite(dts))) {
      stop("Missing or infinite values are not allowed in argument \"dts\"")
    }

    if (nrow(dts) < 2)
      stop("The number of curves must be greater than 1")

    type <- match.arg(type)
    dm <- dim(dts)
    n <- dm[1]
    p <- dm[2]
    rank_matrix_forward <-
      apply(dts, 2L, rank, ties.method = "average") # low values have low ranks

    if (type == "one_sided_left") {
      #low values considered extreme
      sorted_rank <-
        apply(rank_matrix_forward, 1, sort, method = "quick")
    }else{
      invert_rank <- n + 1 - rank_matrix_forward
      if (type == "two_sided") {
        # both low and high are extreme
        rank_ij <- pmin(rank_matrix_forward, invert_rank)
        sorted_rank <- apply(rank_ij, 1, sort, method = "quick")
      } else if (type == "one_sided_right") {
        sorted_rank <-
          apply(invert_rank, 1, sort, method = "quick")
      } else{
        stop("Argument \'type\' can only be: 'two_sided', 'one_sided_left' or 'one_sided_red'. ")
      }
    }

    depths <- .Call(C_extremeRank, as.double(t(sorted_rank)), n, p,
                    PACKAGE = "fdaoutlier")
    return(depths / n)

  }
