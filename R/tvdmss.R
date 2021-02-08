#' @describeIn tvdmss Deprecated function. Use \code{tvdmss} instead.
#' @export
tvd_mss <- function(data,
                    emp_factor_mss = 1.5,
                    emp_factor_tvd = 1.5,
                    central_region_tvd = 0.5){

  .Deprecated("tvdmss")
  tvd_obj <- tvdmss(dts = data, emp_factor_mss, emp_factor_tvd, central_region_tvd)
  return(tvd_obj)



}





