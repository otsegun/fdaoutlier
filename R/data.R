#' Spanish Weather Data
#'
#' A dataset containing daily temperature, log precipitation and wind speed of 73 spanish weather stations
#' in Spain between 1980 - 2009.
#' @name spanish_weather
#'
#' @docType data
#'
#' @format A list containing :
#' \describe{
#'   \item{\code{$station_info:}}{A dataframe containing geographic information from the 73 weather stations
#'   with the following variables:
#'   \describe{
#'    \item{\code{ind:}}{id of weather station}
#'    \item{\code{name:}}{name of weather station}
#'    \item{\code{province:}}{province of weather station}
#'    \item{\code{altitude:}}{altitude in meters of the station}
#'    \item{\code{year.ini:}}{start year}
#'    \item{\code{year.end:}}{end year}
#'    \item{\code{longitude:}}{longitude of the coordinates of the weather station (in decimal degrees)}
#'    \item{\code{longitude:}}{latitude of the coordinates of the weather station (in decimal degrees)}
#'    }
#'   }
#'
#'    \item{\code{$temperature:}}{A matrix of size 73 (stations) by 365 (days) containing
#'    average daily temperature for the period 1980-2009 (in degrees Celsius, marked
#'    with UTF-8 string). Leap years temperatures for February 28 and 29 were averaged.}
#'
#'    \item{\code{$wind_speed:}}{A matrix of size 73 (stations) by 365 (days) containing
#'    average daily wind speed for the period 1980-2009 (in m/s).}
#'
#'   \item{\code{$logprec:}}{A matrix of size 73 (stations) by 365 (days) containing average daily log precipitation
#'   for the period 1980-2009 (in log mm). Negligible precipitation (less than 1 tenth of mm)
#'   is replaced by 0.05 and no precipitation (\code{0.0} mm) is replaced by \code{0.01} after which
#'   the logarithm was applied.}
#'  }
#'
#'
#' @details
#' This is a stripped down version of the popular \code{aemet} spanish weather data available in
#' the \code{fda.usc}\href{http://dx.doi.org/10.18637/jss.v051.i04}{<doi:10.18637/jss.v051.i04>} package.
#' See the documentation of \code{fda.usc} for more details about data.
#'
#' @source
#' Data obtained from the \code{fda.usc}\href{http://dx.doi.org/10.18637/jss.v051.i04}{<doi:10.18637/jss.v051.i04>} package.
#'
#' @keywords datasets
#' @examples
#'
#' data(spanish_weather)
#' names(spanish_weather)
#' names(spanish_weather$station_info)
#'
"spanish_weather"


#' Simulated functional data from a simple magnitude model
#'
#'
#' @description A simulated dataset containing 100 functional observation observed on 50 domain points.
#' Data was simulated from a simple magnitude outlier model with main model :
#'\deqn{X(t) = 4t + e(t)}
#'and contamination model:
#'\deqn{X(t) = 4t + 8k + e(t)}
#'where \eqn{t \in [0,1]}, and  \eqn{e(t)} is a Gaussian process with zero mean and covariance function
#' \deqn{\gamma(s,t) = exp{-|t-s|}}
#' and \eqn{k \in [-1, 1]} with \eqn{P(k = -1) = P(k = 1) = 0.5}.
#'
#' There are 90 non-outliers and 10 magnitude outliers.
#'
#' @name sim_data1
#'
#' @docType data
#'
#' @format A list containing 2 elements:
#' \itemize{
#'   \item \code{data:}  A matrix of 100 by 50 containing 100 functional observed on 50 domain points.
#'    \item \code{true_outlier:} An integer vector containing indices which indicate observations in \code{sim_data1$data}
#'    that are magnitude outliers.
#'  }
#'
#' @details
#' Data included for illustration and testing purposes.
#'
#' @source
#' Simulation
#'
#' @author
#' Oluwasegun Ojo
#'
#' @keywords datasets
#' @examples
#' data(sim_data1)
#' str(sim_data1)
#'
"sim_data1"
