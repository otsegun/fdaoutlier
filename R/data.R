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
#' the \code{fda.usc} \doi{10.18637/jss.v051.i04} package.
#' See the documentation of \code{fda.usc} for more details about data.
#'
#' @source
#' Data obtained from the \code{fda.usc} \doi{10.18637/jss.v051.i04} package.
#'
#' @keywords datasets
#' @examples
#'
#' data(spanish_weather)
#' names(spanish_weather)
#' names(spanish_weather$station_info)
#'
"spanish_weather"

#' World Population Data by Countries
#'
#'
#' @description This is the world population data, revision 2010, by countries used in the paper
#' Nagy et al. (2016) \doi{10.1080/10618600.2017.1336445} and Dai et al. (2020)
#' \doi{10.1016/j.csda.2020.106960}. It contains population (both sexes) of countries
#' as of July 1 in the years 1950 - 2010. The data have been pre-processed as described
#' in Nagy et al. (2016) and hence contains only the 105 countries with population
#' in the range of one million and fifteen million on July 1, 1980.
#'
#'
#' @name world_population
#'
#' @docType data
#'
#' @format A matrix of size 105 rows by 61 columns.
#'
#'
#' @details
#' Data included for illustration and testing purposes.
#'
#' @source
#' Data originally available in the \code{depth.fd} package.
#'
#'
#' @keywords datasets
#' @examples
#' data(world_population)
#' str(world_population)
#'
"world_population"
