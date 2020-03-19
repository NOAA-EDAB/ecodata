#' Sea-surface temperature anomaly
#'
#' Seasonally-averaged smoothed sea-surface temperature anomalies for the Northeast
#' Continental Shelf; derived from the NOAA optimum interpolation SST
#' high resolution data set (NOAA OISST V2). The 1982-2010
#' climatology was used to calculate anomalies.
#'
#' @format Data set contains 8528 rows and 4 columns
#'
#' \itemize{
#'     \item Latitude
#'     \item Longitude: Longitude (-180 to 180 degrees)
#'     \item Value: Mean SST anomaly (degrees C)
#'     \item Season: Season
#'
#' }
#'
#' @details  Seasons are defined as: Fall = October, November,
#' December; Winter = January, February, March; Spring = April, May, June;
#' Summer = July, August, September. The CRS used to manipulate these data was
#' "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83
#'  +no_defs +ellps=GRS80 +towgs84=0,0,0"
#'
#' @source Source data for this indicator are available at
#' \url{https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html}
"seasonal_sst_anom_gridded"
