#' Bottom temperature anomaly
#'
#' Seasonally-averaged bottom temperature anomalies for the Northeast
#' Continental Shelf; derived from Hubert's spatial bottom temperature
#' product
#'
#' @format Data set contains 50676 rows and 4 columns
#'
#' \itemize{
#'     \item Latitude
#'     \item Longitude: Longitude (-180 to 180 degrees)
#'     \item Value: Mean Banomaly (degrees C)
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
#'  For more details about how this indictaor is calculated,
#'  use our Technical Documentation. \url{https://noaa-edab.github.io/tech-doc/bottom-temperature---high-resolution.html}
#'
"seasonal_bt_anomaly_gridded"
