#' Sea-surface temperature anomaly time serries
#'
#' Data include seasonal time series of sea-surface temperature anomalies on the Northeast Continental Shelf.
#' These data were derived from the NOAA Optimum Interpolation SST High Resolution data set (NOAA OISST V2). The
#' 1982-2010 climatology was used to calculate anomalies.
#'
#' @format 480 observations in 5 columns
#'
#' \itemize{
#'     \item Value: SST anomaly in degrees C.
#'     \item EPU: Ecological Production Unit that data were collected from.
#'     \item Var: Specifies season of anomaly.
#'     \item Time: Year.
#'     \item Units: Units.
#' }
#'
#' @details Seasons are defined as: Fall = October, November, December; Winter = January, February, March; Spring = April,
#' May, June; Summer = July, August, September. Methods used to derive these data are available at \url{https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/get_seasonal_oisst_anom.R}.
#' Contact Kimberly Bastille (kimberly.bastille@noaa.gov) for source data used in the analysis or query your own at
#'  \url{https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html}. For more details about how this indictaor is calculated,
#'  use our Technical Documentation. \url{https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html}.
#'
"seasonal_oisst_anom"
