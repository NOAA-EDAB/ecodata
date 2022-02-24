#' Marine heatwaves within most recent year
#'
#' Data include daily heatwave anomoly for most recent year.
#'
#' @format Data set contains 999 rows and 11 columns
#'
#' \itemize{
#'     \item doy: Day of Year.
#'     \item t: Date.
#'     \item temp: Temperature.
#'     \item seas: Climatological Temperature (1982-2011).
#'     \item thresh: Climatological threshold indicator anything above is a heatwave.
#'     \item threshCriterion: If TRUE then temperature is above the 90th percentile of the long term average.
#'     \item eurationCriterion: If TRUE the temp has been above threshold for 5 or move days.
#'     \item event: If TRUE indicates heatwave event.
#'     \item event_no: Heatwave number.
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#'     \item Year: Year
#' }
#'
#' @details
#' More information including processing and indicator derivation steps are available at
#' \url{https://noaa-edab.github.io/tech-doc/marine_heatwave.html}.
"heatwave_year"
