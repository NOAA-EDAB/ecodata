#' Marine heatwaves within most recent year
#'
#' Data include daily heatwave anomaly for most recent year.
#'
#' @format Data set contains 3129 rows and 12 columns
#'
#' \itemize{
#'     \item doy: Day of Year.
#'     \item t: Date.
#'     \item temp: Temperature.
#'     \item seas: Climatological Temperature (1982-max year).
#'     \item thresh: Climatological threshold indicator anything above is a heatwave.
#'     \item threshCriterion: If TRUE then temperature is above the 90th percentile of the long term average.
#'     \item durationCriterion: If TRUE the temp has been above threshold for
#'     5 or more days (surface) and 30 or more days (bottom).
#'     \item event: If TRUE indicates heatwave event.
#'     \item event_no: Heatwave number.
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#'     \item Year: Year
#'     \item Var: Variable
#' }
#'
#' @details
#' More information including processing and indicator derivation steps are available at
#' \url{https://noaa-edab.github.io/tech-doc/marine_heatwave.html}.
"heatwave_year"
