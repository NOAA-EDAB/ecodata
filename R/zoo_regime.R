#' Zooplankton abundance anomaly used in regime shift analysis
#'
#' Estimated zooplankton abundance anomaly on the Northeast US Continental Shelf
#' used in the regime shift analysis.
#'
#' @format 3458 rows and 4 columns.
#'
#' \itemize{
#'     \item Var: species.
#'     \item Value: Value of variable \code{Var}.
#'     \item EPU: Ecological Production Units where data originated.
#'     \item Time: Year.
#' }
#'
#' @details
#' These data were derived from bimonthly Ecosystem Monitoring (EcoMon) cruises throughout the Northeast US Continental Shelf,
#' and represent a different estimation method than what is found in the zooplankton anomaly data set. Specifically, abundance
#' anomalies were interpolated following an ordinary kriging approach by season across the Northeast Shelf Ecosystem. Exact methods
#' used to derived these data are available at \url{https://noaa-edab.github.io/ECSA/}.
#'
"zoo_regime"
