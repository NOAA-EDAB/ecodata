#' Zooplankton diversity
#'
#' Estimated zooplankton diversity on the Northeast US Continental Shelf.
#'
#' @format 129 rows and 5 columns.
#'
#' \itemize{
#'     \item Var: Shannon diversity.
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
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
"zoo_diversity"
