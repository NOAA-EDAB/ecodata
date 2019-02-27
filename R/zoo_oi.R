#' Zooplankton abundance (EcoMon seasonal and interpolated)
#' 
#' Estimated seasonal zooplankton abundance on the Northeast US Continental Shelf. Derived from EcoMon survey data through
#' an ordinary kriging process. 
#' 
#' @format 2160 rows and 5 columns.
#' \itemize{
#'     \item Var: Specifies species name and observation season.
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
#' @references 
#' \url{https://noaa-edab.github.io/ECSA/}
"zoo_oi"