#' Bennet indicator
#'
#' Data contain volume and price indicators for commercial fisheries on the Northeast Continental Shelf,
#' along with their sum totals for each year and species groupings (i.e. the Bennet indicator). This
#' metric allows for examining the drivers of revenue change by Ecological Production Unit (EPU).
#'
#' @format A data set containing 2510 rows and 6 columns.
#'
#' \itemize{
#'     \item Var: Specifies indicator type (price, volume, or total) and associated species grouping.
#'     \item Value: Value of variable \code{Var}.
#'     \item Time: Year.
#'     \item Units: Units of variable \code{Var}.
#'     \item EPU: Ecological Production Unit (EPU) from which data were drawn.
#'     \item Source: Source data from comland (Commercial Landings).
#' }
#'
#' @details These data are documented in full at \url{https://noaa-edab.github.io/tech-doc/bennet-indicator.html}.
#'
"bennet"
