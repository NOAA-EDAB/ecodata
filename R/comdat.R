#' Aggregated commercial fisheries data
#'
#' Aggregated time series of commercial fisheries landings and revenue data from the Northeast Continental Shelf
#' grouped by feeding guild and Ecological Production Unit (EPU). Data are presented for NEFMC and MAFMC managed and unmanaged (i.e.
#' all landings) fisheries, and were derived from the Commercial Fisheries Database Biological Sample (CFDBS).
#'
#' @format These data include 25143 rows and 5 columns.
#'
#' \itemize{
#'     \item Var: Specifies feeding guild and data type (e.g. managed or unmanaged, revenue or landings). Variables including
#'     \code{prop} give the proportion of landings or revenue attributed to a specified feeding guild.
#'     \item Value: Value of variable \code{Var}.
#'     \item EPU: Ecological Production Unit from which data were aggregated.
#'     \item Units: Units of variable \code{Var}.
#'     \item Time: Year.
#' }
#'
#' @details These data are fully documented at \url{https://noaa-edab.github.io/tech-doc/comdat.html}.
"comdat"
