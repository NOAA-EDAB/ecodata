#' Managed species stock status
#' 
#' Stock status for MAFMC and NEFMC managed species in New England and the Mid-Atlantic. 
#' 
#' @format 98 rows and 7 columns
#' 
#' \itemize{
#'     \item Stock: Stock identifier.
#'     \item Last assessment: Last year that a stock assessment was performed for a given stock.
#'     \item Council: Management body (MAFMC, NEFMC, or Both).
#'     \item Code: Shortened identifier for stocks.
#'     \item Var: Either \code{F.Fmsy}, representing the ratio of fishery mortality to fishery mortality
#'     at maximum sustainable yield (or proxy), or \code{B.Bmsy}, defined as the ratio of estimated stock biomass to 
#'     estimated biomass at maximum sustainable yield (or proxy). 
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
#' }
#' 
#' @details 
#' More information about this data set may be found at \url{https://noaa-edab.github.io/tech-doc/stockstatus.html}.
#' 
"stock_status"
