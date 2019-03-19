#' Oyster harvest data from the Mid-Atlantic 
#' 
#' These data contain time series of number of oysters sold in Maryland, Virginia, and New Jersey. The data were pulled directly
#' from annual state- and university-sponsored surveys of shellfish farmers.
#' 
#' @format A data set containing 24 rows and 5 columns. 
#' 
#' \itemize{
#'     \item Var: Specifies state from which oyster harvest data were taken.
#'     \item Value: Number of oysters sold.
#'     \item EPU: Ecological Production Unit (EPU) where data originated. Here "MAB" refers to the Mid-Atlantic Bight.
#'     \item Units: Units of variable \code{Var}.
#'     \item Time: Survey year.
#' }
#' 
#' @details More information available at \url{https://noaa-edab.github.io/tech-doc/aquaculture.html}
"aquaculture"
