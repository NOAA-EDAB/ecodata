#' North Atlantic Oscillation
#' 
#' North Atlantic Oscillation data were taken from the NOAA NWS Climate Prediction Center.
#' These data show the monthly NAO index time series beginning in 1950 and ending in November 2018.
#' 
#' @format Data include 827 rows and 6 columns
#' 
#' \itemize{
#'     \item Var: Variable name.
#'     \item Value: Value of variable name \code{Var}.
#'     \item Year: Year.
#'     \item Month: Month.
#'     \item Units: Units of variable \code{Var}.
#'     \item EPU: Ecological Production Unit (EPU).
#'     
#' }
#' 
#' @details The index is standardized by the standard deviation of the 1950-2000 reference period. More
#' information regarding the methodology involved in deriving the NAO and its significance is
#' available at \url{http://www.cpc.ncep.noaa.gov/data/teledoc/nao.shtml}.
#' 
"nao"