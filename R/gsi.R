#' Gulf Stream Index
#' 
#' Annual time series of the Gulf Stream Index from 1954 to 2017. Positive
#' values are a more northerly Gulf Stream, and negative values are a more southerly
#' Gulf Stream. 
#' 
#' @format 
#' 
#' \itemize{
#'     \item Var: Variable of interest.
#'     \item Units: Units of variable \code{Var}.
#'     \item Value: Value of variable \code{Var}.
#'     \item EPU: Representative Ecological Production Unit (MAB = Mid-Atlantic Bight, GB = Georges Bank,
#'     GOM = Gulf of Maine, SS = Scotian Shelf, or All). 
#'     \item Time: Years.
#' }
#' 
#' @details The GSI is the degrees latitude above the average Gulf Stream position based on ocean temperature
#' at 200 m (15 C) depth between 55°W to 75°W. Ocean temperatures at 200 m are available at \url{https://www.nodc.noaa.gov/OC5/3M_HEAT_CONTENT/}.
#' 
#' @references 
#' Joyce, T.M. and Zhang, R. 2010. On the path of the Gulf Stream and the Atlantic Meridional Overturning Circulation. \emph{Journal of Climate} 23, 3146-3154.
"gsi"