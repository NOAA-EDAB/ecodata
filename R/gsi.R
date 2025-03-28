#' Gulf Stream Index
#'
#' Annual time series of the Gulf Stream Index. Positive
#' values are a more northerly Gulf Stream, and negative values are a more southerly
#' Gulf Stream.
#'
#' @format Time series with 816 rows and 4 columns
#'
#' \itemize{
#'     \item Var: Variable of interest.
#'     \item Value: Value of variable \code{Var}.
#'     \item EPU: Representative Ecological Production Unit (MAB = Mid-Atlantic Bight, GB = Georges Bank,
#'     GOM = Gulf of Maine, SS = Scotian Shelf, or All).
#'     \item Time: Years.
#' }
#'
#' @details The GSI is the degrees latitude above the average Gulf Stream position based on ocean temperature
#' at 200 m (15 C) depth between 55°W to 75°W. Ocean temperature data used for this analysis are available
#' at \url{https://resources.marine.copernicus.eu/?option=com_csw&view=details&product_id=SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047}.
#'
#' @references
#' Joyce, T.M. and Zhang, R. 2010. On the path of the Gulf Stream and the Atlantic Meridional Overturning Circulation. \emph{Journal of Climate} 23, 3146-3154.
"gsi"
