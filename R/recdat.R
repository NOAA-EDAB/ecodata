#' Recreational fishing indicators
#'
#' Recreational fishing indicators derived from Marine Recreational Information Program data queries.
#'
#' @format 320 rows and 5 columns
#'
#' \itemize{
#'     \item Var: Specifies variable name.
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
#'     \item Time: Year.
#'     \item EPU: Ecological Production Units where data originated. In this case, \code{MA} refers
#'     to Mid-Atlantic, and \code{NE} to New England.
#' }
#'
#' @details
#' There are five indicator time series within this data set: recreational effort (number of days fished),
#' recreational seafood (number of fish caught), recreational anglers (number of anglers), diversity of recreational
#' catch (effective Shannon index), and recreational fleet effort diversity across modes (effective Shannon index).
#'
#' More information about each of these indicators is available at
#' \url{https://noaa-edab.github.io/tech-doc/recreational-fishing-indicators.html}.
#'
#' @source
#' Read more about the MRIP database and perform your own queries at \url{https://www.st.nmfs.noaa.gov/recreational-fisheries/data-and-documentation/run-a-data-query}.
"recdat"
