#' Commercial and recreational fishery social-economic indicators
#'
#' Data include commercial and recreational fishery engagement and
#' reliance rankings for coastal communities in the Northeast US.
#'
#' @format 150 rows and 5 columns.
#'
#' \itemize{
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#'     \item Time: Year.
#'     \item Var: Percent engagement of group communities.
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
#' }
#'
#' @details
#' NOAA Fisheries’ Community Social Vulnerability Indicators (CSVIs) were developed using
#' secondary data including social, demographic and fisheries variables. The social and
#' demographic data were downloaded from the American Community Survey (ACS) 5-yr
#' estimates Dataset at the U.S. Census American FactFinder site for coastal communities
#' at the Census Designated Place (CDP) level, and in some cases the County Subdivision
#' (MCD) level. Commercial fisheries data were pulled from the Commercial fisheries database (CFDBS).
#' The recreational fishing information is publicly accessible through the Marine Recreational Information
#' Program (MRIP). More information including processing and indicator derivation steps are available at
#' \url{https://noaa-edab.github.io/tech-doc/community-engagement.html}.
"engagement"


