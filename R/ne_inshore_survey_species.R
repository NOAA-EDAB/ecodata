#' New England inshore state survey data species
#'
#' These data contain species categories of inshore (within 12 miles) fishery-independent survey data collected by the Maine Department of Marine Resources
#' and New Hampshire Fish & Game Department. Time series are grouped by State of the Ecosystem feeding guild. Species assignments
#' within groupings are available through \code{inshore_survdat_species}.
#'
#' @format A data set containing 208 rows and 4 columns.
#'
#'  \itemize{
#'       \item ...1: Number.
#'       \item Species: List of species.
#'       \item Group: Species groupings.
#'       \item Var: Survey source.
#' }
#'
#' @details
#' Metrics for biomass were calculated by:
#' 1.	All species catch weights were summed up for each tow for each group.
#' 2.	Then the average weight per tow and associated variances and standard deviation for each
#' survey, region, stratum, and species group was calculated.
#' 3.	The average weight for each group was then weighted by total area surveyed in each region
#' and stratum. (average weight * area surveyed).
#' 4.	The new weighted averages were then summed up by survey and species group then the totals
#' were divided by the total area of the survey (11699.831 km2) to provide the stratified mean biomass
#' for each species group in each survey. The coefficient of variation, standard error, and 95%
#' confidence intervals were also calculated for each species group and provided.
#'
#' Read more about inshore surveys at \url{https://wildlife.state.nh.us/marine/research.html} and
#' \url{https://www.maine.gov/dmr/science-research/projects/trawlsurvey/index.html}
"ne_inshore_survey_species"
