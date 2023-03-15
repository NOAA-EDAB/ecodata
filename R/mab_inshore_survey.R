#' NEAMAP Mid-Atlantic inshore trawl survey
#'
#' These data are aggregated time series of inshore fishery-independent trawl survey data from the Mid-Atlantic Bight.
#' Surveys are performed biannually in the spring and fall months as part of the Northeast Area Monitoring and
#' Assessment Program (NEAMAP), covering the inshore region between Cape Cod, MA and Cape Hatteras, NC. Cooperative survey
#' efforts are led by scientists at the Virginia Institute of Marine Science.
#'
#' @format These data contain 248 rows and 4 columns
#'
#' \itemize{
#'     \item Var: Specifies variable type, including SOE species groupings and sampling season with stratified mean biomass per tow ("index")
#'     or sample coefficient of variation ("cv").
#'     \item Value: Value of variable \code{Var}.
#'     \item Time: Sampling year.
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#' }
#'
#' @details
#'
#' Stratified mean biomass (kg/tow) was calculated by:
#'
#' 1. Species catch weights were summed for each tow and within each feeding guild.
#' 2. Average weight per tow with associated variances and standard deviation for each
#' year/season/stratum/feeding guild combination was calculated.
#' 3. The final index was then calculated as the sum of the weighted averages of the strata, where the
#' weight of a given stratum was the proportion of the survey area accounted for by that
#' stratum (i.e., stratum area/survey area).
#'
#' Find more information about these data at \url{http://www.vims.edu/research/departments/fisheries/programs/multispecies_fisheries_research/neamap/index.php}.
"mab_inshore_survey"
