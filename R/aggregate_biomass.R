#' NEFSC survey data broken into season and aggregate group biomass
#'
#' These data contain times series of CPUE, kg tow ^-1, of aggregated species groups from NEFSC surveys.
#'
#'
#' @format A data set containing 52286 rows and 5 columns.
#'
#' \itemize{
#'     \item Time: Survey year.
#'     \item Value: Value of variable \code{Var}
#'     \item Var: Includes SOE species grouping, season that survey occurred (fall or spring), and variable calculated. The specific variables included are stratified mean biomass with associated confidence intervals, coefficients of variation, and standard errors.
#'     \item EPU: Ecological Production Unit (EPU) where data originated. Here "MAB" refers to the Mid-Atlantic Bight.
#'     \item Units: Units of variable \code{Var}.
#' }
#'
#' @details More information available at \url{https://noaa-edab.github.io/tech-doc/survdat.html}
"aggregate_biomass"
