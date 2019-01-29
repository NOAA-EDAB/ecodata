#' EcoMon nutrient and oceanographic data
#' 
#' These data include average nutrient concentrations, temperature, salinity, density, and dissolved oxygen
#' sampled via CTD profiles on NEFSC Ecosystem Monitoring (EcoMon) cruises between 2009-11-03 to 2018-06-04.
#' Data presented here are mean values for specified Ecological Production Unit and year of sampling.
#' 
#' 
#' @format Data set includes 1189 rows and 5 columns
#' 
#' \itemize{
#'     \item Var: Specifies variable of interest at different sampling depths, where \code{bottom} refers to samples taken
#'     within 10 m of the benthos, \code{surface} refers to samples taken within 5 m of the surface, and \code{mid-water} refers
#'     to everything in-between.
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#'     \item Time: Year that sampling occurred.
#' }
#' 
#' 
#' @details Full variable definitions, references, and metadata are available at \url{https://www.nodc.noaa.gov/oads/data/0127524.xml}
"ecomon"