#' Fall thermal habitat suitability for managed species
#'
#' These data contain time series of the percentage of each managed species' historic fall
#' habitat footprint where daily bottom temperature (September-November) fell within the
#' species' thermal niche, anomalies relative to the 1982-2011 mean, and the mean anomaly
#' across managed species in each Council region.
#'
#' @format A data set containing 4690 rows and 5 columns.
#'
#' \itemize{
#'     \item Time: Year.
#'     \item Value: Value of variable \code{Var}.
#'     \item Var: Species and variable, separated by " - ". "Percent suitable" is the percentage of the species' fall footprint with bottom temperature inside its thermal niche; "Anomaly" is the difference from the species' 1982-2011 mean. "Managed species mean - Anomaly" is the unweighted mean anomaly of Council-managed species; "Managed species mean (incl. ASMFC) - Anomaly" also includes ASMFC-managed species.
#'     \item EPU: Council report the data are summarized for. Here "NE" refers to New England (NEFMC and jointly managed species) and "MA" to the Mid-Atlantic (MAFMC and jointly managed species).
#'     \item Units: Units of variable \code{Var}.
#' }
#'
#' @details More information available at \url{[tech-doc URL]}
"thermal_suitability"