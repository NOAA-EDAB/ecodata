#' ABC or ACL for Managed Stocks
#'
#' These data contain times series of ABC or ACL and Catch for Mid-Atlantic and New England managed
#' stocks
#'
#'
#' @format A data set containing 1930 rows and 5 columns.
#'
#' \itemize{
#'     \item Time: Management year.
#'     \item Value: Value of variable \code{Var}
#'     \item Var: Quota or Catch for each Fisheries Management Plan.
#'     \item EPU: Ecological Production Unit (EPU) where species is managed. Here "MAB" refers to the Mid-Atlantic Bight and "NE" refers to New England.
#'     \item Units: Metric tons (mt)
#'}
#'
#' @details More information available at \url{https://noaa-edab.github.io/tech-doc/mafmc-abcacl-and-catch.html} and \url{https://noaa-edab.github.io/tech-doc/quota-and-catch---new-england.html}
"abc_acl"
