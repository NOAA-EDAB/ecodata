#' MAMFC ABC or ACL for Managed Stocks
#'
#' These data contain times series of ABC or ACL and Catch for Mid-Atlantic managed
#' stocks
#'
#'
#' @format A data set containing 321 rows and 4 columns.
#'
#' \itemize{
#'     \item Time: Management year.
#'     \item Value: Value of variable \code{Var}
#'     \item Var: Either ABC or ACL depending on what is available for that stock and Catch.
#'     \item EPU: Ecological Production Unit (EPU) where data originated. Here "MAB" refers to the Mid-Atlantic Bight.
#' }
#'
#' @details More information available at \url{https://noaa-edab.github.io/tech-doc/mafmc-abcacl-and-catch.html}
"abc.acl"
