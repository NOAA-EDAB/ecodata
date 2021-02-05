#' Habitat Occupancy
#'
#' Data include habitat area with a probability of occupancy greater than 0.5 was modeled for many species
#' throughout the Northeast Large Marine Ecosystem (NE-LME) using random forest decision tree models.
#'
#'
#' @format 86 rows and 5 columns.
#'
#' \itemize{
#'     \item Value: Value of variable \code{Var}.
#'     \item Var: Season.
#'     \item Time: Time step of \code{Var}.
#'     \item EPU: Ecological Production Unit (EPU) where sampling occurred.
#'
#' }
#'
#' @details Methods used to calculate data are available at \url{https://noaa-edab.github.io/tech-doc/hab-occu.html}.
"habitat_occupancy"
