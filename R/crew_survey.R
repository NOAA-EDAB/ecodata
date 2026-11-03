#' Crew Survey
#'
#' The crew survey is an intercept survey that evaluates demographic and job satisfaction information of crew and hired captains
#'
#' @format A data set containing 3048 rows and 7 columns.
#'
#' \itemize{
#'     \item Time: Year
#'     \item Value: Value of Metric
#'     \item Var: Short-hand survey question name.
#'     \item Var.Long: original survey question name.
#'     \item Var.Answer: categorical answer to question or NA if a numeric answer
#'     \item Var.Group: Question Categories
#'     \item Units: metric type evaluated for each question ("mean likehart" for likehart scale questions, "mean" for mean of numeric responses, "proportion responses" for the proportion of responses with an EPU, )
#'     \item EPU: Ecological Production Unit (EPU) where data originated. Either "NE" for "New England" or "MA" for "Mid-Atlantic"
#'
#' }
#'
#' @details More information available at \url{https://noaa-edab.github.io/tech-doc/crew_survey.html}
"crew_survey"
