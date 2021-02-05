#' Gray Seal Bycatch
#'
#' Data include estimated Gray Seal bycatch on the Northeast Shelf.
#'
#'
#' @format 24 rows and 6 columns.
#'
#' \itemize{
#'     \item totalest5y: Estimated bycatch with five year rolling window.
#'     \item totalest5yUCI: Upper confidence interval of estimated bycatch with five year rolling window.
#'     \item totalest5yLCI: Lower confidence interval of estimated bycatch with five year rolling window.
#'     \item totalest1y: Annual estimated bycatch.
#'     \item pbr: Possible Biological Removals.
#'     \item year: Time step.
#' }
#'
#' @details Methods used to calculate data are available at \url{https://noaa-edab.github.io/tech-doc/harbor-porpoise-and-gray-seal-bycatch.html}.
"gray_seal"
