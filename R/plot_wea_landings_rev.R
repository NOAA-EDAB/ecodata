#' plot revenue from top 10 landings
#'
#' plot wea_landings_rev.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n numeric scalar. The number of species to show (default = n = 10, top 10 species)
#'
#' @return kable object
#'
#'
#' @export
#'

plot_wea_landings_rev <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = 10) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix<- ecodata::wea_landings_rev |>
    dplyr::select("NEFMC, MAFMC, and ASMFC Managed Species",
                  "perc_landings_max","perc_revenue_max" ) |>
    dplyr::slice_head(n=n)  |>
    dplyr::mutate(perc_landings_max = paste0(perc_landings_max, " %"),
                  perc_revenue_max = paste0(perc_revenue_max, " %")) |>
    dplyr::rename("Maximum Percent Total Annual Regional Species Landings"="perc_landings_max",
                  "Maximum Percent Total Annual Regional Species Revenue"="perc_revenue_max")

  t <- kableExtra::kable(fix, caption = "Top ten species Landings and Revenue from Leased Areas." ) |>
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")


  return(t)
}
