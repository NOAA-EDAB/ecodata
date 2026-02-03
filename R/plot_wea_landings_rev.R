#' plot revenue from top 10 landings
#'
#' plot wea_landings_rev.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n numeric scalar. The number of species to show (default = n = NULL, all species)
#'
#' @return kable object
#'
#'
#' @export
#'

plot_wea_landings_rev <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = NULL) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAFMC")
  } else {
    filterEPUs <- c("NEFMC","ASMFC")
  }

  # set n to dataset length if n is not specified in function call
  if (is.null(n)) {
    n <- as.numeric(nrow(ecodata::wea_landings_rev))
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix<- ecodata::wea_landings_rev |>
    dplyr::filter(Council %in% c(filterEPUs, "MAFMC/NEFMC")) |>
    dplyr::select("NEFMC, MAFMC, and ASMFC Managed Species",
                  "perc_landings_max","perc_revenue_max" ) |>
    dplyr::arrange(desc(perc_revenue_max)) |>
    dplyr::slice_head(n=n)  |>
    dplyr::mutate(perc_landings_max = paste0(perc_landings_max, " %"),
                  perc_revenue_max = paste0(perc_revenue_max, " %")) |>
    dplyr::rename("Maximum Percent Total Annual Regional Species Landings"="perc_landings_max",
                  "Maximum Percent Total Annual Regional Species Revenue"="perc_revenue_max")


  if (report == "MidAtlantic") {
    fix <- dplyr::rename(fix, "MAFMC and Joint Managed Species" = "NEFMC, MAFMC, and ASMFC Managed Species")
  } else {
    fix <- dplyr::rename(fix, "NEFMC, ASMFC and Joint Managed Species" = "NEFMC, MAFMC, and ASMFC Managed Species")
  }

  t <- kableExtra::kable(fix, caption = "Species Landings and Revenue from Leased Areas." ) |>
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")

  return(t)
}
