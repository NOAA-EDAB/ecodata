#' plot revenue from top 10 landings
#'
#' plot wea_landings_rev.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n numeric scalar. The number of species to show (default = n = 0, all species)
#'
#' @return flextable object
#'
#'
#' @export
#'

plot_wea_landings_rev <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAFMC")
  } else {
    filterEPUs <- c("NEFMC", "ASMFC")
  }

  # set n to dataset length if n is not specified in function call
  if (n == 0) {
    n <- as.numeric(nrow(ecodata::wea_landings_rev))
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix <- ecodata::wea_landings_rev |>
    dplyr::mutate(
      "NEFMC, MAFMC, and ASMFC Managed Species" = stringr::str_remove(
        Var,
        "_perc.*"
      )
    ) |>
    dplyr::mutate(Var = stringr::str_remove(Var, ".*_")) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::filter(Jurisdiction %in% c(filterEPUs, "MAFMC/NEFMC")) |>
    dplyr::select(
      "NEFMC, MAFMC, and ASMFC Managed Species",
      "perc landings max",
      "perc revenue max"
    ) |>
    dplyr::arrange(desc("perc revenue max")) |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(
      "perc landings max" = paste0(`perc landings max`, " %"),
      "perc revenue max" = paste0(`perc revenue max`, " %")
    ) |>
    dplyr::rename(
      "Maximum Percent Total Annual Regional Species Landings" = "perc landings max",
      "Maximum Percent Total Annual Regional Species Revenue" = "perc revenue max"
    )

  if (report == "MidAtlantic") {
    fix <- dplyr::rename(
      fix,
      "MAFMC and Joint Managed Species" = "NEFMC, MAFMC, and ASMFC Managed Species"
    )
  } else {
    fix <- dplyr::rename(
      fix,
      "NEFMC, ASMFC and Joint Managed Species" = "NEFMC, MAFMC, and ASMFC Managed Species"
    )
  }

  t <- flextable::flextable(fix) |>
    flextable::set_caption(
      caption = "Species Landings and Revenue from Leased Areas."
    ) |>
    flextable::font(fontname = "Cambria", part = "all") |>
    flextable::autofit()

  return(t)
}

attr(plot_wea_landings_rev, "report") <- c("MidAtlantic", "NewEngland")
