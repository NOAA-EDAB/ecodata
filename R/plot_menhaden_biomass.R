#' Plot menhaden biomass time series
#'
#' uses ecodata::menhaden_biomass
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#' @export

plot_menhaden_biomass <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "menhaden_biomass",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   message("This indicator is only present in the `MidAtlantic` report")
  #   filterEPUs <- c("GB", "GOM")
  # }

  fix <- ecodata::menhaden_biomass |>
    dplyr::mutate(hline = mean(Value))

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
    ggplot2::ggtitle("Menhaden Age1+ Biomass") +
    ggplot2::ylab("Thousands mt") +
    ggplot2::xlab(ggplot2::element_blank()) +
    ecodata::geom_gls() +
    ecodata::geom_lm(n = n) +
    ecodata::theme_ts() +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ecodata::theme_title()

  if (report == "NewEngland") {
    p <- "This indicator is only present in the `MidAtlantic` report"
  }

  return(p)
}


attr(plot_menhaden_biomass, "report") <- c("MidAtlantic", "NewEngland")
