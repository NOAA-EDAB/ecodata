#' plot aquaculture
#'
#' Creates line plot of oyster production.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_aquaculture <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB") # no EPU in 2023 dataset
    ylabel <- "Oyster production"
    aqua <- ecodata::aquaculture |>
      dplyr::filter(Region %in% c("MD", "VA", "NJ")) |>
      dplyr::filter(!Value == "NA") |>
      dplyr::mutate(Time = as.integer(Time), Value = as.numeric(Value))
  } else {
    filterEPUs <- c("GB", "GOM") # no EPU in 2023 dataset
    ylabel <- "Production/Acre"
    aqua <- ecodata::aquaculture |>
      dplyr::ungroup() |>
      dplyr::mutate(Region = as.character(Region)) |>
      dplyr::filter(
        !Region == "VA",
        !Region == "NJ",
        !Region == "MD",
        !Region == "NA",
        !Value == "NA"
      ) |>
      dplyr::mutate(Time = as.integer(Time), Value = as.numeric(Value)) |>
      dplyr::filter(Var == "Production/Acre") |>
      dplyr::group_by(Time) |>
      dplyr::summarise(Value = sum(Value))
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  # see above, differs by region

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  # create xaxis label breaks
  yrs <- unique(ecodata::aquaculture$Time)
  breaks <- seq(min(yrs), max(yrs), 2)

  if (report == "MidAtlantic") {
    p <- aqua |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Region)) +
      ggplot2::geom_line(linewidth = setup$lwd) +
      ggplot2::geom_point(size = setup$lwd)
  }

  if (report == "NewEngland") {
    p <- aqua |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
      ggplot2::geom_line(linewidth = setup$lwd) +
      ggplot2::geom_point(size = setup$lwd)
  }

  p <- p +
    ggplot2::ggtitle(paste0("Oyster Production in ", setup$region)) +
    ggplot2::ylab(expression(ylabel)) +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(
      breaks = breaks
    ) +
    #ecodata::geom_gls() + n < 30
    ecodata::geom_lm(n = n) +
    ecodata::theme_ts() +
    ecodata::theme_title()

  return(p)
}


attr(plot_aquaculture, "report") <- c("MidAtlantic", "NewEngland")
