#' plot gulf stream index
#'
#' plots gsi dataset. This is a shelfwide indicator
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("gsi","westgsi")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#' @param trendStats logical. If TRUE returns a list object of plot and model fit statistics
#' @param plotdata logical. If TRUE returns a list object of plot and cleaned underlying dataset
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_gsi <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "gsi",
  n = 0,
  trendStats = FALSE,
  plotdata = FALSE
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    message("This indicator is only present in the `MidAtlantic` report")
    filterEPUs <- c("GB", "GOM")
  }

  if (varName == "gsi") {
    var <- "gulf stream index"
  } else {
    var <- "western gulf stream index"
  }

  fix <- ecodata::gsi |>
    dplyr::filter(Var == var) |>
    dplyr::mutate(Year = floor(Time)) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Value = mean(Value)) |>
    dplyr::mutate(hline = mean(Value)) |>
    dplyr::rename(Time = Year) |>
    ecodata:::format_plot_data(var = varName, epu = filterEPUs)

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
    ggplot2::ggtitle(stringr::str_to_title(var)) +
    ggplot2::ylab("Anomaly") +
    ggplot2::xlab(NULL) +
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
    if (trendStats) {
      return(list(plot = p, stats = NULL, data = NULL))
    }
    return(p)
  } else {
    return(ecodata:::finalize_plot(
      p,
      data = fix,
      plotdata = plotdata,
      trendStats = trendStats
    ))
  }
}

attr(plot_gsi, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_gsi, "varName") <- c("gsi", "westgsi")
