#' Plot fuel price indicator
#'
#' uses ecodata::fuel_price
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#' @export

plot_fuel_price <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Average Monthly Diesel Price_Legacy No. 2 Diesel",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = NULL, report = "MidAtlantic")

  # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   message("This indicator is only present in the `MidAtlantic` report")
  #   filterEPUs <- c("GB", "GOM")
  # }

  fix <- ecodata::fuel_price |>
    dplyr::mutate(
      Time = lubridate::ymd(Time),
      Month = lubridate::month(Time, label = TRUE, abbr = TRUE),
      Year = lubridate::year(Time)
    ) |>
    dplyr::group_by(Month) |>
    dplyr::mutate(hline = mean(Value, na.rm = TRUE)) |>
    dplyr::ungroup()

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, group = Month)) +
    ggplot2::geom_line() +
    ecodata::geom_lm(n = 0) +
    ecodata::geom_lm(n = length(unique(fix$Year))) +
    ggplot2::scale_x_continuous(breaks = seq(1990, 2020, by = 10)) +
    ggplot2::facet_wrap(~Month, nrow = 1) +
    ggplot2::ggtitle("Average Monthly Diesel Price") +
    ggplot2::ylab('2025 $USD') +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Month),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ecodata::theme_facet() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "gray80"),
      panel.spacing = ggplot2::unit(0.1, "lines"),
      plot.margin = ggplot2::unit(c(0.1, 0, 0, 0), "cm"),
      text = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
    ) +
    ecodata::theme_title()

  if (report == "NewEngland") {
    p <- "This indicator is only present in the `MidAtlantic` report"
  }

  return(p)
}


attr(plot_fuel_price, "report") <- c("MidAtlantic", "NewEngland")
