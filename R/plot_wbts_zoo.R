#' plot zoo_diversity
#'
#' zooplanktn diversity index (Shannon index)
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

plot_wbts_zoo <- function(shadedRegion = NULL,
                              report="NewEngland",
                              n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   wbts.zoo.data = ecodata::wbts_zoo$data |>
     tidyr::separate(Time, into = c("Year", "day_of_year")) |>
     dplyr::mutate(year_period = dplyr::case_when(
       Year >= 2004 & Year <= 2010  ~ "2004-2010",
       Year > 2010 & Year <= 2024 ~ "2011 - 2024",
       Year == 2025 ~ "2025"
     )) |>
     dplyr::mutate(
       day_of_year = as.numeric(day_of_year)
     )

   wbts.zoo.model = ecodata::wbts_zoo$model |>
     tidyr::pivot_wider(names_from = Var, values_from = c(Value))



  # p <- fix |>
   ggplot2::ggplot() +
     ggplot2::geom_point(data = wbts.zoo.data, ggplot2::aes(x = day_of_year, y = Value, fill = year_period, color = year_period,
                                    shape = year_period, size = year_period)) +
     ggplot2::geom_line(data = wbts.zoo.model, ggplot2::aes(x = day_of_year, y = pred), color = "black") +
     ggplot2::geom_ribbon(data = wbts.zoo.model, ggplot2::aes(x = day_of_year, ymin = pred_low, ymax = pred_high), fill = "grey", alpha = 0.4) +
     # ggplot2::scale_fill_manual(values = colors, name = "Year Period") +
     # ggplot2::scale_color_manual(values = colors, name = "Year Period") +
     # ggplot2::scale_shape_manual(values = shapes, name = "Year Period") +
     # ggplot2::scale_size_manual(values = sizes, name = "Year Period") +
     # ggplot2::scale_x_continuous(breaks = month_breaks, labels = month_labels, limits = c(1, 365)) +
     ggplot2::scale_y_continuous(breaks = c(100, 200, 300, 400, 500), labels = c("10", "40", "90", "160", "250")) +
     ggplot2::scale_y_continuous(name = expression(paste("CIII - CIV Abundance (",  " 1,000"~m^{-2}, ")"))) +
     ggplot2::labs(x = "", title = "WBTS, Calanus Climatology") +
     ggplot2::geom_vline(xintercept = c(75.5, 147.5, 247.5, 364.5), linetype = "dashed", color = "black", linewidth = 1, alpha = 0.3) +
     ggplot2::theme_bw() +
     ggplot2::theme(
       plot.margin =  ggplot2::margin(1, 2, 1, 2),
       legend.position = "none"
     )

  return(p)
}

attr(plot_wbts_zoo,"report") <- c("NewEngland")
