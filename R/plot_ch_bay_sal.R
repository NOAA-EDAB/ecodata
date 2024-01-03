#' plot ch_bay_sal
#'
#' Plot Chesapeake Bay salinity daily time series.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_ch_bay_sal <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   filterEPUs <- c("GB", "GOM")
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  CBsal <- ecodata::ch_bay_sal |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- CBsal |>
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = minLTA, ymax = maxLTA), fill = "grey", alpha = 0.5)+

    ggplot2::geom_line(ggplot2::aes(x = Time, y = YearLTA, color= "Long Term Average 2010-2020")) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Year, color = "Daily 2022")) +
    ggplot2::ylab("Salinity") +
    ggplot2::ggtitle("Chesapeake Bay Salinity") +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())+
    ecodata::theme_ts()+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)

  # Paste commented original plot code chunk for reference
  # MAB only
  # ecodata::ch_bay_sal %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_ribbon(aes(x = Time, ymin = minLTA, ymax = maxLTA), fill = "grey", alpha = 0.5)+
  #
  #   ggplot2::geom_line(aes(x = Time, y = YearLTA, color= "Long Term Average 2010-2020")) +
  #   ggplot2::geom_line(aes(x = Time, y = Year, color = "Daily 2022")) +
  #   ggplot2::ylab("Salinity") +
  #   ggplot2::ggtitle("Chesapeake Bay Salinity") +
  #   ggplot2::theme(legend.position = "bottom",
  #                  legend.title = element_blank())+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

}
