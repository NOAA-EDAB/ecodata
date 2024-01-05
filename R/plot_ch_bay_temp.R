#' plot ch_bay_temp
#'
#' Plot Chesapeake Bay temperature daily time series.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_ch_bay_temp <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   filterEPUs <- c("GB", "GOM")
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   CBtemp <- ecodata::ch_bay_temp |>
     tidyr::pivot_wider(names_from = Var, values_from = Value) |>
     dplyr::mutate(YearLTAC = (YearLTA - 32)*(5/9),
                   minLTAC = (minLTA - 32)*(5/9),
                   maxLTAC = (maxLTA - 32)*(5/9),
                   YearC = (Year - 32)*(5/9))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- CBtemp |>
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = minLTAC, ymax = maxLTAC), fill = "grey", alpha = 0.5)+

    ggplot2::geom_line(ggplot2::aes(x = Time, y = YearLTAC, color= "Long Term Average 2010-2020")) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = YearC, color = "Daily 2022")) +
    ggplot2::ylab("Temperature (C)") +
    ggplot2::ggtitle("Chesapeake Bay Temperature") +
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
  # ecodata::ch_bay_temp %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
  #   dplyr::mutate(YearLTAC = (YearLTA - 32)*(5/9),
  #                 minLTAC = (minLTA - 32)*(5/9),
  #                 maxLTAC = (maxLTA - 32)*(5/9),
  #                 YearC = (Year - 32)*(5/9)) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_ribbon(aes(x = Time, ymin = minLTAC, ymax = maxLTAC), fill = "grey", alpha = 0.5)+
  #
  #   ggplot2::geom_line(aes(x = Time, y = YearLTAC, color= "Long Term Average 2010-2020")) +
  #   ggplot2::geom_line(aes(x = Time, y = YearC, color = "Daily 2022")) +
  #   ggplot2::ylab("Temperature (C)") +
  #   ggplot2::ggtitle("Chesapeake Bay Temperature") +
  #   ggplot2::theme(legend.position = "bottom",
  #                  legend.title = element_blank())+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()  #
  #

}
