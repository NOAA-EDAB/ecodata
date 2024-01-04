#' plot harborporpoise
#'
#'Time series of annual and 5 year rolling mean harbor porpoise bycatch with potential
#' biological removal (PRB) threshold. Same plot for both regions.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_harborporpoise <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  ribbon<- ecodata::harborporpoise |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- ecodata::harborporpoise |>
    dplyr::filter(Var %in% c("pbr", "totalest5y", "totalest1y")) |>
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value, linetype = Var, color = Var))+
    ggplot2::geom_ribbon(data = ribbon, ggplot2::aes(ymin = total5yLCI, ymax =total5yUCI, x = Time), fill = "blue", alpha = 0.2)+
    ggplot2::ggtitle("Harbor Porpoise Bycatch")+
    ggplot2::ylab("Estimated Bycatch (n)")+
    ggplot2::scale_linetype_manual(name="",
                                   values=c(1,2,1),
                                   labels = c("PBR", "Annual Estimates", "5yr rolling mean and CI"))+
    ggplot2::scale_color_manual(name="",values = c('red', 'black','blue'),
                                labels = c("PBR", "Annual Estimates", "5yr rolling mean and CI"))+
    ggplot2::theme(#legend.position = "none",
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.75, 0.8),
      legend.text = ggplot2::element_text(size = 8),
      legend.background = ggplot2::element_rect(
        colour = "transparent", fill = "transparent"))+
    #ggplot2::guides(color = "none")+
    ecodata::theme_ts()+
    ecodata::theme_title()

    return(p)

  # Paste commented original plot code chunk for reference
  # ribbon<- ecodata::harborporpoise %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value)
  #
  # ecodata::harborporpoise %>%
  #   dplyr::filter(Var %in% c("pbr", "totalest5y", "totalest1y")) %>%
  #   ggplot2::ggplot()+
  #   ggplot2::geom_line(aes(x = Time, y = Value, linetype = Var, color = Var))+
  #   ggplot2::geom_ribbon(data = ribbon, aes(ymin = total5yLCI, ymax =total5yUCI, x = Time), fill = "blue", alpha = 0.2)+
  #   ggplot2::ggtitle("Harbor Porpoise Bycatch")+
  #   ggplot2::ylab("Estimated Bycatch (n)")+
  #   ggplot2::scale_color_manual(name = element_blank(), values = c('red', 'black','blue'))+
  #   ggplot2::scale_linetype_manual(values=c(1,2,1),
  #                                  labels = c("PBR", "Annual Estimates", "5yr rolling mean and CI"))+
  #   ggplot2::theme(#legend.position = "none",
  #     legend.title = element_blank(),
  #     legend.position = c(0.75, 0.8),
  #     legend.text = element_text(size = 8),
  #     legend.background = element_rect(
  #       colour = "transparent", fill = "transparent"))+
  #   guides(color = FALSE)+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

}
