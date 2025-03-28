#' plot zoo_regime
#'
#' Plots Zooplankton Abundance anomaly
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_zoo_regime <- function(shadedRegion = NULL,
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
   fix<- ecodata::zoo_regime |>
     dplyr::filter(Var %in% c('pseudo_100m3', 'ctyp_100m3', 'calfin_100m3'),
                   EPU %in% filterEPUs) |>
     dplyr::mutate(Var = dplyr::recode(Var,  "ctyp_100m3" = "C.typicus",
                                       "calfin_100m3" = "C.finmachicus"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color=Var))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::facet_wrap(~EPU, scales = "free")+
    ggplot2::ggtitle("")+
    ggplot2::ylab("Abundance anomaly") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linewidth = setup$hline.size,
                      alpha = setup$hline.alpha,
                      linetype = setup$hline.lty)+

    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::scale_color_discrete(
      labels = c(expression(italic("C.finmarchicus")),
                 expression(italic("C.typicus")),
                 expression(italic("Pseudocalanus spp."))))+
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())+
    #ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = element_blank())
    #
    # }

    return(p)
}

attr(plot_zoo_regime,"report") <- c("MidAtlantic","NewEngland")
  # Paste commented original plot code chunk for reference
  # ecodata::dataset |>
  #   dplyr::filter(Var %in% c("..."),
  #                 EPU == "...") |>
  #   ... more dataset wrangling as necessary |>
  #   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   ggplot2::ggtitle("Title")+
  #   ggplot2::ylab(expression("Y label"))+
  #   ggplot2::xlab(element_blank())+
  #   ecodata::geom_gls()+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

