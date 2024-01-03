#' plot forage anomaly
#'
#' Plots ecodata::forage_anomaly
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_forage_anomaly <- function(shadedRegion = NULL,
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
   fix <- ecodata::forage_anomaly |>
     dplyr::filter(EPU %in% filterEPUs) |>
     tidyr::pivot_wider(names_from = Var, values_from = Value)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x=Time, y = Forage_Mean))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Forage_Lower, ymax = Forage_Upper, x = Time), alpha = 0.3)+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")+
    ggplot2::ggtitle("Forage Anomalies")+
    ggplot2::ylab("Forage Anomaly")+
    ggplot2::facet_wrap(~EPU)+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::theme_ts()+
    ecodata::geom_gls()+
    ecodata::theme_facet() +
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = element_blank())

    }

    return(p)



}
