#' plot ches_bay_wq
#'
#' Plot time series of Chesapeake Bay water quality indicator.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_ches_bay_wq <- function(shadedRegion = shadedRegion,
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
  # three year moving average, these labels requested for clarity
  minlab <- seq(1987,2017,5)
  maxlab <- seq(1989,2019,5)
  minl<- as.numeric(sprintf('%02d',minlab %% 100))
  maxl<- sprintf('%02d', maxlab %% 100)

  CBwq <- ecodata::ches_bay_wq |>
    dplyr::mutate(hline = mean(Value))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- CBwq |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ecodata::geom_gls() +
    #ecodata::geom_lm()+
    ggplot2::ylab(expression("Estimated attainment (%)")) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Chesapeake Bay Water Quality Attainment") +
    ggplot2::scale_x_continuous(breaks = minlab,labels = c("87-89", "92-94",
                                                           "97-99", "02-04",
                                                           "07-09", "12-14", "17-19")) +
    #ggplot2::scale_x_descrete(breaks = minl,labels = paste0(minl,"-",maxl),expand = c(0.01, 0.01)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ecodata::theme_ts()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)

  # Paste commented original plot code chunk for reference
  # minlab <- seq(1987,2017,5)
  # maxlab <- seq(1989,2019,5)
  # minl<- as.numeric(sprintf('%02d',minlab %% 100))
  # maxl<- sprintf('%02d', maxlab %% 100)
  # ecodata::ches_bay_wq %>%
  #   dplyr::mutate(hline = mean(Value)) %>%
  #   ggplot2::ggplot(aes(x = Time, y = Value)) +
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_line() +
  #   ggplot2::geom_point() +
  #   ecodata::geom_gls() +
  #   #ecodata::geom_lm()+
  #   ggplot2::ylab(expression("Estimated attainment (%)")) +
  #   ggplot2::xlab(element_blank())+
  #   ggplot2::ggtitle("Chesapeake Bay Water Quality Attainment") +
  #   ggplot2::scale_x_continuous(breaks = minlab,labels = c("87-89", "92-94",
  #                                                          "97-99", "02-04",
  #                                                          "07-09", "12-14", "17-19")) +
  #   #ggplot2::scale_x_descrete(breaks = minl,labels = paste0(minl,"-",maxl),expand = c(0.01, 0.01)) +
  #   ggplot2::geom_hline(aes(yintercept = hline),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty) +
  #   ecodata::theme_ts()
  #

}
