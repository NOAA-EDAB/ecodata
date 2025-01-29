#' plot demo for SOE report
#'
#' Plots demo timeseries and EPU map for SOE presentations
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export

plot_demo <- function(shadedRegion = NULL,
                      report="MidAtlantic",
                      n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # set a seed to always produce the same plot
  set.seed(111)
  m <- 0.1
  x <- 1985:setup$shadedRegion[2]
  y <-  m*x + rnorm(length(x), sd = 0.25)
  #reset to random seed
  set.seed(NULL)

  # set up data to plot
  data <- data.frame(x = x,
                     y = y)
  #Define constants for figure plot
  hline = mean(y)

  #Plot series with trend
  psample <- ggplot2::ggplot(data = data,ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = setup$pcex) +
    ecodata::geom_gls() +
    ecodata::geom_lm(n=10,pValThreshold = 0.5) +
    ggplot2::scale_color_manual(aesthetics = "color")+
    ggplot2::guides(color = "none") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
               linewidth = setup$hline.size,
               alpha = setup$hline.alpha,
               linetype = setup$hline.lty)+
    ggplot2::geom_line() +
    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
    ggplot2::scale_x_continuous(breaks = seq(1985, setup$shadedRegion[2]+1, by = 5), expand = c(0.01, 0.01)) +
    #ggplot2::ylab(expression("Invented Index, 10"^3*"widgets")) +
    ggplot2::ylab("") +
    ggplot2::xlab("Time") +
    ggplot2::labs(tag = "a")  +
    ecodata::theme_ts()


  # Specify data frame with lat/lon locations for labels
  epu_labels <- data.frame(EPU = c("Mid-Atlantic\n Bight",
                                   "Gulf of Maine",
                                   "Georges Bank"),
                           latitude = c(40,42.85,41),
                           longitude = c(-72.7,-69,-68.5))

  # Map of NE LME
  epumap <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = setup$epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::coord_sf(xlim = setup$xlims, ylim = setup$ylims) + #crs = crs,  +
    ggplot2::geom_text(data = epu_labels,
                       ggplot2::aes(x = longitude,
                                    y = latitude,
                                    label = EPU),
                       size = 1.7) +
    ecodata::theme_map() +
    ggplot2::scale_x_continuous(breaks = seq(-78, -65, by = 4),
                                expand = c(0.01, 0.01)) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(tag = "b")

  # combine figures
  # p <-  patchwork::plot_layout(psample + epumap,
  #                          ncol = 2,
  #                          nrow = 1,
  #                          widths=c(.75,1),heights = c(.4,1))

  p <- ggpubr::ggarrange(psample,
                         epumap,
                         nrow = 1,
                         ncol = 2)

  return(p)

}
