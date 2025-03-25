#' plot Warm core rings
#'
#' plots wcr dataset
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

plot_wcr <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                     n=0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    message("This indicator is only present in the `MidAtlantic` report")
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # upper and lower are used to define the two regimes?
  # where year 2000 come from? Estimated change point from publication?

  fix <- ecodata::wcr

  upper.line <- fix |>
    dplyr::filter(Time>2000 & Time <=2017)  |>
    dplyr::mutate(hline = c(mean(Value))) |>
    dplyr::select(Time,hline)

  lower.line<-ecodata::wcr |>
    dplyr::filter(Time<2000)  |>
    dplyr::mutate(hline = c(mean(Value))) |>
    dplyr::select(Time,hline)


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle("Warm Core Rings")+
    ggplot2::ylab("Birth frequency")+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::geom_lm(n=n)+
#    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ggplot2::geom_segment(data = upper.line, ggplot2::aes(x = min(Time), y = hline,
                                                 xend = max(Time), yend = hline, color = "segment") )+
    ggplot2::geom_segment(data = lower.line, ggplot2::aes(x = min(Time), y = hline,
                                                 xend = max(Time), yend = hline, color = "segment") )+
    ggplot2::theme(legend.position = "none")+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    if (report == "NewEngland"){
      p <- NULL
    }

    return(p)

}

attr(plot_wcr,"report") <- c("MidAtlantic","NewEngland")
