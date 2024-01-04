#' plot Warm core rings
#'
#' plots wcr dataset
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wcr <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    stop("This indicator is only present in the `MidAtlantic` report")
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # upper and lower are used to define the two regimes?
  # where year 2000 come from?
  upper.line<-ecodata::wcr |>
    dplyr::filter(Time>2000)  |>
    dplyr::mutate(hline = c(mean(Value)))
  lower.line<-ecodata::wcr |>
    dplyr::filter(Time<2000)  |>
    dplyr::mutate(hline = c(mean(Value)))

  fix<- upper.line  |>
    rbind(lower.line)


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

    return(p)

}
