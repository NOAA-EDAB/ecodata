#' plot gulf stream index
#'
#' plots gsi dataset. This is a shelfwide indicator
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("gsi","westgsi")
#' @return ggplot object
#'
#'
#' @export
#'

plot_gsi <- function(shadedRegion = NULL,
                     report="MidAtlantic",
                     varName = "gsi") {

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

  if (varName == "gsi") {
    var <- "gulf stream index"
  } else {
    var <- "western gulf stream index"
  }
  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below



  fix <- ecodata::gsi  |>
    dplyr::filter(Var == var) |>
    dplyr::mutate(Year = floor(Time)) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Value = mean(Value)) |>
    dplyr::mutate(hline = mean(Value))|>
    dplyr::rename(Time = Year)

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
    ggplot2::ggtitle(stringr::str_to_title(var))+
    ggplot2::ylab("Anomaly")+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
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


attr(plot_gsi,"report") <- c("MidAtlantic","NewEngland")
attr(plot_gsi,"varName") <- c("gsi","westgsi")
