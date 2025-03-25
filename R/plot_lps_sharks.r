#' plot shark fishing landings from large pelagics survey
#'
#' Plots lps_sharks data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("Total", "Blue_Shark", "Common_Thresher", "Shortfin_Mako")
#' Default plot is Total pelagic sharks.
#'
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_lps_sharks <- function(shadedRegion = NULL,
                            report="MidAtlantic",
                            varName = "Total",
                            n=0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix <- ecodata::lps_sharks |>
     dplyr::filter(EPU %in% filterEPUs,
                   Var %in% varName) |>
     dplyr::mutate(Value = Value/1000)

   ltm <- fix |>
     dplyr::summarise(hline = mean(Value, na.rm = T))

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
    ecodata::geom_lm(n = n) +
    ggplot2::ggtitle(paste(report, "Recreational Pelagic Shark Landings"))+
    ggplot2::ylab(expression("Number of Fish (1000s)"))+
    ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = ltm$hline),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+

#    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
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

attr(plot_lps_sharks,"report") <- c("MidAtlantic","NewEngland")
attr(plot_lps_sharks,"varName") <- c("Total", "Blue_Shark", "Common_Thresher", "Shortfin_Mako")


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
