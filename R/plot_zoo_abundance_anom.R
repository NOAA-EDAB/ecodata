#' plot zooplankton abundance anomaly
#'
#' Plots zoo_abundance_anom data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("copepod","euphausid")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_zoo_abundance_anom <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "copepod",
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

  if (varName == "copepod") {
    varName <-  "large-bodied|small-bodied"
    vtitle <- "Small and large-bodied copepod abundance anomaly"
  } else if (varName == "euphausid") {
    varName <-  "Euphausiacea|Cnidaria"
    vtitle <- "Zooplankton abundance anomaly"

  } else {
    stop("Please select either 'copepod' or 'euphausid'")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::zoo_abundance_anom |>
     dplyr::mutate(Var = dplyr::recode(Var,LgCopepods = "large-bodied",
                                       SmCopepods = "small-bodied")) |>
     dplyr::filter(EPU %in% filterEPUs,
                   stringr::str_detect(Var, varName))  |>
     dplyr::mutate(Value = as.numeric(Value),
                   hline = 0)

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
    ggplot2::ggtitle(paste0(report,": ",vtitle))+
    ggplot2::ylab(expression("Abundance anomaly"))+
    ggplot2::xlab(ggplot2::element_blank())+
    #ggplot2::scale_colour_discrete(name = "Copepods", labels = c("large-bodied", "small-bodied"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
               linewidth = setup$hline.size,
               alpha = setup$hline.alpha,
               linetype = setup$hline.lty)+
    ggplot2::facet_wrap(~EPU~Var)+
    ecodata::geom_gls() +
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "none") +
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

attr(plot_zoo_abundance_anom,"varName") <- c("copepod","euphausid")
attr(plot_zoo_abundance_anom,"report") <- c("MidAtlantic","NewEngland")
