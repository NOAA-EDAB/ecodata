#' plot abundance of Euphasids and Cnids
#'
#' Plots zoo_abund_strat data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_zoo_strat_abun <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic",
                              epu = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(epu %in% c("GB", "GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- epu
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::zoo_strat_abun |>
     dplyr::filter(EPU %in% filterEPUs) |>
     dplyr::mutate(Value = log10(Value+1))  |>
     dplyr::group_by(Var, EPU) |>
     dplyr::mutate(Value = as.numeric(Value),
                   hline = mean(Value, na.rm = TRUE))

   if (nrow(fix) == 0) {
     stop(paste0("Please check to make sure data are available for this epu (",epu,")"))
   }

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
    ggplot2::ggtitle(paste0(filterEPUs,": Zooplankton Abundance"))+
    ggplot2::ylab(expression("Log Stratified Abundance"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::facet_wrap(~Var)+
    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)



}
