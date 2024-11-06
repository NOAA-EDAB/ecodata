#' plot recreational data
#'
#' Plot recdat data set, effort, diversity, and landings
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Variable to plot ("landings","effortdiversity","catchdiversity","effort")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_recdat <- function(shadedRegion = NULL,
                        report="MidAtlantic",
                        varName = "landings",
                        n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MA")
  } else {
    filterEPUs <- c("NE")
  }

  if (varName =="landings") {
    varName <- "Recreational Seafood"
    vtitle <- "Recreational seafood harvest"
    vylab <- expression("Landings (10"^6*" lbs)")
    scalar <- 1e6
  } else if (varName == "effort") {
    varName <- "Recreational Effort"
    vtitle <- "Recreational Effort"
    vylab <- expression("Angler Trips (Number x 10"^6*")")
    scalar <- 1e6
  } else if (varName == "effortdiversity") {
    varName <- "Recreational fleet effort diversity across modes"
    vtitle <- "Recreational fleet effort diversity"
    vylab <- expression("Effective Shannon")
    scalar <- 1

  } else if (varName =="catchdiversity") {
    varName <- "Recreational Diversity of Catch"
    vtitle <- "Recreational diversity of catch"
    vylab <- expression("Effective Shannon")
    scalar <- 1
  }


  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix <- ecodata::recdat |>
     dplyr::filter(EPU %in% filterEPUs,
                   Var == varName) |>
     dplyr::mutate(Value = Value/scalar) |>
     dplyr::mutate(hline = mean(Value))

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
    ggplot2::geom_point(size = setup$pcex)+
    ggplot2::geom_line(size=setup$lwd)+
    ggplot2::ggtitle(vtitle)+
    ggplot2::ylab(vylab)+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ecodata::geom_gls()+
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)


}

attr(plot_recdat,"varName") <- c("landings","effortdiversity","catchdiversity","effort")
attr(plot_recdat,"report") <- c("MidAtlantic","NewEngland")
