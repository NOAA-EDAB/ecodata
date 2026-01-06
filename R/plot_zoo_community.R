#' plot zooplankton community PCA
#'
#' Zooplankton community index using a principal component analysis
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

plot_zoo_community <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = 0) {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # convert to leading capital
  if (report == "MidAtlantic") {
      filterEPUs <- c("MAB")
    } else {
      filterEPUs <- c("GB", "GOM")
    }

  fix<- ecodata::zoo_community |>
    dplyr::filter(EPU %in% filterEPUs)

  p <- ggplot2::ggplot(data = fix, ggplot2::aes(x = Time, y = Value, color = Var))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_hline(yintercept = 0, lty = setup$hline.lty, linewidth = setup$hline.size, alpha = setup$hline.alpha)+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle("")+
    ggplot2::ylab(paste("Principal Component Values"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::facet_wrap(.~EPU)+
    ecodata::geom_gls()+
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

  if (report == "NewEngland") {
    p <- p +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())

  }

  return(p)

}

attr(plot_zoo_community,"report") <- c("MidAtlantic","NewEngland")

