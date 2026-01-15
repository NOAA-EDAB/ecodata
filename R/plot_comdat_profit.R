#' plot comdat profitability
#'
#' Plots Geret's profitability indices
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("all","profit_index", "cost_index", "revenue_index")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be "MAB"
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_comdat_profit <- function(shadedRegion = NULL,
                               report = "MidAtlantic",
                               varName = "all",
                               EPU = "MAB",
                               n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB", "GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  if (varName == "profit_index") {
    indextype <- "Profitability Index"
  } else if (varName == "cost_index") {
    indextype <- "Cost Index"
  } else if (varName == "revenue_index") {
    indextype <- "Revenue Index"
  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max

  if (varName == "all") {
    p <-  ecodata::comdat_profit |>
      dplyr::filter(EPU %in% filterEPUs) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Var))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf)+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ecodata::theme_ts()+
      ecodata::theme_title()+
      ggplot2::ylab("Index")+
      ggplot2::ggtitle(paste0(EPU,": Profitability, Cost and Revenue Indices"))+
      ecodata::theme_facet()+
      ecodata::geom_lm(n=n)
  } else {

  p <-  ecodata::comdat_profit |>
     dplyr::filter(Var == varName,
                   EPU %in% filterEPUs) |>
     ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
     ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                       xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                       ymin = -Inf, ymax = Inf)+
     ggplot2::geom_point()+
     ggplot2::geom_line()+
     ecodata::theme_ts()+
     ecodata::theme_title()+
     ggplot2::ylab(paste0(indextype))+
     ggplot2::ggtitle(paste0(EPU,": ",indextype))+
     ecodata::theme_facet()+
     ecodata::geom_lm(n=n)
  }

  return(p)

}
attr(plot_comdat_profit,"varName") <- c("profit_index","cost_index","revenue_index")
attr(plot_comdat_profit,"report") <- c("MidAtlantic","NewEngland")
attr(plot_comdat_profit,"EPU") <- c("MAB","GB","GOM")

