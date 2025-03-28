#' plot SST anomaly (OISST)
#'
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10), passed from plot function
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland"), passed from plot function
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#' @export

plot_seasonal_oisst_anom <- function(shadedRegion = NULL,
                                     report = "MidAtlantic",
                                     EPU="MAB",
                                     n = 0) {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  ne_anom <- ecodata::seasonal_oisst_anom |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::mutate(Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall"))) |>
    dplyr::mutate(Var = factor(Var, levels= c("Winter","Spring","Summer","Fall")))

  # ne_anom <- ne_anom %>%
  #   dplyr::filter(Var %in% season)
  #
  p <- ggplot2::ggplot(data = ne_anom,
                                 ggplot2::aes(x = Time, y = Value,  group = EPU)) + #color = EPU,
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::ylim(-2,3)+
    ggplot2::ylab(expression("SST Anomaly (C)")) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle(paste0(EPU,": SST Anomaly (OISST)")) +
    ggplot2::scale_color_manual(values = c("black","indianred"))+
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::geom_hline(yintercept = 0,
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ggplot2::facet_wrap(~Var, scales = "free_y")+
    ecodata::theme_facet() +
    ecodata::geom_gls() +
    ecodata::geom_lm(n=n)+
    #ecodata::geom_lm(aes(x = Time, y = Value))+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   plot.title = ggplot2::element_text(size = 12))+
    ecodata::theme_title()


  return(p)
}

attr(plot_seasonal_oisst_anom,"EPU") <- c("MAB","GB","GOM")
attr(plot_seasonal_oisst_anom,"report") <- c("MidAtlantic","NewEngland")
