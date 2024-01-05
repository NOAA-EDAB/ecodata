#' plot bottom anomaly temperature time series using GLORYS
#'
#' ecodata::bottom_temp_glorys
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_glorys <- function(shadedRegion=NULL,
                             report = "MidAtlantic") {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  fix <- ecodata::bottom_temp_glorys |>
    dplyr::filter(EPU == filterEPUs)

  #gl_bt<- ecodata::bottom_temp_glorys |>
  #  dplyr::filter(EPU %in% filterEPUs)

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line() +
    ggplot2::geom_point()  +
    ggplot2::facet_wrap(~EPU) +
    ggplot2::geom_hline(yintercept=0,linetype=setup$hline.lty)+
   # ggplot2::geom_point(ggplot2::aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
  #  ggplot2::geom_line(ggplot2::aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
    #ecodata::geom_lm(aes(x = temp_anom$Time, y = temp_anom$Value))+
    ggplot2::ylab("Temperature (C)") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle(paste(report,": GLORYS bottom temperature anomaly")) +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()


  return(p)
}
