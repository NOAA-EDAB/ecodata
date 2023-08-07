#' plot bottom anomaly temperature time series using Glorys data
#'
#' ecodata::bottom_temp_glorys
#'
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot
#' @param shade.fill Character string. Color of shaded region. (Default = "lightgrey)
#' @param shade.alpha Numeric scalar. Alpha of shaded region (Default = 0.5)
#' @param hline.size Numeric scalar Line width of horizontal line. (Default = 1)
#' @param hline.alpha Numeric scalar. Alpha of horizontal line (Default = 0.5)
#' @param hline.lty Character string. Line type of horizontal line (Default = "dashed")
#'
#' @importFrom ggplot2 aes element_text element_blank theme element_rect margin
#' @importFrom magrittr "%>%"
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_anomaly_glorys <- function(region = NULL,
                                          shadedRegion=c(2012,2022),
                                          shade.fill="lightgrey",
                                          shade.alpha=0.3,
                                          hline.size = 1,
                                          hline.alpha = 0.5,
                                          hline.lty = "dashed") {
  # find all EPUs in dtaa set
  regions <- unique(ecodata::bottom_temp_glorys$EPU)

  if(is.null(region) || !(region %in% regions)){
    stop(paste("please specify a region to plot:", paste(regions,collapse = ",")))
  }

  x.shade.min <- shadedRegion[1]
  x.shade.max <- shadedRegion[2]

  gl_bt<- ecodata::bottom_temp_glorys%>%
    dplyr::filter(EPU == region)

  bt<- ecodata::bottom_temp %>%
    dplyr::filter(EPU == region,
                  Var == "bottom temp anomaly in situ") %>%
    dplyr::mutate(hline = 0)

  p_bottomtemp<- ggplot2::ggplot()+ #plot
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                      xmin = x.shade.min , xmax = x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(aes(x = bt$Time, y = bt$Value)) +
    ecodata::geom_gls(aes(x = bt$Time, y = bt$Value)) +
    #ecodata::geom_lm(aes(x = bt$Time, y = bt$Value))+
    ggplot2::geom_point(aes(x = bt$Time, y = bt$Value), size = 1) +
    ggplot2::geom_point(aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
    ggplot2::geom_line(aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
    ggplot2::ylab("Temperature Anomaly (C)") +
    ggplot2::xlab(element_blank())+
    ggplot2::ylim(-1.4,2)+
    ggplot2::ggtitle(region) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::geom_hline(aes(yintercept = bt$hline),
                        size = hline.size,
                        alpha = hline.alpha,
                        linetype = hline.lty) +
    ecodata::theme_ts() +
    ggplot2::theme(strip.text=element_text(hjust=0),
                   plot.title = element_text(size = 12))+
    ecodata::theme_title()



  return(p_bottomtemp)
}
