#' plot SST transition_dates
#'
#' @param season Character string. Season to plot. (Default = NULL, plot all seasons)
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot
#' @param shade.fill Character string. Color of shaded region. (Default = "lightgrey)
#' @param shade.alpha Numeric scalar. Alpha of shaded region (Default = 0.5)
#' @param hline.size Numeric scalar Line width of horizontal line. (Default = 1)
#' @param hline.alpha Numeric scalar. Alpha of horizontal line (Default = 0.5)
#' @param hline.lty Character string. Line type of horizontal line (Default = "dashed")
#'
#' @return ggplot object
#'
#' @export

plot_sst_transition_dates_length <- function(season = NULL,
                                              region = NULL,
                                              shadedRegion=c(2012,2022),
                                              shade.fill="lightgrey",
                                              shade.alpha=0.3,
                                              hline.size = 1,
                                              hline.alpha = 0.5,
                                              hline.lty = "dashed") {

  if(is.null(region)){
    region <- c("GOM","GB")
  }
  # if(is.null(season)) {
  #   season <- c("Winter","Spring","Summer","Fall")
  # }

  x.shade.min <- shadedRegion[1]
  x.shade.max <- shadedRegion[2]

  p <- ecodata::trans_dates %>%
    dplyr::filter(EPU %in% region,
                  Var == "sumlen",
                  !Value == "NA") %>%
    #dplyr::filter(Var %in% season) %>%
    ggplot2::ggplot(ggplot2::aes(x= Time, y = Value))+
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                      xmin = x.shade.min , xmax = x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ecodata::geom_gls() +
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   plot.title = ggplot2::element_text(size = 12))+
    ecodata::theme_title()+
    ggplot2::ylab("Number of Days")+
    ggplot2::xlab(element_blank())+
    ecodata::theme_ts()+
    ggplot2::facet_wrap(.~EPU)+
    ecodata::theme_facet()
  return(p)
}
