#' plot SST transition_dates
#'
#' @param season Character string. Season to plot. (Default = NULL, plot all seasons)
#' @param region Character vector. Regional EPUs ("GB","MAB") to overly on figure. (Default = NULL, use all)
#'
#' @return ggplot object
#'
#' @export

plot_sst_transition_dates <- function(season = NULL,
                                      region = NULL) {

  if(is.null(region)){
    region <- c("GOM","GB")
  }
  if(is.null(season)) {
    season <- c("Winter","Spring","Summer","Fall")
  }

  p <- ecodata::trans_dates %>%
    dplyr::filter(EPU %in% region,
                  Var %in% c("falltrans", "sprtrans"),
                  !Value == "NA",
                  !Var == "NA",
                  !Time == "NA") %>%
    dplyr::mutate(Var = dplyr::recode(Var, "falltrans"="Fall",
                               "sprtrans" = "Spring",
                               "maxday" = "Max")) %>%
    dplyr::filter(Var %in% season) %>%
    ggplot2::ggplot(ggplot2::aes(x= Time, y = Value, color = Var)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ecodata::geom_gls() +
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   plot.title = ggplot2::element_text(size = 12)) +
    ecodata::theme_title("") +
    ggplot2::ylab("Day of Year") +
    ggplot2::facet_wrap(.~EPU) +
    ecodata::theme_facet() +
    ggplot2::xlab(ggplot2::element_blank())

  return(p)
}
