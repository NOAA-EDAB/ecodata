#' plot revenue by port from wind energy
#'
#' plot wind_port data
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param data Data frame. Data to be plotted. Default is from 'all_data.csv'
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wind_port <- function(shadedRegion = NULL,
                           report="MidAtlantic",
                           data = all_data) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  p <- data |>
    dplyr::filter(report == region) |>
    dplyr::mutate(PORT_STATE = stringr::str_wrap(PORT_STATE, 20)) |>
    ggplot2::ggplot(ggplot2::aes(x = PORT_STATE, y = perc_AVG, fill = lease_status)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 1, preserve = "single")) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = perc_MIN, ymax = perc_MAX),
                           width = 0.5,
                           position = ggplot2::position_dodge(width = 1, preserve = "single"),
                           color = "gray20") +
    ggplot2::scale_fill_manual(values = c("Active leases" = "#DB6015",
                                          "Non-active leases" = "#002364",
                                          "All leases" = "#4B8320")) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(rows = ggplot2::vars(PORT_STATE), scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top") +
    ggplot2::ylab("Percent revenue") +
    ggplot2::ggtitle(report, "Port Revenue from Wind Lease Areas") +
    ggplot2::facet_wrap(~PORT_STATE, ncol = 2, scales = "free_y")

  return(p)

}

attr(plot_wind_port,"report") <- c("MidAtlantic","NewEngland")
