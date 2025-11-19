#' @details
#' The \code{lease_status} column is converted to a \strong{factor} with levels ordered
#' as: "All leases", "Non-active leases", and "Active leases".
#'
#' If \code{port_list} is \code{NULL}, data is filtered by the \code{region} column
#' matching the \code{report} value (e.g., \code{"MidAtlantic"}). If \code{port_list}
#' is provided, data is filtered to include only those port states.
#'
#' The ports (\code{PORT_STATE}) are ordered on the plot based on the
#' \strong{descending \code{avg_val}} for the \strong{"All leases"} status.
#'
#' The plot uses a \strong{dodged column geometry} (\code{geom_col}) and is
#' \strong{coordinate-flipped} (\code{coord_flip}). The title and \code{facet_wrap}
#' layout depend on whether a \code{port_list} was provided.
#'
#' @examples
#' # Assuming 'all_data' is a dataframe with the required columns
#' # (Run these examples after loading required packages like ggplot2 and dplyr)
#'
#' # Example 1: Plotting for the default Mid-Atlantic region
#' # plot_wind_port()
#'
#' # Example 2: Plotting for the New England region (by setting a different report value)
#' # plot_wind_port(report = "NewEngland")
#'
#' # Example 3: Plotting for a specific list of ports
#' # plot_wind_port(port_list = c("New Bedford, MA", "Montauk, NY"))
#'

plot_wind_port <- function(shadedRegion = NULL,
                           report="MidAtlantic",
                           data = all_data,
                           port_list = NULL) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    region <- "Mid Atlantic"
  } else {
    filterEPUs <- c("NE")
    region <- "New England"
  }

  data$lease_status <- factor(data$lease_status,
                              levels = c("All leases",
                                         "Non-active leases",
                                         "Active leases"))

  # filter to ports of interest
  if(is.null(port_list)) {
    data <- data |>
      dplyr::filter(.data$region == report)
  } else {
    data <- data |>
      dplyr::filter(PORT_STATE %in% port_list)
  }

  # order ports
  port_order <- data |>
    dplyr::filter(lease_status == "All leases") |>
    dplyr::arrange(-avg_val)

  data$PORT_STATE <- factor(stringr::str_wrap(data$PORT_STATE, 20),
                            levels = stringr::str_wrap(port_order$PORT_STATE, 20))

  title <- ifelse(is.null(port_list),
                  paste(region, "Port Revenue from Wind Lease Areas"),
                  "Port Revenue from WEA, Majority MAFMC Species")

  plt <- data |>
    # dplyr::filter(.data$region == report) |>
    ggplot2::ggplot(ggplot2::aes(x = PORT_STATE, y = perc_AVG, fill = lease_status)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 1, preserve = "single")) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = perc_MIN, ymax = perc_MAX),
                           width = 0.5,
                           position = ggplot2::position_dodge(width = 1, preserve = "single"),
                           color = "gray20") +
    ggplot2::scale_fill_manual(values = c("Active leases" = "#DB6015",
                                          "Non-active leases" = "#002364",
                                          "All leases" = "#4B8320")) +
    # ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~PORT_STATE,
                        ncol = ifelse(is.null(port_list), 2, 1),
                        scales = "free_y") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   panel.spacing = ggplot2::unit(2, "pt")) +
    ggplot2::ylab("Percent revenue") +
    ggplot2::ggtitle(title)

  return(plt)

}


attr(plot_wind_port,"report") <- c("MidAtlantic","NewEngland")
