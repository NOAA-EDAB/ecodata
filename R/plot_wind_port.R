#' @title Plot Port Revenue for Wind Lease Areas
#'
#' @description
#' Generates a horizontal bar plot (using \code{ggplot2}) to visualize the
#' percentage of port revenue derived from areas corresponding to wind energy leases.
#' The data can be filtered by a specific reporting region or a custom list of ports.
#' Bars represent the average percentage of revenue (\code{perc_AVG}), and error
#' bars represent the minimum (\code{perc_MIN}) and maximum (\code{perc_MAX}) percentages.
#'
#' @param shadedRegion A numeric vector or list passed to \code{ecodata::plot_setup}
#'   to define a shaded region on the plot. **Currently unused** in the final
#'   \code{ggplot} call but maintained for consistency with other plotting functions.
#'   Defaults to \code{NULL}.
#' @param report A character string specifying the reporting region to filter the data.
#'   Must be one of \code{"MidAtlantic"} (for the Mid-Atlantic region/MAB EPU) or
#'   any other value (which defaults to "New England"/NE EPU). This filter is
#'   applied only if \code{port_list} is \code{NULL}. Defaults to \code{"MidAtlantic"}.
#' @param data A data frame containing the necessary revenue and lease status
#'   information. It must include the columns: \code{lease_status}, \code{region},
#'   \code{PORT_STATE}, \code{avg_val}, \code{perc_AVG}, \code{perc_MIN}, and \code{perc_MAX}.
#'   Defaults to \code{all_data}.
#' @param port_list A character vector of port names (contained in the \code{PORT_STATE}
#'   column) to specifically include in the plot. If provided, the \code{report}
#'   argument is ignored, and the data is filtered by this list. Defaults to \code{NULL}.
#'   Used to create the plot of NE ports landing majority MAFMC species from WEAs.
#' @param mode If set to "opp_region", data must have an `opp_region` column
#' @param cap Footnote text, optional
#'
#' @return A \code{ggplot} object (the bar plot).
#' @export
#'
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
                           port_list = NULL,
                           mode = "regular",
                           cap = "") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    region <- "Mid Atlantic"
    council <- "MAFMC"
  } else {
    filterEPUs <- c("NE")
    region <- "New England"
    council <- "NEFMC"
  }

  ## Update lease category names -- transfer this into data wrangling
  data <- data |>
    dplyr::mutate(lease_status = dplyr::case_when(lease_status == "Active leases" ~ "Active leases",
                                                  lease_status == "Non-active leases" ~ "Leases, construction not in progress",
                                                  TRUE ~ lease_status))

  data$lease_status <- factor(data$lease_status,
                              levels = c("All leases",
                                         "Leases, construction not in progress",
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
    dplyr::arrange(-WEA_MAX)

  # data$PORT_STATE <- factor(stringr::str_wrap(data$PORT_STATE, 20),
  #                           levels = stringr::str_wrap(port_order$PORT_STATE, 20))
  data$PORT_STATE <- factor(data$PORT_STATE,
                            levels = port_order$PORT_STATE)

  title <- dplyr::case_when(is.null(port_list) ~ paste(region, "Port Revenue from Wind Lease Areas"),
                            TRUE ~ paste("Port Revenue from Lease Areas, Majority", council, "Species"))

  # update options here if wanting to add two columns
  num_cols <- ifelse(length(unique(data$PORT_STATE)) > 5, 1, 1)

  # only keep "all lease" column for ports that can't display by lease status
  plt_data <- data |>
    dplyr::group_by(PORT_STATE) |>
    dplyr::mutate(keep_all_lease = dplyr::n() == 1) |>
    dplyr::filter(lease_status != "All leases" |
                    keep_all_lease)

  # print(head(plt_data))

  symbol_data <- data |>
    dplyr::select(PORT_STATE, Demographics, Gent) |>
    tidyr::pivot_longer(cols = c(Demographics, Gent), names_to = "Variable") |>
    dplyr::filter(!value == "NA") |>
    dplyr::mutate(symbol = dplyr::recode(Variable, Demographics = -7, Gent = -3),
                  Variable = dplyr::recode(Variable,"Demographics"= "Mid-High to High Demographics Concerns" ,
                                           "Gent" ="Mid-High to High Gentrification Concerns"))

  ## add footnote symbols to ports, if needed (only for opp region plots)
  if(mode == "opp_region") {
    footnote_ports <- plt_data |>
      dplyr::filter(lease_status != "All leases") |>
      dplyr::mutate(lease_status = dplyr::case_when(stringr::str_detect(lease_status, "not") ~ "non_active",
                                                    TRUE ~ "active")) |>
      dplyr::select(PORT_STATE, opp_region, lease_status) |>
      tidyr::pivot_wider(names_from = "lease_status", values_from = "opp_region") |>
      dplyr::mutate(footnote = dplyr::case_when(isTRUE(.data$active) & is.na(.data$non_active) ~ "\u00b2",
                                                is.na(.data$active) & isTRUE(.data$non_active) ~ "\u00b9",
                                                TRUE ~ ""))

    #
    plt_data <- plt_data |>
      dplyr::left_join(footnote_ports) |>
      # replace any NAs with blanks for ports that only have all lease data (east haven)
      dplyr::mutate(footnote = dplyr::case_when(is.na(footnote) ~ "",
                                                TRUE ~ footnote),
                    PORT_STATE = paste0(PORT_STATE, footnote))

    symbol_data <- symbol_data |>
      dplyr::left_join(footnote_ports) |>
      dplyr::mutate(PORT_STATE = paste0(PORT_STATE, footnote))

  }

  plt <- plt_data |>
    # dplyr::filter(lease_status != "All leases") |>
    # dplyr::filter(.data$region == report) |>
    ggplot2::ggplot(ggplot2::aes(x = PORT_STATE, y = perc_AVG * 100, fill = lease_status)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 1, preserve = "single")) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = perc_MIN * 100, ymax = perc_MAX * 100),
                           width = 0.5,
                           position = ggplot2::position_dodge(width = 1, preserve = "single"),
                           color = "gray20") +
    ggplot2::geom_point(data = symbol_data,
                        ggplot2::aes(y = symbol,
                                     x = PORT_STATE,
                                     shape = Variable),
                        inherit.aes = FALSE) +
    ggplot2::scale_shape_manual(values = c(16, 17)) +
    ggplot2::scale_fill_manual(values = c("Active leases" = "#DB6015",
                                          "Leases, construction not in progress" = "#002364",
                                          "All leases" = "#4B8320")) +
    ggplot2::ylim(c(NA, 100)) +
    # ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~PORT_STATE,
                        ncol =  num_cols,
                        scales = "free_y") +
    ggplot2::theme(#aspect.ratio = 0.15,
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.spacing = ggplot2::unit(2, "pt"),
      panel.border = ggplot2::element_rect(color = "gray50"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = 1),
      # not sure any of this space adjustment is actually working
      legend.key.spacing.y = ggplot2::unit(1, "pt"),
      legend.spacing.y = ggplot2::unit(0, "pt"),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.margin = ggplot2::unit(0, "pt"),
      plot.caption = ggplot2::element_text(hjust = 0,
                                           size = 8)) +
    ggplot2::ylab("Percent revenue") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, order = 1),
                    shape = ggplot2::guide_legend(ncol = 1, order = 2)) +
    ggplot2::ggtitle(title) +
    ggh4x::force_panelsizes(rows = ggplot2::unit(0.35, "cm"))

  if(mode == "opp_region") {
    region_labs <- plt_data |>
      dplyr::filter(is.na(opp_region))
    plt <- plt +
      # ggplot2::geom_point(data = region_labs,
      #                     ggplot2::aes(y = perc_MAX * 100 + 5,
      #                                  x = PORT_STATE,
      #                                  group = lease_status),
      #                     inherit.aes = FALSE,
      #                     position = ggplot2::position_dodge(width = 1, preserve = "single"),
      #                     pch = 8) +
      ggplot2::labs(caption = cap)
  }

  return(plt)

}

attr(plot_wind_port, "report") <- c("MidAtlantic", "NewEngland")
