#' plot revenue by port from wind lease areas
#'
#' plot wind_port data
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wind_port <- function(
  shadedRegion = NULL,
  report = "MidAtlantic"
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

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

  ## convert back to semi-wide format
  data <- ecodata::wind_port |>
    dplyr::filter(.data$EPU == filterEPUs) |>
    tidyr::separate_wider_delim(
      cols = Var,
      delim = "-",
      names = c("Var", "lease_status")
    ) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::mutate(
      PORT_STATE = paste(.data$City, .data$State, sep = ", ") |>
        stringr::str_squish()
    ) |>
    dplyr::select(-c(State, City))

  data$lease_status <- factor(
    data$lease_status,
    levels = c(
      "All leases",
      "Leases, construction not in progress",
      "Leases, construction in progress or complete"
    )
  )

  # order ports
  port_order <- data |>
    dplyr::filter(lease_status == "All leases") |>
    dplyr::arrange(-MaxVal)

  # data$PORT_STATE <- factor(stringr::str_wrap(data$PORT_STATE, 20),
  #                           levels = stringr::str_wrap(port_order$PORT_STATE, 20))
  data$PORT_STATE <- factor(data$PORT_STATE, levels = port_order$PORT_STATE)

  title <- paste(region, "Port Revenue from Wind Lease Areas")

  # update options here if wanting to add two columns
  num_cols <- ifelse(length(unique(data$PORT_STATE)) > 5, 1, 1)

  # only keep "all lease" column for ports that can't display by lease status
  plt_data <- data |>
    # drop na's created by pivoting
    tidyr::drop_na(perc_AVG) |>
    dplyr::group_by(PORT_STATE) |>
    dplyr::mutate(keep_all_lease = dplyr::n() == 1) |>
    dplyr::filter(
      lease_status != "All leases" |
        keep_all_lease
    )

  # print(head(plt_data))

  symbol_data <- data |>
    dplyr::select(PORT_STATE, Demographics, Gentrification) |>
    tidyr::pivot_longer(
      cols = c(Demographics, Gentrification),
      names_to = "Variable"
    ) |>
    dplyr::filter(!value == "NA") |>
    dplyr::mutate(
      symbol = dplyr::recode(Variable, Demographics = -7, Gentrification = -3),
      Variable = dplyr::recode(
        Variable,
        "Demographics" = "Mid-High to High Demographics Concerns",
        "Gentrification" = "Mid-High to High Gentrification Concerns"
      )
    )

  plt <- plt_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = PORT_STATE,
      y = perc_AVG,
      fill = lease_status
    )) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 1, preserve = "single")
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = perc_MIN, ymax = perc_MAX),
      width = 0.5,
      position = ggplot2::position_dodge(width = 1, preserve = "single"),
      color = "gray20"
    ) +
    ggplot2::geom_point(
      data = symbol_data,
      ggplot2::aes(y = symbol, x = PORT_STATE, shape = Variable),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_shape_manual(values = c(16, 17)) +
    ggplot2::scale_fill_manual(
      values = c(
        "Leases, construction in progress or complete" = "#DB6015",
        "Leases, construction not in progress" = "#002364",
        "All leases" = "#4B8320"
      )
    ) +
    ggplot2::ylim(c(NA, 100)) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~PORT_STATE, ncol = num_cols, scales = "free_y") +
    ggplot2::theme(
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
      plot.caption = ggplot2::element_text(hjust = 0, size = 8)
    ) +
    ggplot2::ylab("Percent revenue") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = 1, order = 1),
      shape = ggplot2::guide_legend(ncol = 1, order = 2)
    ) +
    ggplot2::ggtitle(title) +
    ggh4x::force_panelsizes(rows = ggplot2::unit(0.35, "cm"))

  return(plt)
}

# plot_wind_port()
# plot_wind_port(report = "NewEngland")

attr(plot_wind_port, "report") <- c("MidAtlantic", "NewEngland")
