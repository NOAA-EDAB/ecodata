plot_wea_sound <- function(
  plottype = c("timeseries", "bar")
) {
  ### input data ----
  data <- wea_sound |>
    dplyr::mutate(Time = lubridate::as_date(Time)) |>
    tidyr::separate_wider_delim(
      cols = Var,
      delim = "_",
      names = c("location", "Category")
    )

  data$Category = factor(
    data$Category,
    levels = c("High", "Typical", "Low")
  )

  ### assign thresholds for plotting ----
  thresholdsAllData = as.data.frame(quantile(
    wea_sound$Value,
    probs = c(0.01, 0.25, 0.5, 0.75, 0.99),
    na.rm = TRUE
  ))
  colnames(thresholdsAllData) = "thresholds"

  quantThresholds <- thresholdsAllData |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::pivot_wider(names_from = id, values_from = thresholds)
  colnames(quantThresholds) = c("q01", "q25", "q50", "q75", "q99")

  threshold_bands <- data.frame(
    Category = c("Low", "Typical", "High"),
    ymin = c(94, quantThresholds$q25, quantThresholds$q75),
    ymax = c(quantThresholds$q25, quantThresholds$q75, 146),
    fill = c("#99CCFF", "#CCCCCC", "#FF9999")
  )

  threshold_bands$Category = factor(
    threshold_bands$Category,
    levels = c("High", "Typical", "Low")
  )

  ### create plots ----

  if (plottype == "timeseries") {
    plt <- data |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
      ggplot2::geom_rect(
        data = threshold_bands,
        ggplot2::aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = ymin,
          ymax = ymax,
          fill = Category
        ),
        color = "white",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = setNames(threshold_bands$fill, threshold_bands$Category)
      ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~location, ncol = 2) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "right",
        axis.title.x = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 10),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::ylab("Median Broadband SPL (dB re 1 μPa)") +
      ggplot2::labs(title = "Ambient Noise") +
      ecodata::theme_facet()
  }

  if (plottype == "bar") {
    plt_data <- data |>
      dplyr::mutate(year = lubridate::year(Time)) |>
      dplyr::group_by(year, location) |>
      dplyr::count(Category) |>
      dplyr::mutate(proportion = n / sum(n))

    plt <- plt_data |>
      ggplot2::ggplot(ggplot2::aes(x = year, y = proportion)) +
      ggplot2::theme_bw() +
      ggplot2::geom_col(ggplot2::aes(fill = Category)) +
      ggplot2::scale_fill_manual(
        values = setNames(threshold_bands$fill, threshold_bands$Category)
      ) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(
        breaks = seq(min(plt_data$year), max(plt_data$year), by = 1)
      ) +
      ggplot2::facet_wrap(~location, ncol = 2) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Proportion of days in each category") +
      ggplot2::theme(
        legend.position = "right",
        axis.title.x = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 10),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::labs(title = "Ambient Noise Proportions") +
      ecodata::theme_facet()
  }

  return(plt)
}
