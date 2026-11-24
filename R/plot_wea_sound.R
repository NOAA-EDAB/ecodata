plot_wea_sound <- function(
  plottype = c("timeseries", "bar"),
  varName = c("COX01", "NS02")
) {
  ### input data ----
  data <- wea_sound |>
    dplyr::mutate(Time = lubridate::as_date(Time)) |>
    tidyr::separate_wider_delim(
      cols = Var,
      delim = "_",
      names = c("location", "Category")
    ) |>
    dplyr::filter(location == varName)

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
      ggplot(aes(x = Time, y = Value)) +
      geom_rect(
        data = threshold_bands,
        aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = ymin,
          ymax = ymax,
          fill = Category
        ),
        color = "white",
        inherit.aes = FALSE
      ) +
      scale_fill_manual(
        values = setNames(threshold_bands$fill, threshold_bands$Category)
      ) +
      geom_line() +
      theme_bw() +
      theme(
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.text = ggplot2::element_text(size = 10),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_blank()
      ) +
      coord_cartesian(expand = FALSE) +
      ylab("Median Broadband SPL (dB re 1 μPa)") +
      ggplot2::labs(title = paste(varName, "Ambient Noise"))
  }

  if (plottype == "bar") {
    plt_data <- data |>
      dplyr::mutate(year = lubridate::year(Time)) |>
      dplyr::group_by(year) |>
      dplyr::count(Category) |>
      dplyr::mutate(proportion = n / sum(n))

    plt <- plt_data |>
      ggplot(aes(x = year, y = proportion)) +
      theme_bw() +
      geom_col(aes(fill = Category)) +
      scale_fill_manual(
        values = setNames(threshold_bands$fill, threshold_bands$Category)
      ) +
      coord_cartesian(expand = FALSE) +
      scale_x_continuous(
        breaks = seq(min(plt_data$year), max(plt_data$year), by = 1)
      ) +
      xlab("Year") +
      ylab("Proportion of days in each category") +
      theme(
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.text = ggplot2::element_text(size = 10),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::labs(title = paste(varName, "Ambient Noise Proportions"))
  }

  return(plt)
}
