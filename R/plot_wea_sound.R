#' Plot Ambient Underwater Sound Levels in Wind Energy Areas (WEAs)
#'
#' @description
#' `plot_wea_sound()` generates faceted `ggplot2` visualizations of median
#' broadband sound pressure levels (SPL) across Wind Energy Area monitoring
#' locations, rendered either as a daily time series overlaid on quantile-based
#' acoustic regime bands or as an annual stacked bar chart of category proportions.
#'
#' @details
#' The function reads the `wea_sound` dataset directly from the global or package
#' environment, parses `Time` into `Date` objects, and splits the `Var` string
#' (`"<location>_<Category>"`) into `location` and `Category` (`"High"`, `"Typical"`,
#' `"Low"`).
#'
#' Acoustic regime thresholds are calculated dynamically across all non-missing
#' observations in `wea_sound$Value`:
#' \itemize{
#'   \item **`"Low"`** (`#99CCFF`): 94 dB to the 25th percentile (`q25`).
#'   \item **`"Typical"`** (`#CCCCCC`): 25th percentile (`q25`) to the 75th percentile (`q75`).
#'   \item **`"High"`** (`#FF9999`): 75th percentile (`q75`) to 146 dB.
#' }
#'
#' Depending on `plottype`, two visualizations are supported:
#' \itemize{
#'   \item **`"timeseries"`**: Plots daily median broadband SPL (`dB re 1 uPa`) over
#'         time against shaded background quantile bands, faceted by `location`.
#'   \item **`"bar"`**: Aggregates daily observations by year and `location`, plotting
#'         the annual proportion of days spent in each acoustic `Category`.
#' }
#'
#' @param plottype Character string specifying the plot layout to return. Must be
#'   one of `"timeseries"` (default) or `"bar"`. Also stored as a `"plottype"`
#'   attribute on the function object for programmatic discovery.
#'
#' @return A `ggplot` object faceted by monitoring `location` (`ncol = 2`).
#'
#' @note
#' \strong{Data & Environment Dependencies:}
#' \itemize{
#'   \item Background threshold rectangles in `"timeseries"` mode hardcode lower
#'         and upper y-axis bounds at `94` and `146` dB; values outside `[94, 146]`
#'         will extend beyond the shaded bands.
#' }
#'
#' @importFrom dplyr mutate row_number group_by count
#' @importFrom ecodata theme_facet
#' @importFrom ggplot2 ggplot aes geom_rect scale_fill_manual geom_line
#'   facet_wrap theme_bw theme element_blank element_text coord_cartesian ylab
#'   xlab labs geom_col scale_x_continuous
#' @importFrom lubridate as_date year
#' @importFrom stats quantile setNames
#' @importFrom tidyr separate_wider_delim pivot_wider
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot daily ambient noise time series with quantile threshold bands
#' plot_wea_sound(plottype = "timeseries")
#'
#' # Plot annual proportion of days in Low, Typical, and High noise categories
#' plot_wea_sound(plottype = "bar")
#' }

plot_wea_sound <- function(
  plottype = c("timeseries")
) {
  ### input data ----
  data <- ecodata::wea_sound |>
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

attr(plot_wea_sound, "plottype") <- c("timeseries", "bar")
