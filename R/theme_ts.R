#'A ggplot2 theme for SOE time series figures
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' m <- 0.1
#' x <- 1:30
#' y <-  m*x + rnorm(30, sd = 0.35)
#'
#' data <- data.frame(x = x,
#'                   y = y)
#'
#' #Plot series with trend and SOE plot theme
#' ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_gls(aes(x = x, y = y))+
#'   theme_ts()

theme_ts <- function(...) {
  th <- list(
    ggplot2::theme(
      # font sizes
      plot.title = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      # no x-label for time series
      axis.title.x = ggplot2::element_blank(),

      # panel background
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 0.75
      ),

      # facets
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 12),

      # legend
      legend.key = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),
    ),
    ggplot2::scale_color_discrete(labels = format_legend_labels)
  )
  class(th) <- c("ecodata_theme", class(th))
  th
}

# ecodata::plot_comdat_profit() + theme_ts()
# ecodata::plot_rec_hms() + theme_ts()

#' Format legend labels: split camelCase, replace underscores, and apply sentence case
#' @noRd
format_legend_labels <- function(x) {
  # Handle factors safely by converting to character
  x <- as.character(x)

  # 1. Insert spaces at camelCase boundaries:
  #    - Between a lowercase letter/digit and an uppercase letter (e.g., "seaSurface" -> "sea Surface")
  #    - Between consecutive uppercase letters and a following lowercase letter (e.g., "SSTAnomaly" -> "SST Anomaly")
  x <- gsub(
    "(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])",
    " ",
    x,
    perl = TRUE
  )

  # 2. Replace underscores with spaces
  x <- gsub("_", " ", x)

  # 3. Collapse any accidental multiple spaces and trim leading/trailing whitespace
  x <- trimws(gsub("\\s+", " ", x))

  # 4. Apply sentence case (preserving NA values)
  ifelse(
    is.na(x) | nchar(x) == 0,
    x,
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  )
}

#' Format specific EPU names in plot titles
#' @param x Character vector or factor containing plot titles
#' @return Character vector with "NewEngland" and "MidAtlantic" formatted
#' @noRd
format_plot_title <- function(x) {
  x <- as.character(x)
  x <- gsub("NewEngland", "New England", x, fixed = TRUE)
  x <- gsub("MidAtlantic", "Mid-Atlantic", x, fixed = TRUE)
  x
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ecodata_theme <- function(object, plot, object_name) {
  # Format the title if one has already been set on the plot
  if (!is.null(plot$labels$title)) {
    plot$labels$title <- format_plot_title(plot$labels$title)
  }

  # Strip the custom class and delegate standard theme addition to ggplot2
  class(object) <- setdiff(class(object), "ecodata_theme")
  ggplot2::ggplot_add(object, plot, object_name)
}
