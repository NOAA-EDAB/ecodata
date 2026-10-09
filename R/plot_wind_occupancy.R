#' plot probability of species occupancy in wind area
#'
#' plot wind_occupancy data. This is not region specific?
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return flextable object
#'
#'
#' @export
#'

plot_wind_occupancy <- function(shadedRegion = NULL, report = "MidAtlantic") {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  wind1 <- ecodata::wind_occupancy
  wind1$trend <- ifelse(
    wind1$Trend == "pos",
    "\u2197",
    ifelse(wind1$Trend == "neg", "\u2198", " ")
  )
  wind2 <- wind1 |> dplyr::select(Area, Season, Species, trend)
  names <- c("Area", "Season", "Species", "trend")
  bnew <- c("Area.1", "Season.1", "Species.1", "trend.1")
  cnew <- c("Area.2", "Season.2", "Species.2", "trend.2")
  dnew <- c("Area.3", "Season.3", "Species.3", "trend.3")
  enew <- c("Area.4", "Season.4", "Species.4", "trend.4")
  a <- wind2 |> dplyr::filter(Area == "Existing-North")
  b <- wind2 |>
    dplyr::filter(Area == "Proposed-North") |>
    dplyr::rename_at(dplyr::vars(names), ~bnew)
  c <- wind2 |>
    dplyr::filter(Area == "Existing-Mid") |>
    dplyr::rename_at(dplyr::vars(names), ~cnew)
  d <- wind2 |>
    dplyr::filter(Area == "Proposed-Mid") |>
    dplyr::rename_at(dplyr::vars(names), ~dnew)
  e <- wind2 |>
    dplyr::filter(Area == "Existing-South") |>
    dplyr::rename_at(dplyr::vars(names), ~enew)
  all <- a |> cbind(b, c, d, e) |> dplyr::select(2:4, 7:8, 11:12, 15:16, 19:20)

  p <- flextable::flextable(all) |>
    flextable::set_caption(
      caption = flextable::as_paragraph(flextable::as_b(
        'Species with highest probability of occupancy species each season and area, with observed trends'
      ))
    ) |>
    flextable::add_header_row(
      values = c(
        "",
        "Existing - North",
        "Proposed - North",
        "Existing - Mid",
        "Proposed - Mid",
        "Existing - South"
      ),
      colwidths = c(1, 2, 2, 2, 2, 2)
    ) |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::set_header_labels(
      trend = "Trend",
      trend.1 = "Trend",
      trend.2 = "Trend",
      trend.3 = "Trend",
      trend.4 = "Trend",
      Species.1 = "Species",
      Species.2 = "Species",
      Species.3 = "Species",
      Species.4 = "Species"
    ) |>
    flextable::fontsize(size = 20, j = c(3, 5, 7, 9, 11), part = "body") |>
    flextable::bold(part = "header") |>
    flextable::align(i = 1, align = "center", part = "header")

  return(p)
}

attr(plot_wind_occupancy, "report") <- c("MidAtlantic", "NewEngland")
