#' plots fall thermal habitat suitability
#'
#' Plots the percentage of each managed species' historic fall habitat
#' footprint where daily bottom temperature (September-November) fell within
#' the species' thermal niche, as a species x year heatmap, or species
#' anomalies with the mean across managed species.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param plottype Character string. Type of figure ("scaled" heatmap of percent
#'   suitable scaled within species and binned by quantiles, "raw" heatmap of
#'   percent suitable, or "spaghetti" species anomalies with the regional mean)
#' @param include_asmfc Logical. Include species managed by the Atlantic States
#'   Marine Fisheries Commission (default FALSE: federally managed species only)
#' @param data_file Character string. Path to the indicator file exported by
#'   READ-EDAB-enviroThresholds (05_export_soe.R). Defaults to the SOE 2027
#'   indicator folder on the NEFSC network drive.
#'
#' @return ggplot object
#'
#' @export
#'

plot_thermal_suitability <- function(
    shadedRegion = NULL,
    report = "MidAtlantic",
    plottype = "scaled",
    include_asmfc = FALSE,
    data_file = "//nefscdata/EDAB_Indicators/SOE_ecodata/SOE_2027/thermal_habitat_soe.rds"
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)
  
  # which report? Data are reported by Council region, not EPU
  if (report == "MidAtlantic") {
    filterEPUs <- "MA"
    regionName <- "the Mid-Atlantic"
  } else if (report == "NewEngland") {
    filterEPUs <- "NE"
    regionName <- "New England"
  } else {
    stop("report must be either 'MidAtlantic' or 'NewEngland'")
  }
  
  if (!(plottype %in% c("scaled", "raw", "spaghetti"))) {
    stop("plottype must be 'scaled', 'raw', or 'spaghetti'")
  }
  
  # species jurisdictions to show, and the matching regional mean
  jurisdictions <- c("NEFMC", "MAFMC", "JOINT")
  meanGroup <- "Federal"
  titleSuffix <- ""
  if (include_asmfc) {
    jurisdictions <- c(jurisdictions, "ASMFC")
    meanGroup <- "All"
    titleSuffix <- " (incl. ASMFC)"
  }
  
  # read the indicator directly from the network drive (not stored in ecodata)
  if (!file.exists(data_file)) {
    stop(
      "Thermal suitability data not found at: ", data_file,
      "\nCheck the network drive connection or supply data_file."
    )
  }
  thermal_suitability <- readRDS(data_file)
  
  # Var is "<Species> - <metric>"; split into species and metric
  fix <- thermal_suitability |>
    dplyr::filter(EPU == filterEPUs) |>
    dplyr::mutate(
      Species = sub(" - [^-]*$", "", Var),
      Metric = sub("^.* - ", "", Var)
    )
  
  speciesData <- fix |>
    dplyr::filter(Jurisdiction %in% jurisdictions)
  
  numberOfConditions <- 5
  
  if (plottype %in% c("scaled", "raw")) {
    heat <- speciesData |>
      dplyr::filter(Metric == "Percent suitable") |>
      dplyr::group_by(Species)
    
    if (plottype == "scaled") {
      heat <- heat |>
        dplyr::mutate(scaleVal = as.numeric(scale(Value, scale = TRUE, center = TRUE)))
    } else {
      heat <- heat |>
        dplyr::mutate(scaleVal = Value)
    }
    heat <- dplyr::ungroup(heat)
    
    # finds quantiles
    xs <- quantile(heat$scaleVal, seq(0, 1, length.out = numberOfConditions + 1), na.rm = TRUE)
    
    heat <- heat |>
      dplyr::mutate(
        category = cut(
          scaleVal,
          breaks = xs,
          labels = c(
            "Low Suitability",
            "Below Average",
            "Neutral",
            "Above Average",
            "High Suitability"
          ),
          include.lowest = TRUE
        )
      )
    
    # order species by mean percent suitable (scaled means are all ~0)
    sortNames <- heat |>
      dplyr::group_by(Species) |>
      dplyr::summarize(total = mean(Value, na.rm = TRUE)) |>
      dplyr::arrange(total) |>
      dplyr::pull(Species)
    
    heat$Species <- factor(heat$Species, levels = sortNames)
    
    # 5 scale colors for viridis
    vir <- viridis::viridis_pal()(numberOfConditions)
    
    if (plottype == "scaled") {
      p <- heat |>
        ggplot2::ggplot(ggplot2::aes(
          x = Time,
          y = forcats::fct_rev(Species),
          fill = category
        )) +
        ggplot2::scale_fill_manual(values = vir, drop = FALSE) +
        ggplot2::labs(fill = "Quantiles of\nSuitable Habitat")
    } else {
      p <- heat |>
        ggplot2::ggplot(ggplot2::aes(
          x = Time,
          y = forcats::fct_rev(Species),
          fill = scaleVal
        )) +
        ggplot2::scale_fill_gradientn(
          colors = c(viridis::rocket(4)[2:4], viridis::mako(4)[4:2]),
          values = scales::rescale(xs, from = c(min(xs), max(xs)), to = c(0, 1))
        ) +
        ggplot2::labs(fill = "Suitable\nHabitat (%)")
    }
    
    p <- p +
      ggplot2::geom_tile() +
      ggplot2::coord_equal() +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(
        breaks = round(seq(min(heat$Time), max(heat$Time), by = numberOfConditions))
      ) +
      ggplot2::theme(
        legend.position = "right",
        legend.box = "vertical",
        legend.title = ggplot2::element_text(size = 8),
        legend.text = ggplot2::element_text(size = 6),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 6),
        axis.text.y = ggplot2::element_text(size = 6),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ggtitle(paste0(
        "Fall thermal habitat suitability for species managed in ",
        regionName,
        titleSuffix
      )) +
      ggplot2::ylab(NULL) +
      ggplot2::xlab(NULL) +
      ecodata::theme_ts() +
      ecodata::theme_title()
  } else {
    # spaghetti: species anomalies (grey) and mean of managed species (bold)
    anom <- speciesData |>
      dplyr::filter(Metric == "Anomaly")
    
    meanLine <- fix |>
      dplyr::filter(Jurisdiction == meanGroup)
    
    p <- anom |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
      ggplot2::annotate(
        "rect",
        fill = setup$shade.fill,
        alpha = setup$shade.alpha,
        xmin = setup$x.shade.min,
        xmax = setup$x.shade.max,
        ymin = -Inf,
        ymax = Inf
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(group = Species), colour = "grey70", linewidth = 0.3) +
      ggplot2::geom_line(data = meanLine, colour = "black", linewidth = 1.1) +
      ecodata::geom_gls(data = meanLine, ggplot2::aes(x = Time, y = Value)) +
      ggplot2::ylab("Anomaly (percentage points)") +
      ggplot2::xlab(NULL) +
      ggplot2::ggtitle(paste0(
        "Fall thermal habitat anomaly, species managed in ",
        regionName,
        titleSuffix
      )) +
      ecodata::theme_ts() +
      ecodata::theme_title()
  }
  
  return(p)
}

attr(plot_thermal_suitability, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_thermal_suitability, "plottype") <- c("scaled", "raw", "spaghetti")
attr(plot_thermal_suitability, "include_asmfc") <- c(FALSE, TRUE)