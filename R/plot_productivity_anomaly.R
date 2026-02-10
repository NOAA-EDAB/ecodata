#' plot productivity_anomaly
#'
#' plots the productivity anomaly dataset
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("anomaly","assessment")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#' @param plottype Character string. "region" (legacy) or "council" (plot species managed by corresponding council)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_productivity_anomaly <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  plottype = "region",
  varName = "anomaly",
  EPU = "MAB"
) {
  # Need to generalize this in a function and add validation checks for
  # arguments in all functions
  # Input validation
  if (!report %in% c("MidAtlantic", "NewEngland")) {
    stop("report must be 'MidAtlantic' or 'NewEngland'")
  }

  if (!plottype %in% c("region", "council")) {
    stop("plottype must be 'region' or 'council'")
  }

  if (!varName %in% c("anomaly", "assessment")) {
    stop("varName must be 'anomaly' or 'assessment'")
  }

  if (varName == "assessment") {
    message("EPU is ignored for assessment plots.")
  }

  # EPUs are only filtered for region/anomaly
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- EPU
  }
  # for council/ anomaly all EPUs used
  # for region/assessment plots all epus are ignored. jurisdiction is used

  if (varName == "anomaly") {
    if (
      report == "MidAtlantic" && plottype == "region" && !identical(EPU, "MAB")
    ) {
      stop("MidAtlantic region anomaly plots require EPU = 'MAB'")
    }

    if (
      report == "NewEngland" &&
        plottype == "region" &&
        (!EPU %in% c("GB", "GOM"))
    ) {
      stop("NewEngland region anomaly plots require EPU = 'GB' or 'GOM'")
    }

    if (plottype == "council") {
      filterEPUs <- "All"
      message("EPU argument ignored. Council anomaly plots use all EPU data")
    }
  }

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # this should be added to plot_setip
  leg_font_size <- 8

  # base data object
  prod_dat <- ecodata::productivity_anomaly

  # council-level filtering
  if (plottype == "council") {
    if (report == "MidAtlantic") {
      prod_dat <- prod_dat |>
        dplyr::filter(Jurisdiction %in% c("MAFMC", "JOINT"))
    } else if (report == "NewEngland") {
      prod_dat <- prod_dat |>
        dplyr::filter(Jurisdiction %in% c("NEFMC", "JOINT"))
    } else {
      stop("Invalid report value")
    }
  }

  # determine EPU filtering logic
  # filterEPUs <- NULL

  # if (varName == "anomaly") {
  #   if (plottype == "region") {
  #     if (report == "MidAtlantic") {
  #       filterEPUs <- "MAB"
  #     } else if (report == "NewEngland") {
  #       if (!(EPU %in% c("GB", "GOM"))) {
  #         stop("Invalid EPU. For NewEngland use 'GB' or 'GOM'")
  #       }
  #
  #       filterEPUs <- EPU
  #     }
  #   } else if (plottype == "council") {
  #     # council-level anomaly plots should use EPU == "All"
  #     filterEPUs <- "All"
  #   }
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  if (varName == "assessment") {
    fix <- prod_dat |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-") |>
      dplyr::filter(Var == "rs_anom") |>
      dplyr::group_by(Time) |>
      dplyr::summarise(
        Total = sum(Value, na.rm = TRUE),
        Count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Total = ifelse(Count < max(Count), NA, Total)
      ) |>
      tidyr::complete(
        Time = seq(min(Time), max(Time), by = 1),
        fill = list(Total = NA)
      )

    prod <- prod_dat |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-") |>
      dplyr::filter(Var == "rs_anom") |>
      dplyr::mutate(Stock = toupper(Stock))

    # Title + subtitle logic for assessment plots

    prefix <- if (report == "MidAtlantic") "MA" else "NE"

    subtitle_text <- NULL
    if (plottype == "council") {
      subtitle_text <- if (report == "MidAtlantic") {
        "MAFMC managed species"
      } else {
        "NEFMC managed species"
      }
    }

    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #
    p <-
      ggplot2::ggplot(prod, ggplot2::aes(x = Time)) +
      ggplot2::geom_bar(
        data = prod |> dplyr::filter(Value > 0),
        ggplot2::aes(y = Value, fill = Stock),
        stat = "identity"
      ) +
      ggplot2::geom_bar(
        data = prod |> dplyr::filter(Value < 0),
        ggplot2::aes(y = Value, fill = Stock),
        stat = "identity"
      ) +
      ggplot2::geom_line(
        data = fix,
        ggplot2::aes(x = Time, y = Total),
        linewidth = 1
      ) +
      ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
      ggplot2::xlab("") +
      ggplot2::ylab("Recruitment Anomaly") +
      ggplot2::labs(
        title = paste0(prefix, " Recruitment Anomaly from Stock Assessments"),
        subtitle = subtitle_text
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3)) +
      ecodata::theme_ts() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = setup$label.size * 2.4),
        axis.text = ggplot2::element_text(size = setup$label.size * 2.4),
        plot.title = ggplot2::element_text(size = setup$label.size * 2.8),
        #legend.text  = element_text(size = leg_font_size),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = setup$label.size * 1.8),
        legend.position = "bottom"
      )

    if (report == "NewEngland") {
      p <- p +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            ncol = 4
          )
        )
    } else {
      p <- p
    }
  }

  if (varName == "anomaly") {
    bar_dat <- prod_dat |>
      dplyr::filter(grepl('_Survey', Var))

    if (!is.null(filterEPUs)) {
      bar_dat <- bar_dat |>
        dplyr::filter(EPU == filterEPUs)
    }

    bar_dat <- bar_dat |>
      tidyr::separate(Var, into = c("Var", "Survey"), sep = "_") |>
      dplyr::mutate(
        Var = gsub("^NM LME\\s+", "", Var)
      )

    p <- plot_stackbarcpts_single(
      YEAR = bar_dat$Time,
      var2bar = bar_dat$Var,
      x = bar_dat$Value,
      titl = if (plottype == "council") {
        paste0(report, " council from survey data")
      } else {
        paste0(EPU, " from survey data")
      },
      xlab = "",
      ylab = "Small fish per large fish biomass (anomaly)",
      height = 5.5,
      width = 9,
      filt = FALSE,
      leg_font_size = leg_font_size,
      label = "",
      y.text = 10,
      aggregate = TRUE
    )
    if (report == "NewEngland") {
      p <- p +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            ncol = 4
          )
        )
    } else {
      p <- p
    }
  }

  if (varName == "assessment" && EPU == "GOM") {
    p <- "Assessment variable includes GB and GOM. See plot for EPU = GB."
  }

  return(p)
}


attr(plot_productivity_anomaly, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_productivity_anomaly, "varName") <- c("anomaly", "assessment")
attr(plot_productivity_anomaly, "EPU") <- c("MAB", "GB", "GOM")
attr(plot_productivity_anomaly, "plottype") <- c("region", "council")


#' anomaly stacked barchart. needs to be reworked
#' @noRd
plot_stackbarcpts_single <- function(
  YEAR,
  var2bar,
  x,
  xlab,
  ylab,
  titl,
  file_suffix,
  leg_font_size = leg_font_size,
  remove_leg = FALSE,
  leg_ncol = 3,
  wcpts = TRUE,
  wdashed = TRUE,
  height = 5.5,
  width = 8,
  filt = TRUE,
  label = label,
  y.text = y.text,
  aggregate = FALSE
) {
  dat2bar <- data.frame(YEAR, var2bar, x)

  if (filt == TRUE) {
    mab_species <- list(
      "SUMMER FLOUNDER",
      "SCUP",
      "BLACK SEA BASS",
      "BLUEFISH",
      "NORTHERN SHORTFIN SQUID",
      "LONGFIN SQUID",
      "ATLANTIC MACKEREL",
      "BUTTERFISH",
      "ATLANTIC SURFCLAM",
      "OCEAN QUAHOG",
      "TILEFISH",
      "BLUELINE TILEFISH",
      "SPINY DOGFISH",
      "GOOSEFISH"
    )
    dat2plot <- dat2bar |>
      tidyr::gather(variable, value, -YEAR, -var2bar) |>
      dplyr::mutate(
        var2bar = gsub("^[A-Z]{2}\\s+LME\\s+", "", var2bar),
        var2bar = gsub("_", " ", var2bar),
        var2bar = gsub(pattern = "Atl.", replacement = "ATLANTIC", x = var2bar),
        var2bar = gsub(pattern = "Atl", replacement = "ATLANTIC", x = var2bar),
        var2bar = gsub(
          pattern = "NS and combined",
          replacement = "",
          x = var2bar
        ),
        var2bar = gsub(pattern = "YT", replacement = "Yellowtail", x = var2bar),
        var2bar = gsub(pattern = " GoM", replacement = " GOM", x = var2bar),
        var2bar = gsub(pattern = " by EPU", replacement = "", x = var2bar)
      ) |>
      dplyr::filter(var2bar %in% mab_species)
  } else if (filt == FALSE) {
    dat2plot <- dat2bar |>
      tidyr::gather(variable, value, -YEAR, -var2bar) |>
      dplyr::mutate(
        var2bar = gsub("^[A-Z]{2}\\s+LME\\s+", "", var2bar),
        var2bar = gsub("_", " ", var2bar),

        var2bar = gsub(pattern = "Atl.", replacement = "ATLANTIC", x = var2bar),
        var2bar = gsub(pattern = "Atl", replacement = "ATLANTIC", x = var2bar),
        var2bar = gsub(
          pattern = "NS and combined",
          replacement = "",
          x = var2bar
        ),
        var2bar = gsub(pattern = "YT", replacement = "Yellowtail", x = var2bar),
        var2bar = gsub(pattern = " GoM", replacement = " GOM", x = var2bar),
        var2bar = gsub(pattern = " by EPU", replacement = "", x = var2bar)
      )
  }
  if (aggregate) {
    agg <- dat2plot |>
      dplyr::mutate(
        value = ifelse(is.nan(value), NA_real_, value)
      ) |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(
        Total = sum(value, na.rm = TRUE),
        Count = sum(!is.na(value)),
        .groups = "drop"
      )

    # maximum number of species ever observed in a single year
    max_count <- max(agg$Count, na.rm = TRUE)

    # allow line when >= 75% of species are present
    threshold <- 0.75 * max_count

    agg <- agg |>
      dplyr::mutate(
        Total = ifelse(Count < threshold, NA_real_, Total)
      ) |>
      tidyr::complete(
        YEAR = seq(min(YEAR), max(YEAR), by = 1),
        fill = list(Total = NA_real_)
      )
  }

  p <-
    ggplot2::ggplot(dat2plot, ggplot2::aes(x = YEAR)) +
    ggplot2::geom_bar(
      data = dat2plot |> dplyr::filter(value > 0),
      ggplot2::aes(y = value, fill = var2bar),
      stat = "identity"
    ) +
    ggplot2::geom_bar(
      data = dat2plot |> dplyr::filter(value < 0),
      ggplot2::aes(y = value, fill = var2bar),
      stat = "identity"
    ) +
    {
      if (aggregate) {
        ggplot2::geom_line(
          data = agg,
          ggplot2::aes(x = YEAR, y = Total),
          linewidth = 1
        )
      }
    } +
    ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(titl) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = leg_font_size),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::annotate(
      "text",
      label = label,
      x = 1980,
      y = y.text,
      size = 8,
      colour = "black"
    )

  if (remove_leg) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}
