#' plot aggregate_biomass
#'
#' Plots faceted spring and fall survey biomass time series by aggregate group
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_aggregate_biomass <- function(
    shadedRegion = NULL,
    report = "MidAtlantic",
    EPU = "MAB",
    n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB", "GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # this code from NE reports, works for Mid but includes Spring 2020
  agg <- ecodata::aggregate_biomass |>
    dplyr::mutate(EPU = factor(EPU)) |>
    dplyr::filter(
      !stringr::str_detect(
        Var,
        "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA"
      )
    ) |> #remove unused datasets
    tidyr::separate(
      Var,
      c("feeding.guild", "season", "Biomass", "Var1"),
      sep = " "
    ) |>
    tidyr::unite("Var", feeding.guild:season, sep = " ") |>
    dplyr::mutate(
      stat = dplyr::recode(Var1, Index = "Mean", Standard = "SD")
    ) |>
    dplyr::select(-Biomass, -Var1) |>
    dplyr::group_by(Var, Time, EPU) |>
    tidyr::spread(stat, Value) |>
    dplyr::mutate(upper = Mean + (2 * SD), lower = Mean - (2 * SD))

  agg_bio <- agg |>
    dplyr::filter(EPU == filterEPUs, Time >= 1968) |>
    dplyr::group_by(Var, EPU) |>
    dplyr::mutate(hline = mean(Mean, na.rm = T)) |>
    dplyr::ungroup()

  agg_bio$Var <- factor(
    agg_bio$Var,
    levels = c(
      "Piscivore Spring",
      "Piscivore Fall",
      "Benthivore Spring",
      "Benthivore Fall",
      "Planktivore Spring",
      "Planktivore Fall",
      "Benthos Spring",
      "Benthos Fall"
    )
  )

  series.col <- rep("black", length(unique(agg_bio$Var)))

  facet_names <- list(
    "Piscivores" = expression("Piscivores"),
    "Planktivores" = expression("Planktivores"),
    "Benthivores" = expression("Benthivores"),
    "Benthos" = expression("Benthos")
  )

  if (report == "MidAtlantic") {
    #Get NEAMAP
    neamap <- ecodata::mab_inshore_survey |>
      tidyr::separate(Var, into = c("Var", "Val"), sep = "-") |>
      tidyr::pivot_wider(names_from = Val, values_from = Value) |>
      dplyr::mutate(Value = as.numeric(Value), CV = as.numeric(CV)) |>
      dplyr::group_by(Var) |>
      dplyr::mutate(
        hline = mean(Value),
        SD = Value * CV, #calculate SD from CV
        upper = Value + (2 * SD),
        lower = Value - (2 * SD)
      )

    neamap$Var <- factor(
      neamap$Var,
      levels = c(
        "Piscivore Spring",
        "Piscivore Fall",
        "Benthivore Spring",
        "Benthivore Fall",
        "Planktivore Spring",
        "Planktivore Fall",
        "Benthos Spring",
        "Benthos Fall"
      )
    )
    neamap.1 <- neamap |>
      dplyr::filter(stringr::str_detect(Var, "Piscivore"))
    neamap.2 <- neamap |>
      dplyr::filter(stringr::str_detect(Var, "Benthivore"))
    neamap.3 <- neamap |>
      dplyr::filter(stringr::str_detect(Var, "Planktivore"))
    neamap.4 <- neamap |>
      dplyr::filter(stringr::str_detect(Var, "Benthos"))
  }

  # calculate max y values for each guild
  piscivore <- agg_bio |>
    dplyr::filter(Var == "Piscivore Spring" | Var == "Piscivore Fall")
  piscivore_new_max <- max((piscivore$Mean) * 1.2, na.rm = TRUE)

  benthivore <- agg_bio |>
    dplyr::filter(Var == "Benthivore Spring" | Var == "Benthivore Fall")
  benthivore_new_max <- max((neamap.2$Value) * 1.2, na.rm = TRUE)

  planktivore <- agg_bio |>
    dplyr::filter(Var == "Planktivore Spring" | Var == "Planktivore Fall")
  planktivore_new_max <- max((neamap.3$Value) * 1.2, na.rm = TRUE)

  benthos <- agg_bio |>
    dplyr::filter(Var == "Benthos Spring" | Var == "Benthos Fall")
  benthos_new_max <- max((benthos$Mean) * 1.2, na.rm = TRUE)


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  # Build each group plot, assemble below
  ## Piscivore
  p1 <- agg_bio |>
    dplyr::filter(stringr::str_detect(Var, "Piscivore")) |>
    ggplot2::ggplot() +
    #Highlight last ten years
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    #Test for trend and add lines
    ecodata::geom_gls(
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    ecodata::geom_lm(
      n = n,
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
      alpha = 0.5,
      fill = "grey"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = Time, y = Mean),
      linewidth = setup$lwd - 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Time, y = Mean),
      size = setup$pcex - 0.5,
      na.rm = T
    ) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color") +
    ggplot2::guides(color = "none") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::facet_wrap(Var ~ ., ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(
      breaks = seq(1970, 2020, by = 10),
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, piscivore_new_max),
      oob = scales::oob_keep
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::ylab(ggplot2::element_blank()) +
    ecodata::theme_facet() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0, size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 25)
    )

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p1 <- p1 +
      ggplot2::geom_ribbon(
        data = neamap.1,
        ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
        alpha = 0.5,
        fill = "pink"
      ) +
      ggplot2::geom_line(
        data = neamap.1,
        ggplot2::aes(x = Time, y = Value),
        color = "#ca0020"
      ) +
      ggplot2::geom_point(
        data = neamap.1,
        ggplot2::aes(x = Time, y = Value),
        na.rm = T,
        size = setup$pcex - 0.5,
        color = "#ca0020"
      )
  }

  ## Benthivore
  p2 <- agg_bio |>
    dplyr::filter(stringr::str_detect(Var, "Benthivore")) |>
    ggplot2::ggplot() +

    #Highlight last ten years
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    #Test for trend and add lines
    ecodata::geom_gls(
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    ecodata::geom_lm(
      n = n,
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
      alpha = 0.5,
      fill = "grey"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = Time, y = Mean),
      linewidth = setup$lwd - 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Time, y = Mean),
      size = setup$pcex - 0.5,
      na.rm = T
    ) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color") +
    ggplot2::guides(color = "none") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::facet_wrap(Var ~ ., ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(
      breaks = seq(1970, 2020, by = 10),
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, benthivore_new_max),
      oob = scales::oob_keep
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::ylab(ggplot2::element_blank()) +
    ecodata::theme_facet() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0, size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 25)
    )

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p2 <- p2 +
      ggplot2::geom_ribbon(
        data = neamap.2,
        ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
        alpha = 0.5,
        fill = "pink"
      ) +
      ggplot2::geom_line(
        data = neamap.2,
        ggplot2::aes(x = Time, y = Value),
        color = "#ca0020"
      ) +
      ggplot2::geom_point(
        data = neamap.2,
        ggplot2::aes(x = Time, y = Value),
        size = setup$pcex - 0.5,
        color = "#ca0020",
        na.rm = T
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, benthivore_neamap_new_max),
        oob = scales::oob_keep
      )
  }

  ### Planktivore
  p3 <- agg_bio |>
    dplyr::filter(stringr::str_detect(Var, "Planktivore")) |>
    dplyr::mutate(Mean = dplyr::case_when(Mean > 80 ~ 90, TRUE ~ Mean)) |>

    ggplot2::ggplot() +

    #Highlight last ten years
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    #Test for trend and add lines
    # bespoke geom just for planktivores since nmle package could not fit
    # models with ar1 errors
    ecodata::geom_gls(
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    ecodata::geom_lm(
      n = n,
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
      alpha = 0.5,
      fill = "grey"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = Time, y = Mean),
      linewidth = setup$lwd - 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Time, y = Mean),
      size = setup$pcex - 0.5,
      na.rm = T
    ) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color") +
    ggplot2::guides(color = "none") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::facet_wrap(Var ~ ., ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(
      breaks = seq(1970, 2020, by = 10),
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, planktivore_new_max),
      oob = scales::oob_keep
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::ylab(ggplot2::element_blank()) +
    ecodata::theme_facet() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0, size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 25)
    )

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p3 <- p3 +
      ggplot2::geom_ribbon(
        data = neamap.3,
        ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
        alpha = 0.5,
        fill = "pink"
      ) +
      ggplot2::geom_line(
        data = neamap.3,
        ggplot2::aes(x = Time, y = Value),
        color = "#ca0020"
      ) +
      ggplot2::geom_point(
        data = neamap.3,
        ggplot2::aes(x = Time, y = Value),
        size = setup$pcex - 0.5,
        color = "#ca0020",
        na.rm = T
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, planktivore_neamap_new_max),
        oob = scales::oob_keep
      )
  }

  ### Benthos
  p4 <- agg_bio |>
    dplyr::filter(stringr::str_detect(Var, "Benthos")) |>
    #ggplot(aes(x = Time, y = Mean)) +
    ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +

    #Highlight last ten years
    #Test for trend and add lines
    ecodata::geom_gls(
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +
    ecodata::geom_lm(
      n = n,
      ggplot2::aes(x = Time, y = Mean, color = Var),
      alpha = setup$trend.alpha,
      size = setup$trend.size,
      na.rm = T
    ) +

    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
      alpha = 0.5,
      fill = "grey"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = Time, y = Mean),
      linewidth = setup$lwd - 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Time, y = Mean),
      size = setup$pcex - 0.5,
      na.rm = T
    ) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color") +
    ggplot2::guides(color = "none") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::facet_wrap(Var ~ ., ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(
      breaks = seq(1970, 2020, by = 10),
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, benthos_new_max),
      oob = scales::oob_keep
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::ylab(ggplot2::element_blank()) +
    ecodata::theme_facet() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0, size = 12),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 25)
    )

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p4 <- p4 +
      ggplot2::geom_ribbon(
        data = neamap.4,
        ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
        alpha = 0.5,
        fill = "pink"
      ) +
      ggplot2::geom_line(
        data = neamap.4,
        ggplot2::aes(x = Time, y = Value),
        color = "#ca0020"
      ) +
      ggplot2::geom_point(
        data = neamap.4,
        ggplot2::aes(x = Time, y = Value),
        size = setup$pcex - 0.5,
        color = "#ca0020",
        na.rm = T
      )
  }

  p <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 4)

  p <- ggpubr::annotate_figure(
    p,
    left = ggpubr::text_grob("Biomass (kg tow ^-1)", rot = 90, size = 16)
  )

  return(p)
}

# plot_aggregate_biomass()
# plot_aggregate_biomass(report = "NewEngland", EPU = "GB")
# plot_aggregate_biomass(report = "NewEngland", EPU = "GOM")

attr(plot_aggregate_biomass, "EPU") <- c("MAB", "GB", "GOM")
attr(plot_aggregate_biomass, "report") <- c("MidAtlantic", "NewEngland")
