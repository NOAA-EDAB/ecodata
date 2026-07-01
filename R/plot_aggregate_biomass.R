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
  end_year <- max(ecodata::aggregate_biomass$Time)
  # this code from NE reports, works for Mid but includes Spring 2020
  agg <- ecodata::aggregate_biomass |>
    dplyr::mutate(EPU = factor(EPU)) |>
    dplyr::filter(
      EPU %in% filterEPUs,
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
    dplyr::mutate(
      upper = Mean + (2 * SD),
      lower = Mean - (2 * SD)
    )

  agg_bio <- agg |>
    dplyr::filter(EPU == filterEPUs, Time >= 1968) |>
    dplyr::group_by(Var, EPU) |>
    dplyr::mutate(hline = mean(Mean, na.rm = T), plt_max = hline * 1.2) |>
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

  if (report == "MidAtlantic") {
    #Get NEAMAP
    neamap <- ecodata::mab_inshore_survey |>
      tidyr::separate(Var, into = c("Var", "Val"), sep = "-") |>
      tidyr::pivot_wider(names_from = Val, values_from = Value) |>
      dplyr::mutate(Value = as.numeric(Value), CV = as.numeric(CV)) |>
      dplyr::group_by(Var) |>
      dplyr::mutate(
        hline = mean(Value),
        plt_max = hline * 1.2,
        SD = Value * CV, #calculate SD from CV
        upper = Value + (2 * SD),
        lower = Value - (2 * SD)
      ) |>
      dplyr::rename(Mean = Value)

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

    plt_data <- dplyr::bind_rows(
      agg_bio |> dplyr::mutate(source = "NEFSC"),
      neamap |> dplyr::mutate(source = "NEAMAP")
    ) |>
      # drop "other" that is now NA
      tidyr::drop_na(Var) |>
      dplyr::group_by(Var, Time) |>
      dplyr::mutate(real_plt_max = max(plt_max))

    ## plot ----
    p <- plt_data |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Mean, color = source)) +
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
      #Add time series
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = Time,
          ymin = pmax(lower, 0),
          ymax = upper,
          fill = source
        ),
        alpha = 0.2,
        color = NA
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline, group = Var, color = source),
        linewidth = setup$hline.size,
        alpha = setup$hline.alpha,
        linetype = setup$hline.lty
      ) +
      ggplot2::geom_line(
        inherit.aes = TRUE,
        linewidth = setup$lwd - 0.5
      ) +
      ggplot2::geom_point(
        inherit.aes = TRUE,
        size = setup$pcex - 0.5,
        na.rm = T
      ) +
      #Test for trend and add lines
      ecodata::geom_gls(
        ggplot2::aes(x = Time, y = Mean),
        alpha = setup$trend.alpha,
        size = setup$trend.size,
        na.rm = T
      ) +
      ecodata::geom_lm(
        n = n,
        ggplot2::aes(x = Time, y = Mean),
        alpha = setup$trend.alpha,
        size = setup$trend.size,
        na.rm = T
      ) +
      ggplot2::scale_color_manual(values = c("red", "black")) +
      ggplot2::scale_fill_manual(values = c("red", "black")) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title = ggplot2::element_blank()),
        fill = ggplot2::guide_legend(title = ggplot2::element_blank())
      ) +

      ggplot2::facet_wrap(~Var, ncol = 2, scales = "free") +
      #Axis and theme
      ggplot2::scale_x_continuous(
        breaks = seq(1970, end_year, by = 10),
        expand = c(0.01, 0.01)
      ) +
      ggplot2::scale_y_continuous(
        oob = scales::oob_keep
      ) +
      ggplot2::coord_cartesian(clip = "on") +
      ggplot2::ylab(ggplot2::element_blank()) +
      ecodata::theme_facet() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(hjust = 0, size = 12),
        axis.title.x = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(angle = 25),
        legend.position = "bottom"
      ) +
      ggplot2::ylab("Biomass (kg tow ^-1)")
  } else if (report == "NewEngland") {
    p <- agg_bio |>
      tidyr::drop_na(Var) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Mean)) +
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
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = Time,
          ymin = pmax(lower, 0),
          ymax = upper
        ),
        fill = "grey",
        alpha = 0.5,
        color = NA
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline, group = Var),
        linewidth = setup$hline.size,
        alpha = setup$hline.alpha,
        linetype = setup$hline.lty
      ) +
      ggplot2::geom_line(
        inherit.aes = TRUE,
        linewidth = setup$lwd - 0.5
      ) +
      ggplot2::geom_point(
        inherit.aes = TRUE,
        size = setup$pcex - 0.5,
        na.rm = T
      ) +
      #Test for trend and add lines
      ecodata::geom_gls(
        inherit.aes = TRUE,
        alpha = setup$trend.alpha,
        size = setup$trend.size,
        na.rm = T
      ) +
      ecodata::geom_lm(
        n = n,
        inherit.aes = TRUE,
        alpha = setup$trend.alpha,
        size = setup$trend.size,
        na.rm = T
      ) +
      ggplot2::facet_wrap(~Var, ncol = 2, scales = "free") +
      #Axis and theme
      ggplot2::scale_x_continuous(
        breaks = seq(1970, end_year, by = 10),
        expand = c(0.01, 0.01)
      ) +
      ggplot2::scale_y_continuous(
        oob = scales::oob_keep
      ) +
      ggplot2::coord_cartesian(clip = "on") +
      ggplot2::ylab(ggplot2::element_blank()) +
      ecodata::theme_facet() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(hjust = 0, size = 12),
        axis.title.x = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(angle = 25),
        legend.position = "bottom"
      ) +
      ggplot2::ylab("Biomass (kg tow ^-1)")
  }

  return(p)
}

#plot_aggregate_biomass()
# plot_aggregate_biomass(report = "NewEngland", EPU = "GB")
# plot_aggregate_biomass(report = "NewEngland", EPU = "GOM")

attr(plot_aggregate_biomass, "EPU") <- c("MAB", "GB", "GOM")
attr(plot_aggregate_biomass, "report") <- c("MidAtlantic", "NewEngland")
