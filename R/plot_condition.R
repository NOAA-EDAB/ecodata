#' plots Fish Condition
#'
#' plots the condition data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU in the report ("GB", "GOM", "MAB")
#' @param plottype Character string. How categories should be assigned ("scaled" rescales and assigns by quantiles, while "raw" assigns based on relative condition)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_condition <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB",
  plottype = "scaled"
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

  numberOfConditions <- 5

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  if (plottype == "scaled") {
    fix <- ecodata::condition |>
      dplyr::filter(EPU == filterEPUs) |>
      dplyr::group_by(Var) |>
      dplyr::mutate(scaleCond = scale(Value, scale = T, center = T))
  } else if (plottype == "raw") {
    fix <- ecodata::condition |>
      dplyr::filter(EPU == filterEPUs) |>
      dplyr::group_by(Var) |>
      dplyr::mutate(scaleCond = Value)
  } else {
    stop("plottype must be either 'scaled' or 'raw'")
  }

  # finds quantiles
  xs <- quantile(fix$scaleCond, seq(0, 1, length.out = 6), na.rm = TRUE)

  fix <- fix |>
    dplyr::mutate(
      category = cut(
        scaleCond,
        breaks = xs,
        labels = c(
          "Poor Condition",
          "Below Average",
          "Neutral",
          "Above Average",
          "Good Condition"
        ),
        include.lowest = TRUE
      )
    )

  sortNames <- fix |>
    dplyr::filter(Time <= 2014) |>
    dplyr::group_by(Var) |>
    dplyr::summarize(total = sum(scaleCond)) |>
    dplyr::arrange(total) |>
    dplyr::mutate(Species = factor(Var, levels = unique(Var))) |>
    dplyr::pull(Species)

  # fix$Var <- factor(fix$Var, levels = sortNames)

  #See 5 scale colors for viridis:
  vir <- viridis::viridis_pal()(numberOfConditions)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  if (plottype == "scaled") {
    p <- fix |>
      ggplot2::ggplot(ggplot2::aes(
        x = Time,
        y = forcats::fct_rev(Var),
        fill = category
      )) +
      ggplot2::scale_fill_manual(values = vir)
  } else if (plottype == "raw") {
    p <- fix |>
      ggplot2::ggplot(ggplot2::aes(
        x = Time,
        y = forcats::fct_rev(Var),
        fill = scaleCond
      )) +
      # viridis::scale_fill_viridis(
      #   discrete = FALSE #,
      #   # option = "turbo",
      #   # direction = -1
      # )
      ggplot2::scale_fill_gradientn(
        colors = c(viridis::rocket(4)[2:4], viridis::mako(4)[4:2]),
        # limits = c(min(fix$scaleCond), max(fix$scaleCond)),
        values = scales::rescale(xs, from = c(min(xs), max(xs)), to = c(0, 1))
      )
  }

  p <- p +
    ggplot2::geom_tile() +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    # ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +

    ggplot2::scale_x_continuous(
      breaks = round(seq(min(fix$Time), max(fix$Time), by = numberOfConditions))
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
      "Relative condition for species sampled in ",
      EPU
    )) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank()) +
    ecodata::theme_ts() +
    ecodata::theme_title()

  if (plottype == "scaled") {
    p <- p +
      ggplot2::labs(fill = "Quantiles of Condition")
  } else if (plottype == "raw") {
    p <- p +
      ggplot2::labs(fill = "Relative Condition")
  }

  return(p)
}

# plot_condition(plottype = "raw")
# plot_condition()
#
# dat <- ecodata::condition |>
#   dplyr::group_by(Var, EPU) |>
#   dplyr::filter(EPU != "SS") |>
#   dplyr::mutate(
#     scaleCond = scale(Value, scale = T, center = T)
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(
#     cat = cut(
#       scaleCond,
#       breaks = quantile(
#         dat$scaleCond,
#         seq(0, 1, length.out = 6),
#         na.rm = TRUE
#       ),
#       labels = c(
#         "Poor Condition",
#         "Below Average",
#         "Neutral",
#         "Above Average",
#         "Good Condition"
#       ),
#       include.lowest = TRUE
#     ),
#     cat2 = cut(
#       Value,
#       breaks = quantile(
#         dat$Value,
#         seq(0, 1, length.out = 6),
#         na.rm = TRUE
#       ),
#       labels = c(
#         "Poor Condition",
#         "Below Average",
#         "Neutral",
#         "Above Average",
#         "Good Condition"
#       ),
#       include.lowest = TRUE
#     )
#   )
#
# dat |>
#   dplyr::group_by(cat) |>
#   dplyr::summarise(min = min(Value), max = max(Value))
#
# dat |>
#   dplyr::group_by(cat2) |>
#   dplyr::summarise(min = min(Value), max = max(Value))
#
# dat |>
#   dplyr::group_by(Var) |>
#   dplyr::summarise(delta = max(Value) - min(Value)) |>
#   dplyr::arrange(delta)
#
# dat |>
#   ggplot2::ggplot(ggplot2::aes(x = Value, y = scaleCond, color = Var)) +
#   ggplot2::geom_point() +
#   ggplot2::theme_bw() +
#   ggplot2::facet_wrap(~EPU, ncol = 1) +
#   ggplot2::geom_hline(
#     yintercept = quantile(
#       dat$scaleCond,
#       seq(0, 1, length.out = 6),
#       na.rm = TRUE
#     ),
#     lty = 2
#   ) +
#   ggplot2::theme(legend.position = "none")
