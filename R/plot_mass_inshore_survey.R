#' plot massachusetts inshore data
#'
#' Plots mass_inshore_survey
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_mass_inshore_survey <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    stop("Indicator for 'NewEngland' report only")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::mass_inshore_survey |>
    dplyr::filter(EPU %in% filterEPUs,
                  !grepl("Other",Var)) |>
    tidyr::separate(Var, into = c("Var",  "Trash"), sep = " - ") |>
    dplyr::select(!Trash) |>
    tidyr::separate(Var, into = c("Var", "Val"), sep = " Biomass ") |>
    tidyr::pivot_wider(names_from = Val, values_from = Value) |>
    dplyr::mutate(Index = as.numeric(Index),
                  SE = as.numeric(SE)) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(hline = mean(Index),
                  upper = Index + (2*SE),
                  lower = Index - (2*SE))

  fix$Var <- factor(fix$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                       "Benthivore Spring", "Benthivore Fall",
                                       "Planktivore Spring", "Planktivore Fall",
                                       "Benthos Spring", "Benthos Fall"))

  # Determine the order of your facets
  facet_order <- levels(fix$Var)

  # function to plot custom y lims as scalar multiplier of 1.2x the max value for guild
  plot_custom_lims <- function(data) {
    new_max <- max((data$Index) * 1.2, na.rm = TRUE)
    p <- data |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Index)) +
      ggplot2::annotate(
        "rect",
        fill = setup$shade.fill,
        alpha = setup$shade.alpha,
        xmin = setup$x.shade.min,
        xmax = setup$x.shade.max,
        ymin = -Inf,
        ymax = Inf
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline, group = Var)
      ) +
      ggplot2::geom_ribbon(
        # data = fix,
        ggplot2::aes(x = Time, ymin = lower, ymax = upper),
        alpha = 0.5,
        fill = "gray"
      ) +
      #  ggplot2::ggtitle("Massachusetts inshore BTS") +
      ggplot2::ylab(ggplot2::element_blank()) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(
        limits = c(0, new_max),
        oob = scales::oob_keep
      ) +
      ggplot2::coord_cartesian(clip = "on") +
      ggplot2::facet_wrap(~Var, ncol = 2) +
      ecodata::geom_gls() +
      ecodata::geom_lm(n = 10) +
      ecodata::theme_ts() +
      ecodata::theme_facet() +
      ecodata::theme_title()
  }

  p1 <-  fix |>
    dplyr::filter(Var == "Piscivore Spring" | Var == "Piscivore Fall") |>
    plot_custom_lims()

  p2 <- fix |>
    dplyr::filter(Var == "Benthivore Spring" | Var == "Benthivore Fall") |>
    plot_custom_lims()

  p3 <- fix |>
    dplyr::filter(Var == "Planktivore Spring" | Var == "Planktivore Fall") |>
    plot_custom_lims()

  p4 <- fix |>
    dplyr::filter(Var == "Benthos Spring" | Var == "Benthos Fall") |>
    plot_custom_lims()

  p <- ggpubr::ggarrange(p1, p2, p3, p4, ncol = 1, nrow = 4)

  p <- ggpubr::annotate_figure(
    p,
    top = ggpubr::text_grob("Massachusetts inshore BTS", size = 12),
    left = ggpubr::text_grob("Biomass (kg tow ^-1)", rot = 90, size = 16)
  )

    return(p)
}

attr(plot_mass_inshore_survey,"report") <- c("MidAtlantic","NewEngland")

#plot_mass_inshore_survey(report = "NewEngland")
