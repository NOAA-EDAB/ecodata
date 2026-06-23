#' Plot ecosystem overfishing indices
#'
#' Plots ppr dataset. Primary production (PP), Fogarty and Ryther indices, primary production required (ppr) scaled by primary production, and mean trophic level (mtl) index
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be "MAB"
#' @param varName Character string. Variable to plot ("pp","fogarty","ryther","ppr","mtl")
#' @param threshold Character string. Select how thresholds are calculated ("global","regional")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_ppr <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB",
  varName = "ryther",
  threshold = "global"
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  if (threshold == "regional") {
    # not yet available
    message("Regional thresholds are not yet available")
    return(p = NULL)
  }

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB", "GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  if (!(tolower(varName) %in% c("ryther", "fogarty", "pp", "ppr", "mtl"))) {
    stop(
      "Please use one following for varName.  `ryther`, `fogarty`, `pp`, `ppr`, `mtl`"
    )
  }

  if (tolower(varName) == "pp") {
    varName <- "PP"
    scalar <- 1 / 1e6
    vtitle <- "PP Reconstructed"
    vylab <- expression("mtC region"^-1 * "year"^-1 * "(millions)")
  } else if (tolower(varName) == "fogarty") {
    varName <- "Fogarty"
    scalar <- 1000
    vtitle <- "Fogarty Index"
    vylab <- "Parts per thousand"
    if (threshold == "global") {
      gr_up = 0.92
      rd_up = 2.5
      gr_lw = 0.22
      rd_lw = 1
    } else {}
  } else if (tolower(varName) == "ryther") {
    varName <- "Ryther"
    scalar <- 1
    vtitle <- "Ryther Index"
    vylab <- expression("mt km"^-2 * " y"^-1 * "")
    if (threshold == "global") {
      gr_up = 1.1
      rd_up = 5
      gr_lw = 0.3
      rd_lw = 3
    } else {}
  } else if (tolower(varName) == "mtl") {
    varName = "MTL"
    scalar <- 1
    vtitle <- "Mean trophic level of landings"
    vylab <- "Trophic Level"
  } else if (tolower(varName) == "ppr") {
    varName = "PPR"
    scalar <- 1
    vtitle <- "Primary production required (scaled)"
    vylab <- "Proportion"
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::ppr |>
    dplyr::filter(EPU %in% filterEPUs, Var == varName) |>
    dplyr::group_by(EPU) |>
    dplyr::mutate(Value = Value * scalar, hline = mean(Value))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    )

  if (varName == "PP") {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline),
        linewidth = setup$hline.size,
        alpha = setup$hline.alpha,
        linetype = setup$hline.lty
      )
  } else if (varName %in% c("PPR", "MTL")) {
    p <- p
  } else {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = gr_lw, ymax = gr_up, x = Time),
        fill = "darkolivegreen3",
        alpha = 0.5
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = rd_lw, ymax = rd_up, x = Time),
        fill = "sandybrown",
        alpha = 0.5
      )
  }

  p <- p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +

    ggplot2::ggtitle(paste0(EPU, " ", vtitle)) +
    ggplot2::ylab(vylab) +
    ggplot2::xlab("") +
    #ggplot2::facet_wrap(. ~ EPU) +
    ecodata::theme_ts() +
    ecodata::theme_facet() +
    ecodata::theme_title()

  # # optional code for New England specific (2 panel) formatting
  # if (report == "NewEngland") {
  #   p <- p +
  #     ggplot2::theme(
  #       legend.position = "bottom",
  #       legend.title = ggplot2::element_blank()
  #     )
  # }

  return(p)
}

attr(plot_ppr, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_ppr, "EPU") <- c("MAB", "GB", "GOM")
attr(plot_ppr, "varName") <- c("pp", "fogarty", "ryther", "ppr", "mtl")
attr(plot_ppr, "threshold") <- c("global")
