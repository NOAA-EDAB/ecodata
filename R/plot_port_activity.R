#' plot port activity
#'
#' plots port activity dataset by EPU
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("all","overall","permit","transaction").
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#' @param topN Numeric scalar. Number of top ports to show (default = 9)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_port_activity <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0,
  topN = 9,
  varName = 'all'
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- setup$region_abbr
  }

  if (varName == 'overall') {
    filtervars <- 'port_overall_score'
  } else if (varName == 'permit') {
    filtervars <- 'port_permit_score'
  } else if (varName == 'transaction') {
    filtervars <- 'port_transaction_score'
  } else {
    filtervars <- c(
      'port_overall_score',
      'port_permit_score',
      'port_transaction_score'
    )
  }

  data <- ecodata::port_activity |>
    dplyr::filter(EPU %in% filterEPUs)

  data.rank <- data |>
    dplyr::filter(Var == 'port_overall_score' & Time == max(Time, na.rm = T)) |>
    dplyr::mutate(
      Value_rank = rank(-Value, ties.method = 'min'),
      is.top = ifelse(Value_rank <= topN, TRUE, FALSE)
    ) |>
    dplyr::select(City, State, is.top)

  fix <- data |>
    dplyr::left_join(data.rank) |>
    dplyr::mutate(is.top = ifelse(is.na(is.top), FALSE, is.top)) |>
    dplyr::filter(is.top) |>
    dplyr::filter(Var %in% filtervars) |>
    tidyr::unite(Port_name, c('City', 'State'), sep = ', ') |>
    dplyr::group_by(EPU, Port_name, Var) |>
    dplyr::mutate(hline = mean(Value, na.rm = T))

  #make plot

  ggplot2::ggplot(data = fix, ggplot2::aes(x = Time, y = Value, color = Var)) +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, color = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::facet_wrap(~Port_name) +
    ecodata::geom_lm(n = n) +
    ecodata::theme_ts() +
    ecodata::theme_title()
}

attr(plot_port_activity, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_port_activity, "varName") <- c(
  'all',
  'overall',
  'permit',
  'transaction'
)
