#' plot species_dist
#'
#' Plots species distribution along shelf and depth
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("along","depth")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_species_dist <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic",
                              varName = "along") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    stop("This is a shelfwide indicator only used in the MidAtlantic report")
    filterEPUs <- c("GB", "GOM")
  }

  if (varName == "along") {
    varName <- "along-shelf distance"
    ylab <- "Distance (km)"
    yaxis <- "identity"
  } else if (varName == "depth") {
    ylab <- "Depth (m)"
    yaxis <- "reverse"

  } else {
    stop("Please select varName = 'along' or 'depth'")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::species_dist  |>
    #dplyr::filter(!Var == "Season") %>%
    dplyr::mutate(Value = as.numeric(Value)) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(hline = mean(Value)) |>
    dplyr::ungroup() |>
    dplyr::filter(Var == varName,
                  !Time == 2020)



  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::scale_y_continuous(trans = yaxis)+
    ggplot2::ggtitle(stringr::str_to_sentence(varName))+
    ggplot2::ylab(ylab)+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ecodata::geom_gls() +
    ecodata::theme_ts()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)


}
