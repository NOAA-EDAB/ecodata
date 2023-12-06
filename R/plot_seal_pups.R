#' plot Gray seal pup populations
#'
#' Plots seal_pups data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_seal_pups <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("NE")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix <- ecodata::seal_pups  |>
     tidyr::separate(Var, into = c("Var", "colony"), sep = "-") |>
     dplyr::filter(!colony == "Green",
                   Var == "count") |>
     dplyr::mutate(Var = dplyr::recode(Var, "count" = "Counts"),
                   colony = dplyr::recode(colony, "Nomans.Land" = "Nomans Land"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, color=colony, shape=colony))+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value,color = colony))+
    ggplot2::ggtitle("Estimated Gray Seal Pup Births")+
    ggplot2::ylab(expression("Pup Count"))+
    ggplot2::xlab("")+
    #ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)



}
