#' plot wind revenue
#'
#' plot wind_revenue data set. Only 5 species are plotted.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("landing","value")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wind_revenue <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "landing") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix <- tidyr::separate(ecodata::wind_revenue,col=Var, into = c("Species", "Var"),sep = "-sum_") |>
     dplyr::filter(Var == varName) |>
     dplyr::mutate(Value = Value/1000000,
                   Time = as.integer(Time)) |>
     dplyr::mutate(Species = dplyr::recode(Species,"MONK"="MONKFISH"))

   if (report == "MidAtlantic") {
     fix <- fix |>
       dplyr::filter(EPU %in% filterEPUs,
                     Species %in% c("LONGFIN SQUID","MONKFISH","SUMMER FLOUNDER",
                                    "OCEAN QUAHOG",  "SURF CLAM" )) |>
       dplyr::mutate(Species = stringr::str_to_sentence(Species))
     } else if (report == "NewEngland") {
       fix <- fix |>
         dplyr::filter(EPU %in% filterEPUs,
                       Species %in% c("ATLANTIC HERRING","MONKFISH","SEA SCALLOP",
                                      "SILVER HAKE",  "SKATES" )) |>
         dplyr::mutate(Species = stringr::str_to_sentence(Species))
     }


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
    ggplot2::ggtitle(paste0(report,": Fishery Revenue in Wind Lease Areas"))+
    ggplot2::ylab(expression("Dollars (millions)"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::facet_wrap(.~Species,scales = "free_y")+
    #ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
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

attr(plot_wind_revenue,"varName") <- c("landing","value")
attr(plot_wind_revenue,"report") <- c("MidAtlantic","NewEngland")


  # Paste commented original plot code chunk for reference
  # ecodata::dataset |>
  #   dplyr::filter(Var %in% c("..."),
  #                 EPU == "...") |>
  #   ... more dataset wrangling as necessary |>
  #   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   ggplot2::ggtitle("Title")+
  #   ggplot2::ylab(expression("Y label"))+
  #   ggplot2::xlab(element_blank())+
  #   ecodata::geom_gls()+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

