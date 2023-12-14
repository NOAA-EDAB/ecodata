#' plot diversity of survey data
#'
#' plots ecodata::survey_shannon
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_survey_shannon <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

   fix <- tidyr::separate(ecodata::survey_shannon,col = Var,into = c("indicator","season"),sep="-") |>
     dplyr::mutate(indicator = trimws(indicator),
                   season = trimws(season)) |>
     dplyr::filter(EPU %in% filterEPUs)


   p <- fix |>
     ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color=season))+
     ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                       xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                       ymin = -Inf, ymax = Inf) +
     ggplot2::geom_point()+
     ggplot2::geom_line()+
     ggplot2::ggtitle(paste0(report,": Survey Shannon Diversity Index"))+
     ggplot2::ylab("Shannon")+
     ggplot2::xlab(ggplot2::element_blank())+
     ggplot2::facet_wrap(.~EPU)+
     ecodata::geom_gls()+
     #ggplot2::scale_color_discrete(name = "Season", labels = c("Fall", "Spring"))+
     ecodata::theme_ts()+
     ecodata::theme_facet()+
     ecodata::theme_title()

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)


}
