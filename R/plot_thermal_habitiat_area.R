#' plot thermal habitat area
#'
#' plots thermal_habitat_area data set.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#' @param plottype character string. Which type of plots should be made. "daily" for daily area proportions or "annual"
#'
#' @return ggplot object
#'
#' @export

#'

plot_thermal_habitat_area <- function(shadedRegion = NULL,
                                      report="MidAtlantic",
                                      EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix <- ecodata::thermal_habitat_area |>
      dplyr::filter(EPU == filterEPUs)

  limits <- fix |>
      dplyr::group_by(Var,Time) |>
      dplyr::summarise(areaMinProportion = min(Value),
                       areaMaxProportion = max(Value),
                       .groups = "drop")

  fix <- fix |>
    dplyr::left_join(limits,by=c("Var","Time"))

  fix.this.year = fix |>
    dplyr::filter(Time == max(Time)) |>
    dplyr::mutate(ReportYear = max(Time))

  p <- ggplot2::ggplot()+
    # ggplot2::geom_line(data=fix, ggplot2::aes(x = temp.threshold, ymin = areaMinProportion, ymax = areaMaxProportion))+
    ggplot2::geom_line(data=fix,ggplot2::aes(x = temp.threshold, y= Value,group = Time,color = Time),alpha = 0.7,linewidth =1.2)+
    ggplot2::scale_color_gradient(name = "Year",low = 'grey70',high ='blue2')+
    ggplot2::geom_line(data=dplyr::filter(fix.this.year,Time == max(Time)),ggplot2::aes(x = temp.threshold, y = Value, linetype = as.factor(ReportYear)), color = 'black',alpha = 0.7,linewidth =2)+
    ggplot2::scale_linetype_manual(name = 'Report Year',values = 1)+
    ggplot2::facet_wrap(~Depth)+
    ggplot2::theme_bw()+
    ggplot2::xlab('Temperature Threshold (\u00B0C)')+
    ggplot2::ylab('Proportion of EPU Area above threshold') +
    ggplot2::ggtitle(filterEPUs)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(p)

}
