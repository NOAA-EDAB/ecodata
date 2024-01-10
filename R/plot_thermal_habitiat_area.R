#' plot thermal habitat area
#'
#' plots thermal_habitat_area data set.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
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
    dplyr::filter(EPU == filterEPUs) |>
    tidyr::pivot_wider(names_from = Source,values_from = Value) |>
    dplyr::mutate(presentBoth = !is.na(GLORYS+PSY),
                  PSYMask = dplyr::case_when(presentBoth == F ~ 1,
                                             TRUE ~ NA_real_),
                  PSY = PSY * PSYMask)  |>
    tidyr::pivot_longer(c(GLORYS,PSY),names_to = "Source",values_to = "Value") |>
    dplyr::select(-PSYMask,-presentBoth) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Time = lubridate::yday(Time))


  limits <- fix |>
    dplyr::group_by(Depth,Var,Time) |>
    dplyr::summarise(areaMinProportion = min(Value),
                     areaMaxProportion = max(Value),
                     .groups = "drop")

  fix <- fix |>
    dplyr::left_join(limits,by=c("Depth","Var","Time")) |>
    dplyr::filter(year == max(year))


  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_ribbon(data=fix, ggplot2::aes(x = Time, ymin = areaMinProportion, ymax = areaMaxProportion),fill = 'grey50')+
    ggplot2::geom_line(data=fix,ggplot2::aes(x = Time, y= Value),color = 'black',alpha = 0.7,size =1)+
    ggplot2::facet_grid(Var~Depth)+
    ggplot2::theme_bw()+
    ggplot2::xlab('Calendar Day')+
    ggplot2::ylab('Proportion of EPU Area above threshold') +
    ggplot2::ggtitle(filterEPUs)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


    return(p)

}
