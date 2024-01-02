#' plot wind lease cumulative areas
#'
#' plot wind_dev_speed. This is shelf wide
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wind_dev_speed <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic") {

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
  fix <- ecodata::wind_dev_speed |>
    dplyr::mutate(Value = as.numeric(Value)/1000000,
                  Time = as.integer(Time)) |>
    dplyr::filter(Var == "Tot_Area_Acres")


  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, color = Report_year))+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value, color = Report_year))+
    #ggplot2::geom_point(aes(x = Time, y = `2022`))+
    #ggplot2::geom_line(aes(x = Time, y = `2022`))+
    ggplot2::ylab("Total Area (Million Acres)")+
    ggplot2::xlab("Project Construction Year")+
    ggplot2::ggtitle("Wind Lease Cumulative Area")+
    ecodata::theme_ts()+
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ggplot2::scale_x_continuous(breaks=c(2020,2022, 2024,2026, 2028, 2030))+
    ecodata::theme_title()+
    ggplot2::scale_colour_discrete(name="Year Reported", labels=c("2021","2022"))

  #  ggplot2::theme(legend.position = c(0.8, 0.2))

  return(p)
}
