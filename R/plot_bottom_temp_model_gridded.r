#' plot bottom temperature seasonal gridded data
#'
#' plots bottom_temp_model_gridded data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10), passed from plot function
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland"), passed from plot function
#' @param scale character string. celsius or fahrenheit. Default = "celsius"
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_model_gridded <- function(shadedRegion = NULL,
                                              report = "MidAtlantic",
                                              scale = "celsius") {


  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")

    xmin = -77
    xmax = -66
    ymin = 35.5
    ymax = 45
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)


  } else {
    filterEPUs <- c("GB", "GOM")
    # Set lat/lon window for maps
    xmin = -73
    xmax = -65
    ymin = 39
    ymax = 45
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
  }


  #EPU shapefile
  ne_epu_sf <- ecodata::epu_sf |>
    dplyr::filter(EPU %in% filterEPUs)


  fix <- ecodata::bottom_temp_model_gridded |>
    dplyr::filter(Time == max(Time)) |>
    dplyr::select(-Time) |>
    dplyr::mutate(Var = factor(Var, levels = c("winter","spring","summer","fall")))



  if (scale == "fahrenheit") {
    # convert celsius to fahrenheit
    fix <- fix |>
      dplyr::mutate(Value = (9/5)*Value + 32)
    label <- "Temp. (\u00B0F)"
    breaks <- c(41, 50, 59, 68, 77)
    labelLegend <- c("41", "50", "59", "68", "77")
    limits <- c(39,80)
    midpoint <- 59
  } else {
    label <- "Temp. (\u00B0C)"
    breaks <- c(5,10,15,20,25)
    labelLegend <- c("5", "10", "15", "20", "25")
    limits <- c(5,25)
    midpoint <- 15
  }

 # fix <- fix |> dplyr::mutate(Value = replace(Value, Value > maxVal, maxVal))




  p <- ggplot2::ggplot(data = fix)+
    ggplot2::geom_tile(ggplot2::aes(x = Longitude, y = Latitude, fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
    ggplot2::facet_wrap(Var~.)+
    ecodata::theme_map() +
    #scales::show_col(viridis::inferno(n=3)) Find the colors
    ggplot2::scale_fill_gradient2(name = label,
                                  low = "#000004FF",
                                  mid = "#BB3754FF",
                                  high = "#FCFFA4FF",
                                  limits = limits,
                                  labels = labelLegend,
                                  midpoint = midpoint) +
    #ggplot2::scale_color_viridis_c(option = 'B',name = 'Bottom \n Temp')+
    ggplot2::ggtitle('Seasonal Mean Bottom Temperature')+
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text=ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))+
    ecodata::theme_title() +

    ecodata::theme_ts()


  return(p)
}

attr(plot_bottom_temp_model_gridded,"report") <- c("MidAtlantic","NewEngland")



# p <-
#   ggplot2::ggplot() +
#   ggplot2::geom_tile(data = sst, ggplot2::aes(x = Longitude, y = Latitude,fill = Value)) +
#   ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
#   ggplot2::geom_sf(data = ne_epu_sf, fill = "transparent", size = setup$map.lwd) +
#   ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
#                                 low = scales::muted("blue"),
#                                 mid = "white",
#                                 high = scales::muted("red"),
#                                 limits = c(-5,5),
#                                 labels = c("<-5", "-2", "0", "2", ">5")) +
#   ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
#   ggplot2::facet_wrap(Season~.) +
#   ecodata::theme_map() +
#   ggplot2::ggtitle("SST anomaly") +
#   ggplot2::xlab("Longitude") +
#   ggplot2::ylab("Latitude") +
#   ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
#                  legend.key = ggplot2::element_blank(),
#                  axis.title = ggplot2::element_text(size = 11),
#                  strip.background = ggplot2::element_blank(),
#                  strip.text=ggplot2::element_text(hjust=0),
#                  axis.text = ggplot2::element_text(size = 8),
#                  axis.title.y = ggplot2::element_text(angle = 90))+
#
#    ecodata::theme_title()
