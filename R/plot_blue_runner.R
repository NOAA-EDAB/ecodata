#' plot blue_runner
#'
#' Creates map of blue runner locations by decade
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_blue_runner <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # this plot goes a bit more south than standard 36
  setup$ylims <- c(35, setup$ymax)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  blue<-ecodata::blue_runner |>
    tidyr::separate(Var, c("Var", "Pos"), "L") |>
    tidyr::pivot_wider(names_from = "Pos", values_from = "Value") |>
    #tidyr::spread(., Pos, Value) |>
    dplyr::rename(Lat = at,
                  Lon = on) |>
    dplyr::mutate(Var = dplyr::recode(Var,
                               "Positive Blue Runner Tows before 2001 - " = "Prior to 2000",
                               "Positive Blue Runner Tows 2001 - 2010 - " = "2001-2010",
                               "Positive Blue Runner Tows since 2010 - " = "Since 2010"))

  blue$Var <- factor(blue$Var, levels = c("Prior to 2000", "2001-2010", "Since 2010"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = setup$epu_sf, fill = "transparent", size = setup$map.lwd) +
    ggplot2::geom_point(data = blue, ggplot2::aes(x = Lon, y = Lat, color = Var, shape = Var))+
    ggplot2::scale_shape_manual(values=c(16, 3, 17))+
    ggplot2::scale_color_manual(values = c("blue", "black", "red"))+
    ggplot2::coord_sf(crs = setup$crs, xlim = setup$xlims, ylim = setup$ylims) +
    ecodata::theme_map() +
    ggplot2::ggtitle("Blue Runner Presence") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text= ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_blank(),
                   legend.position = c(0.6, 0.15))

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)

  # Paste commented original plot code chunk for reference
  # Mid Atlantic only
  # blue<-ecodata::blue_runner %>%
  #   tidyr::separate(Var, c("Var", "Pos"), "L") %>%
  #   tidyr::spread(., Pos, Value) %>%
  #   dplyr::rename(Lat = at,
  #                 Lon = on) %>%
  #   dplyr::mutate(Var = recode(Var,
  #                              "Positive Blue Runner Tows before 2001 - " = "Prior to 2000",
  #                              "Positive Blue Runner Tows 2001 - 2010 - " = "2001-2010",
  #                              "Positive Blue Runner Tows since 2010 - " = "Since 2010"))
  # blue$Var <- factor(blue$Var, levels = c("Prior to 2000", "2001-2010", "Since 2010"))
  #
  # #EPU shapefile
  # epu_sf <- ecodata::epu_sf %>%
  #   dplyr::filter(EPU %in% c("GOM","GB", "MAB"))
  #
  # #Map line parameters
  # map.lwd <- 0.4
  #
  # # Set lat/lon window for maps
  # xmin = -77
  # xmax = -65
  # ymin = 35
  # ymax = 45
  # xlims <- c(xmin, xmax)
  # ylims <- c(ymin, ymax)
  #
  # ## Map plotting blue runner
  # blue_map <-
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  #   ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  #   ggplot2::geom_point(data = blue, aes(x = Lon, y = Lat, color = Var, shape = Var))+
  #   ggplot2::scale_shape_manual(values=c(16, 3, 17))+
  #   ggplot2::scale_color_manual(values = c("blue", "black", "red"))+
  #   ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #   ecodata::theme_map() +
  #   ggplot2::ggtitle("Blue Runner Presence") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
  #                  legend.key = element_blank(),
  #                  axis.title = element_text(size = 11),
  #                  strip.background = element_blank(),
  #                  strip.text=element_text(hjust=0),
  #                  axis.text = element_text(size = 8),
  #                  legend.title = element_blank(),
  #                  legend.position = c(0.6, 0.15))
  #
  # blue_map
  #

}
