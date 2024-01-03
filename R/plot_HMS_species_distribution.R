#' plot HMS species distribution
#'
#' Plots distribution shifts for marine mammals
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_HMS_species_distribution <- function(shadedRegion = NULL,
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
  hms10<- ecodata::HMS_species_distribution |>
    tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::filter(Time == 2010) |>
    dplyr::rename("x_start" = wlon,
                  "y_start" = wlat) |>
    dplyr::select(!Time)

  hms<-ecodata::HMS_species_distribution |>
    tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::filter(Time == 2017) |>
    dplyr::rename("x_end" = wlon,
                  "y_end" = wlat) |>
    dplyr::select(!Time) |>
    dplyr::left_join(hms10) |>
    dplyr::mutate(season = dplyr::recode(season, fall = "Fall",
                                         winter = "Winter",
                                         spring = "Spring",
                                         summer = "Summer"))
  hms$season <- factor(hms$season,levels = c("Winter","Spring",
                                             "Summer", "Fall"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <-  hms |> ggplot2::ggplot() +
      ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
      ggplot2::geom_sf(data = setup$epu_sf, fill = "transparent", size = setup$map.lwd) +
      ggplot2::geom_segment( mapping = ggplot2::aes(x = x_start, y = y_start,
                                  xend = x_end, yend = y_end, color = species),
                    arrow = grid::arrow(length = grid::unit(1, "mm")))+

      #ggplot2::scale_shape_manual(values=c(16, 3, 17))+
      #ggplot2::scale_color_manual(values = c("blue", "black", "red"))+
      ggplot2::coord_sf(crs = setup$crs, xlim = setup$xlims, ylim = setup$ylims) +
      ecodata::theme_map() +
      ggplot2::ggtitle("HMS Species Distribution") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_x_continuous(breaks=c(-76,-72, -68) )+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
                   #legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_blank())+#,
    #ggplot2::legend.position = c(0.6, 0.15)) +
    ggplot2::facet_wrap(~season)
  # hms_map

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = element_blank())
   #
   #  }

    return(p)

  # Paste commented original plot code chunk for reference
  # hms10<- ecodata::HMS_species_distribution |>
  #   tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") |>
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  #   dplyr::filter(Time == 2010) |>
  #   dplyr::rename("x_start" = wlon,
  #                 "y_start" = wlat) |>
  #   dplyr::select(!Time)
  #
  # hms<-ecodata::HMS_species_distribution |>
  #   tidyr::separate(Var, into = c("Var", "species", "season"), sep = "_") |>
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  #   dplyr::filter(Time == 2017) |>
  #   dplyr::rename("x_end" = wlon,
  #                 "y_end" = wlat) |>
  #   dplyr::select(!Time) |>
  #   dplyr::left_join(hms10) |>
  #   dplyr::mutate(season = dplyr::recode(season, fall = "Fall",
  #                                        winter = "Winter",
  #                                        spring = "Spring",
  #                                        summer = "Summer"))
  # hms$season <- factor(hms$season,levels = c("Winter","Spring",
  #                                            "Summer", "Fall"))
  # epu_sf <- ecodata::epu_sf |>
  #   dplyr::filter(EPU %in% c("GOM","GB", "MAB"))
  # map.lwd <- 0.4
  # xmin = -77
  # xmax = -65
  # ymin = 35
  # ymax = 45
  # xlims <- c(xmin, xmax)
  # ylims <- c(ymin, ymax)
  # hms_map <- hms |> ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = ecodata::coast, size = map.lwd) +
  #   ggplot2::geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  #   geom_segment( mapping = aes(x = x_start, y = y_start,
  #                               xend = x_end, yend = y_end, color = species),
  #                 arrow = grid::arrow(length = grid::unit(1, "mm")))+
  #
  #   #ggplot2::scale_shape_manual(values=c(16, 3, 17))+
  #   #ggplot2::scale_color_manual(values = c("blue", "black", "red"))+
  #   ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  #   ecodata::theme_map() +
  #   ggplot2::ggtitle("HMS Species Distibution") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   scale_x_continuous(breaks=c(-76,-72, -68) )+
  #   ggplot2::theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
  #                  #legend.key = element_blank(),
  #                  axis.title = element_text(size = 11),
  #                  strip.background = element_blank(),
  #                  strip.text=element_text(hjust=0),
  #                  axis.text = element_text(size = 8),
  #                  legend.title = element_blank())+#,
  #   #legend.position = c(0.6, 0.15)) +
  #   ggplot2::facet_wrap(~season)
  # hms_map


}
