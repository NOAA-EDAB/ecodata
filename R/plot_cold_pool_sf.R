#' plot cold_pool_sf
#'
#' Creates a map of annual cold pool area highlighting max, min, and most recent
#' year, with inset time series of area.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_cold_pool_sf <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # adjust map window for Mid Atlantic cold pool region
  setup$xmin  <- -78
  setup$xmax <- -69
  setup$ymin <- 35.5
  setup$ymax <- 43
  setup$xlims <- c(setup$xmin, setup$xmax)
  setup$ylims <- c(setup$ymin, setup$ymax)


  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  # YEARS HARDCODED, NOT INCLUDED WITH DATASET
  area<- ecodata::cold_pool_sf |>
    dplyr::mutate(Time = c(1993:2018))
  area2<- data.frame(Area = sf::st_area(area))|>
    dplyr::mutate(Time = c(1993:2018)) |>
    tidyr::separate(Area, c("Value", "Units"), sep = " ") |>
    dplyr::mutate(Value = as.numeric(Value),
           hline = mean(Value))

  minCPyear <- as.numeric(area2$Time[which(area2$Value == min(area2$Value, na.rm=TRUE))])
  maxCPyear <- as.numeric(area2$Time[which(area2$Value == max(area2$Value, na.rm=TRUE))])
  curCPyear <- as.numeric(max(area2$Time, na.rm = TRUE))

  cpsfcur <- area |>
    dplyr::filter(Time == curCPyear)
  cpsf<- area |>
    dplyr::filter(!Time == curCPyear)
  cpmin <- area |>
    dplyr::filter(Time == minCPyear)
  cpmax <- area |>
    dplyr::filter(Time == maxCPyear)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <-   ggplot2::ggplot() +
    #ggplot2::geom_tile(data =hw, aes(x = Longitude, y = Latitude,fill = Value)) +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::geom_sf(data = cpsf, alpha = 0.1)  +
    ggplot2::geom_sf(data = cpsfcur, fill = "transparent", color = "black", size = 1)+
    ggplot2::geom_sf(data = cpmax, fill = "transparent", color = "blue", size = 0.5)+
    ggplot2::geom_sf(data = cpmin, fill = "transparent", color = "red", size = 0.5)+
    ggplot2::coord_sf(crs = setup$crs, xlim = setup$xlims, ylim = setup$ylims) +
    #ggplot2::scale_fill_gradient2(name = "Temp.\nAnomaly (C)",
    #                     low = scales::muted("blue"),
    #                     mid = "white",
    #                     high = scales::muted("red"),
    #                     limits = c(-4,4)) +

    #facet_wrap(Season~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle("Cold Pool Area") +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 11),
                   strip.background = ggplot2::element_blank(),
                   strip.text= ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(angle = 90))+
    ecodata::theme_title()


  area_ts <-  ggplot2::ggplotGrob(area2 |>
                                    ggplot2::ggplot() +
                                    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value), size = setup$lwd) +
                                    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value), size = setup$pcex) +
                                    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                                                        size = setup$hline.size,
                                                        alpha = setup$hline.alpha,
                                                        linetype = setup$hline.lty)+
                                    ggplot2::scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015),
                                                                expand = c(0.01, 0.01)) +
                                    ggplot2::ggtitle("")+
                                    ggplot2::ylab((expression("Area (m"^2*")"))) +
                                    ggplot2::xlab("")+
                                    ecodata::theme_ts()+
                                    ggplot2::theme(
                                      axis.text.y = ggplot2::element_text(size = 6),
                                      axis.text.x = ggplot2::element_text(size = 6),
                                      panel.background = ggplot2::element_rect(fill = "white"),
                                      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                                      panel.grid.major = ggplot2::element_blank(),
                                      panel.grid.minor = ggplot2::element_blank(),
                                      legend.background = ggplot2::element_rect(fill = "transparent"),
                                      legend.box.background = ggplot2::element_rect(fill = "transparent"),
                                      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
                                      axis.line = ggplot2::element_blank())#,
                                  #panel.border = element_blank())
  )


      p <- p +
        ggplot2::annotation_custom(grob = area_ts,  xmin=-78.5, xmax=-71.75,
                          ymin=40.95, ymax=43.75)


    return(p)

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

}
