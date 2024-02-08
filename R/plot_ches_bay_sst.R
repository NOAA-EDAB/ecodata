#' plot ches_bay_sst
#'
#' Plot map of sea surface temperature for Chesapeake Bay.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#' @param scale character string. celsius or fahrenheit. Default = "celsius"
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_ches_bay_sst <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              scale = "celsius") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # adjust map window for Chesapeake Bay
  setup$xmin  <- -77.5
  setup$xmax <- -75
  setup$ymin <- 37
  setup$ymax <- 40
  setup$xlims <- c(setup$xmin, setup$xmax)
  setup$ylims <- c(setup$ymin, setup$ymax)


  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    message("Not part of New England report")
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  sst<- ecodata::ches_bay_sst
  sst$Var <- factor(sst$Var, levels = c("Winter",
                                              "Spring",
                                              "Summer",
                                              "Fall"))

  if (scale == "fahrenheit") {
    # convert celsius to fahrenheit
    sst <- sst |>
      dplyr::mutate(Value = (9/5)*Value + 32)
    label <- "Temp.\nAnomaly (\u00B0F)"
    breaks <- c(23,27.5,32,36.5,41)
    labelLegend <- c("<23", "27.5", "32", "36.5", ">41")
    limits <- c(23,41)
    maxVal <- 41
    midpoint <- 32
  } else {
    label <- "Temp.\nAnomaly (\u00B0C)"
    breaks <- c(-5,-2.5,0,2.5,5)
    labelLegend <- c("<-5", "-2.5", "0", "2.5", ">5")
    limits <- c(-5,5)
    maxVal <- 5
    midpoint <- 0
  }

  sst <- sst |> dplyr::mutate(Value = replace(Value, Value > maxVal, maxVal))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- sst |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = ecodata::coast, size = setup$map.lwd) +
    ggplot2::coord_sf(crs = setup$crs, xlim = setup$xlims, ylim = setup$ylims) +

    ggplot2::geom_tile(data = sst, ggplot2::aes(x = Latitude, y = Longitude,fill = Value)) +
    ggplot2::scale_fill_gradient2(name = label,
                                  low = scales::muted("blue"),
                                  mid = "white",
                                  high = scales::muted("red"),
                                  limits = limits,
                                  labels = labelLegend,
                                  breaks = breaks,
                                  midpoint = midpoint) +
    ggplot2::facet_wrap(Var~.) +
    ecodata::theme_map() +
    ggplot2::ggtitle("Chesapeake Bay SST anomaly") +
    ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(breaks = seq(37, 40, by = 1))+
    ggplot2::scale_x_continuous(breaks = seq(-77, -75, by = 1))+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.75),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = 6),
                   strip.background = ggplot2::element_blank(),
                   strip.text= ggplot2::element_text(hjust=0),
                   axis.text = ggplot2::element_text(size = 6),
                   axis.title.y = ggplot2::element_text(angle = 90) )+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

  if(report == "NewEngland") {
    p <- "Not part of New England report"
  }

    return(p)

}

attr(plot_ches_bay_sst,"report") <- c("MidAtlantic","NewEngland")

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
