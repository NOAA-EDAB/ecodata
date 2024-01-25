#' plot heatwave_year
#'
#' Plot time series of daily detrended surface or bottom temperature for current year with
#' climatology in the background.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("Surface", "Bottom", "withtrend")
#' default variables Surface and Bottom are detrended; option to print surface including
#' climate change signal is "withtrend"
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_heatwave_year <- function(shadedRegion = NULL,
                              report = "MidAtlantic",
                              varName = "Surface") {

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

  plotvar = ifelse(varName == "withtrend", "Surface", paste0(varName,"Detrended"))

  year <- max(ecodata::heatwave_year$Year, na.rm = TRUE)

  hwyr <- ecodata::heatwave_year |>
    dplyr::filter(EPU %in% filterEPUs,
                  Year == year,
                  Var == plotvar)

  legendlab <- ifelse(varName == "withtrend", "Climatology",
                     "Shifted Climatology")

  ylabs <- ifelse(varName == "withtrend",
                  c("Temperature (C)"),
                  c("Temperature - Climate Trend (C)"))

  plotvartitle <- ifelse(varName == "withtrend", "Surface (not detrended)",
                           paste(varName,"Detrended"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  if(nrow(hwyr) > 0) {

    p <- hwyr |>
      ggplot2::ggplot( ggplot2::aes(x = t, y = temp))+
      heatwaveR::geom_flame(ggplot2::aes(y2 = thresh))+
      ggplot2::geom_line(ggplot2::aes(x = t, y = seas, color = "a"), linewidth = 1)+
      ggplot2::geom_line(ggplot2::aes(x = t, y = thresh, color = "c"), linewidth = 1)+
      ggplot2::geom_line(ggplot2::aes(x = t, y = temp, color = "b"))+
      ggplot2::scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
                          labels = c(legendlab,"Temperature", "Threshold"))+
      ggplot2::ylab(ylabs)+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::scale_x_date(date_labels = "%b", breaks = "1 month")+
      ggplot2::facet_wrap(~EPU, scales = "free_y") +
      ecodata::theme_facet() +
      ggplot2::ggtitle(paste(setup$region, plotvartitle, year))+
      ggplot2::theme(legend.title = ggplot2::element_blank(),
            legend.position=c(0.2, 0.8))+
      ecodata::theme_title()

     # optional code for New England specific (2 panel) formatting
      if (report == "NewEngland") {
        p <- p +
          ggplot2::theme(legend.position = "bottom",
                         legend.title = ggplot2::element_blank())

      }
  } else {
    #no data
    p <- "No data available"
  }
    return(p)
}

attr(plot_heatwave_year,"report") <- c("MidAtlantic","NewEngland")
attr(plot_heatwave_year,"varName") <- c("Surface","Bottom")



  # Paste commented original plot code chunk for reference
    # ecodata::heatwave_year %>%
    #   filter(EPU == "GB",
    #          stringr::str_detect(t, "2022"),
    #          Var == "SurfaceDetrended") %>%
    #   ggplot( aes(x = t, y = temp))+
    #   geom_flame(aes(y2 = thresh))+
    #   geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
    #   geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
    #   geom_line(aes(x = t, y = temp, color = "b"))+
    #   scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
    #                       labels = c("Shifted Climatology","Temperature", "Threshold"))+
    #   ylab("Temperature - Trend (C)")+
    #   xlab(element_blank())+
    #   scale_x_date(date_labels = "%b", breaks = "1 month")+
    #   theme_bw()+
    #   ggplot2::ggtitle("Georges Bank")+
    #   theme(legend.title = element_blank(),
    #         legend.position=c(0.2, 0.8))+
    #   ecodata::theme_title()
    #

