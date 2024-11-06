#' plot SAV
#'
#' Plots Mid-Atlantic Submerged Aquatic Vegetation SAV time series
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_SAV <- function(shadedRegion = NULL,
                     report="MidAtlantic",
                     n = 0) {

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
  sav <- ecodata::SAV |>
    dplyr::filter(!Var == "Baywide",
                  !Var == "Oligohaline",
                  !Var == "Mesohaline") |>
      dplyr::mutate(Var = dplyr::recode(Var,
                                 "Tidal" = "Tidal Fresh",
                                 "Polyhaline" = "High Salinity"),
                    Var = factor(Var, levels = c("Tidal Fresh", "High Salinity"))) |>
      dplyr::mutate(Value = (Value/1000))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- sav |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    #ecodata::geom_lm(aes(x = Time, y = Value))+
    ggplot2::facet_wrap(~Var)+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value)) +
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ggplot2::theme(strip.text = ggplot2::element_text(hjust=0,
                                           face = "italic"),
                   axis.title.y = ggplot2::element_text(angle = 90),
                   legend.title = ggplot2::element_blank()) +
    # ggplot2::scale_color_discrete(name = "",
    # labels = c("Lower bay",  "Upper Bay"))+
    ecodata::theme_title()+
    ggplot2::ylab(expression("Acres (10"^3*")"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Submerged Aquatic Vegetation (SAV) Abundance")+
    ecodata::theme_facet()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)
}


attr(plot_SAV,"report") <- c("MidAtlantic","NewEngland")


  # Paste commented original plot code chunk for reference
  # ecodata::SAV %>%
  # dplyr::filter(!Var == "Baywide",
  #               !Var == "Oligohaline",
  #               !Var == "Mesohaline") %>%
  #   dplyr::mutate(Var = recode(Var,
  #                              "Tidal" = "Tidal Fresh",
  #                              "Polyhaline" = "High Salinity"),
  #                 Var = factor(Var, levels = c("Tidal Fresh", "High Salinity"))) %>%
  #   dplyr::mutate(Value = (Value/1000)) %>%
  #   ggplot2::ggplot(aes(x = Time, y = Value))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   #ecodata::geom_lm(aes(x = Time, y = Value))+
  #   ggplot2::facet_wrap(~Var)+
  #   ecodata::geom_gls(aes(x = Time, y = Value)) +
  #   ecodata::theme_ts()+
  #   ggplot2::theme(strip.text=element_text(hjust=0,
  #                                          face = "italic"),
  #                  axis.title.y = element_text(angle = 90),
  #                  legend.title = element_blank()) +
  #   # ggplot2::scale_color_discrete(name = "",
  #   # labels = c("Lower bay",  "Upper Bay"))+
  #   ecodata::theme_title()+
  #   ggplot2::ylab(expression("Acres (10"^3*")"))+
  #   ggplot2::xlab(element_blank())+
  #   ggplot2::ggtitle("Submerged Aquatic Vegetation (SAV) Abundance")+
  #   ecodata::theme_facet()
  #
  #
