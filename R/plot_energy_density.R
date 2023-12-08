#' plot energy_density
#'
#' Creates multiplanel plot of energy density time series by species.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_energy_density <- function(shadedRegion = shadedRegion,
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
  d<-ecodata::energy_density |>
    tidyr::separate(Var, into = c("Species", "Season", "Var"), sep = "/") |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::mutate(Energy.Density_Mean = as.numeric(Energy.Density_Mean),
                  Energy.Density_SD = as.numeric(Energy.Density_SD),
                  upper = Energy.Density_Mean + Energy.Density_SD,
                  lower = Energy.Density_Mean - Energy.Density_SD) |>
    dplyr::group_by(Season, Species)


  old.ed<- data.frame("Species" = c("Alewife", "Atl. Herring","Atl. Mackerel","Butterfish", "Illex squid", "Loligo squid",  "Sand lance","Silver hake" ,  "Atl. Herring", "Illex squid","Sand lance"),
                      "Year" = c("1980s", "1980s", "1980s", "1980s", "1980s", "1980s","1980s", "1980s", "1990s",
                                 "1990s","1990s"),
                      "Season" = c("All", "All", "All", "All", "All", "All", "All", "All" , "All", "All", "All"),
                      "N" = c(rep("NA", 11)),
                      "Energy.Density_Mean" = c(6.4, 10.6, 6.0, 6.2, 7.1, 5.6, 6.8, 4.6, 9.4, 5.9, 4.4))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- d |>
    ggplot2::ggplot(ggplot2::aes(x=Time, y = Energy.Density_Mean, color = Season))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lower, ymax=upper), width=.2) +
    ggplot2::geom_hline(data = old.ed, ggplot2::aes(yintercept = Energy.Density_Mean, linetype =Year))+
    ggplot2::facet_wrap(~Species, nrow = 2)+
    ggplot2::ylab("Mean Energy Density (kJ/g)")+
    ggplot2::theme(axis.title.x= ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45,  hjust = 1),
                   legend.title = ggplot2::element_blank())+
    #ggplot2::scale_x_continuous(breaks=c(2017,2018, 2019, 2020, 2021, 2022))+
    ggplot2::ggtitle("Forage Fish Energy Density")+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)

  # Paste commented original plot code chunk for reference
  # d<-ecodata::energy_density %>%
  #   tidyr::separate(Var, into = c("Species", "Season", "Var"), sep = "/") %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value)
  #
  # old.ed<- data.frame("Species" = c("Alewife", "Atl. Herring","Atl. Mackerel","Butterfish", "Illex squid", "Loligo squid",  "Sand lance","Silver hake" ,  "Atl. Herring", "Illex squid","Sand lance"),
  #                     "Year" = c("1980s", "1980s", "1980s", "1980s", "1980s", "1980s","1980s", "1980s", "1990s",
  #                                "1990s","1990s"),
  #                     "Season" = c("All", "All", "All", "All", "All", "All", "All", "All" , "All", "All", "All"),
  #                     "N" = c(rep("NA", 11)),
  #                     "Energy.Density_Mean" = c(6.4, 10.6, 6.0, 6.2, 7.1, 5.6, 6.8, 4.6, 9.4, 5.9, 4.4))
  #
  #
  # d %>%
  #   dplyr::mutate(Energy.Density_Mean = as.numeric(Energy.Density_Mean),
  #                 Energy.Density_SD = as.numeric(Energy.Density_SD),
  #                 upper = Energy.Density_Mean + Energy.Density_SD,
  #                 lower = Energy.Density_Mean - Energy.Density_SD) %>%
  #   dplyr::group_by(Season, Species) %>%
  #   ggplot2::ggplot(aes(x=Time, y = Energy.Density_Mean, color = Season))+
  #   ggplot2::geom_point()+
  #   geom_line()+
  #   ggplot2::geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  #   geom_hline(data = old.ed, aes(yintercept = Energy.Density_Mean, linetype =Year))+
  #   ggplot2::facet_wrap(~Species, nrow = 2)+
  #   ggplot2::ylab("Mean Energy Density (kJ/g)")+
  #   ggplot2::theme(axis.title.x=element_blank(),
  #                  axis.text.x = element_text(angle = 45,  hjust = 1),
  #                  legend.title = element_blank())+
  #   scale_x_continuous(breaks=c(2017,2018, 2019, 2020, 2021, 2022))+
  #   ggplot2::ggtitle("Forage Fish Energy Density")+
  #   ecodata::theme_facet()+
  #   ecodata::theme_title()
  #
  #

}
