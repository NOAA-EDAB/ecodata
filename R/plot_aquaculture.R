#' plot aquaculture
#'
#' Creates line plot of oyster production.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_aquaculture <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB") # no EPU in 2023 dataset

    aqua <- ecodata::aquaculture |>
      dplyr::filter(Region %in% c("MD", "VA", "NJ")) |>
      dplyr::filter(!Value == "NA") |>
      dplyr::mutate(Time = as.integer(Time),
                    Value = as.numeric(Value))

  } else {
    filterEPUs <- c("GB", "GOM") # no EPU in 2023 dataset

    aqua <- ecodata::aquaculture |>
      dplyr::ungroup() |>
      dplyr::mutate(Region = as.character(Region)) |>
      dplyr::filter(!Region == "VA",
                    !Region == "NJ",
                    !Region == "MD",
                    !Region == "NA",
                    !Value == "NA") |>
      dplyr::mutate(Time = as.integer(Time),
                    Value = as.numeric(Value))|>
      dplyr::filter(Var == "Production/Acre") |>
      dplyr::group_by(Time) |>
      dplyr::summarise(Value = sum(Value))
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  # see above, differs by region


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- aqua |>
    ggplot2::ggplot() +
    #Highlight last ten years
    {if(report == "MidAtlantic") ggplot2::geom_line(ggplot2::aes(x = Time,
                                    y = Value,
                                    color = Region),
                       size = setup$lwd)} +
    {if(report == "MidAtlantic") ggplot2::geom_point(ggplot2::aes(x = Time,
                                     y = Value,
                                     color = Region),
                        size = setup$lwd)} +
    {if(report == "NewEngland") ggplot2::geom_line(ggplot2::aes(x = Time,
                                                                y = Value),
                                                   size = setup$lwd)} +
    {if(report == "NewEngland") ggplot2::geom_point(ggplot2::aes(x = Time,
                                                                 y = Value),
                                                    size = setup$lwd)} +
    #ggplot2::facet_wrap(~Region, nrow = 3)+
    ggplot2::ggtitle(paste0("Oyster Production in ", setup$region))+
    {if(report == "MidAtlantic") ggplot2::ylab(expression("Oyster production"))} +
    {if(report == "NewEngland")ggplot2::ylab(expression("Production/Acre"))} +
    ggplot2::xlab("")+
    ggplot2::theme(legend.position="bottom",
                   legend.title = ggplot2::element_blank())+
    ggplot2::scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019, 2021))+
    ecodata::theme_ts()+
    ecodata::theme_title()


    return(p)
}


attr(plot_aquaculture,"report") <- c("MidAtlantic","NewEngland")


  # Paste commented original plot code chunk for reference
  # MAB
  # aqua <- ecodata::aquaculture %>%
  #   dplyr::filter(Region %in% c("MD", "VA", "NJ")) %>%
  #   dplyr::filter(!Value == "NA") %>%
  #   dplyr::mutate(Time = as.integer(Time),
  #                 Value = as.numeric(Value))
  #
  # ggplot2::ggplot() +
  #   #Highlight last ten years
  #   ggplot2::geom_line(data = aqua, aes(x = Time, y = Value, color = Region), size = lwd) +
  #   ggplot2::geom_point(data = aqua,aes(x = Time, y = Value, color = Region), size = pcex) +
  #   #ggplot2::facet_wrap(~Region, nrow = 3)+
  #   ggplot2::ggtitle("Oyster Production in MAB")+
  #   ggplot2::ylab(expression("Oysters production")) +
  #   ggplot2::xlab(element_blank())+
  #   scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019))+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  # # aqua %>% group_by(Time) %>% summarise(Value = sum(Value)) %>%
  # #   dplyr::filter(Time %in% c(max(Time), max(Time-1))) %>%
  # #   dplyr::summarise(m = mean(Value))
  # #
  # # aqua %>% group_by(Time) %>% summarise(Value = sum(Value)) %>%
  # #   dplyr::filter(Time %in% c(max(Time-2), max(Time-3),  max(Time-4)),
  # #                 !Value == "NA") %>%
  # #   dplyr::summarise(m= mean(Value))
  # #
  #
  # NE
  #
  # ecodata::aquaculture %>%
  #   ungroup() %>%
  #   mutate(Region = as.character(Region)) %>%
  #   dplyr::filter(!Region == "VA",
  #                 !Region == "NJ",
  #                 !Region == "MD",
  #                 !Region == "NA",
  #                 !Value == "NA") %>%
  #   dplyr::mutate(Time = as.integer(Time),
  #                 Value = as.numeric(Value))%>%
  #   filter(Var == "Production/Acre") %>%
  #   group_by(Time) %>%
  #   summarise(Value = sum(Value)) %>%
  #   ggplot2::ggplot() +
  #   #Highlight last ten years
  #   ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  #   ggplot2::geom_point(aes(x = Time, y = Value), size = pcex) +
  #   #ecodata::geom_lm(aes(x = Time, y = Value))+
  #   ggplot2::ggtitle("Total Oyster Production in New England")+
  #   ggplot2::ylab(expression("Production/Acre")) +
  #   ggplot2::xlab("")+
  #   theme(legend.position="bottom",
  #         legend.title = element_blank())+
  #   scale_x_continuous(breaks=c(2009,2011,2013,2015, 2017, 2019, 2021))+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()

