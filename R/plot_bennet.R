#' plot Bennett indicator
#'
#' Stacked bar charts of price and volume components of revenue by feeding guild.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which plot ("guild", "total",""total_guild")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#'
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_bennet <- function(shadedRegion = NULL,
                        report="MidAtlantic",
                        varName="guild",
                        EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  #Filter data into two dataframes for plotting
  indicators <- ecodata::bennet |>
    dplyr::filter(EPU %in% filterEPUs)

  if (varName == "guild") {
    indicators$Var<- gsub( "Predator", "", indicators$Var)
    indicators$Var<- gsub( "Value", "Volume", indicators$Var)
    indicators<- indicators |>
      tidyr::separate(Var, c("Guild", "Var") ) |>
      dplyr::filter(!Var == "Revenue",
                    !Guild == "Total",
                    !Time < 1981) |>
      dplyr::group_by(Time,  Var, EPU) |>
      dplyr::mutate(component = sum(Value)) |>
      dplyr::ungroup()

    revchange <- ecodata::bennet  |>
      dplyr::filter(EPU %in% filterEPUs,
                    #Var %in% c("Total Revenue Change - Bennet"),
                    !Time<1981)

    indicators$Guild<-factor(indicators$Guild, levels = c("Apex", "Piscivore", "Planktivore",
                                                        "Benthivore", "Benthos", "Other"))

    p <- ggplot2::ggplot()+
      #Highlight last ten years
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf)+
      ggplot2::geom_bar(data = indicators, ggplot2::aes(x = Time, y = Value, fill = Guild), stat="identity")+
      #ggplot2::scale_fill_manual(name = "Indicators", values = Guild) +
      ggplot2::geom_line(data = indicators, ggplot2::aes(x = Time, y = component, color = "$"))+
      ggplot2::scale_x_continuous(breaks = seq(1980, 2020, by = 5), expand = c(0.01, 0.01)) +
      ggplot2::facet_grid(EPU~Var, scales = "free")+
      ggplot2::scale_colour_grey(name ="Component") +
      ggplot2::ggtitle("Bennet Indicator")+
      ggplot2::labs(y="Value $1,000,000 ($ADD BASE YEAR)") +

      ggplot2::xlab(ggplot2::element_blank())+
      #ggplot2::scale_x_continuous(breaks = seq(1965, 2020, by = 10), expand = c(0.01, 0.01)) +
      #ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100),
      #                            limits = y.lim, expand = c(0.01, 0.01)) +
      ecodata::theme_ts() +
      ggplot2::scale_fill_brewer(palette = "Set1")+
      ggplot2::theme(title = ggplot2::element_text(size = 10))+
      ecodata::theme_title()+
      ecodata::theme_facet()


  } else if (varName == "total") {

    indicators <- indicators |>
      dplyr::filter(stringr::str_detect(Var, pattern="Total"),
                    !Var == "Total Revenue Change - Bennet",
                    !Time < 1981) |>
      dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Total Volume Indicator - Bennet", "Total Price Index - Bennet"),
                                               to = c("Volume","Price")))  |>
      dplyr::group_by(Time) |>
      dplyr::mutate(New = sum(Value))

    revchange <- ecodata::bennet |>
      dplyr::filter(EPU == filterEPUs,
                    Var %in% c("Total Revenue Change - Bennet"),
                    !Time<1981)

    ind_fill <- c("#a6cee3", "#b2df8a")

    p <- ggplot2::ggplot()+
      #Highlight last ten years
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf)+
      ggplot2::geom_bar(data = indicators, ggplot2::aes(x = Time, y = Value, fill = Var), stat="identity")+
      ggplot2::scale_fill_manual(name = "Indicators", values = ind_fill) +
      ggplot2::geom_line(data = revchange, ggplot2::aes(x = Time, y = Value, color = "$"))+
      ggplot2::scale_colour_grey(name ="Revenue Change") +
      ggplot2::ggtitle("Bennet Indicator")+
      ggplot2::labs(y="Value $1,000,000  ($ADD BASE YEAR)") +
      ggplot2::scale_x_continuous(breaks = seq(1980, 2020, by = 5), expand = c(0.01, 0.01)) +
      #::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100),
      #                            limits = y.lim, expand = c(0.01, 0.01)) +
      ecodata::theme_ts() +
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::theme(title = ggplot2::element_text(size = 10))+
      ecodata::theme_title()


  } else if (varName == "total_guild") {

    indicators$Var<- gsub( "Predator", "", indicators$Var)
    indicators$Var<- gsub( "Value", "Volume", indicators$Var)
    indicators<- indicators |>
      tidyr::separate(Var, c("Guild", "Var") ) |>
      dplyr::filter(!Var == "Revenue",
                    !Guild == "Total",
                    !Time < 1981) |>
      dplyr::group_by(Time, Guild) |>
      dplyr::mutate(New = sum(Value))

    revchange <- ecodata::bennet |>
      dplyr::filter(EPU == filterEPUs,
                    Var %in% c("Total Revenue Change - Bennet"),
                    !Time<1981)
    #custom bar fill color (color-blind friendly)
    ind_fill <- c("#a6cee3", "#b2df8a")

    #limits
    y.lim <- c(-450,600)

    #plot
    p <- ggplot2::ggplot()+
      #Highlight last ten years
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf)+
      ggplot2::geom_bar(data = indicators, ggplot2::aes(x = Time, y = Value, fill = Var), stat="identity")+
      ggplot2::scale_fill_manual(name = "Indicators", values = ind_fill) +
      ggplot2::geom_line(data = revchange, ggplot2::aes(x = Time, y = Value, color = "$"))+
      ggplot2::scale_colour_grey(name ="Revenue Change") +
      ggplot2::ggtitle("Bennet Indicator")+
      ggplot2::labs(y="Value $1,000,000 ($ADD BASE YEAR)") +
      ggplot2::scale_x_continuous(breaks = seq(1980, 2015, by = 10), expand = c(0.01, 0.01)) +
      #ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100),
      #                            limits = y.lim, expand = c(0.01, 0.01)) +
      ecodata::theme_ts() +
      ggplot2::facet_wrap(~Guild)+
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     legend.position = "bottom")+
      ecodata::theme_title()

  }


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #





   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)
}


attr(plot_bennet,"report") <- c("MidAtlantic","NewEngland")
attr(plot_bennet,"varName") <- c("guild","total","total_guild")
attr(plot_bennet,"EPU") <- c("MAB","GB","GOM")

  # Paste commented original plot code chunk for reference
  # MAB version
  # # #Filter data into two dataframes for plotting
  # indicators <- ecodata::bennet |>
  #   dplyr::filter(EPU == epu_abbr)
  #
  # indicators$Var<- gsub( "Predator", "", indicators$Var)
  # indicators$Var<- gsub( "Value", "Volume", indicators$Var)
  # indicators<- indicators |>
  #   separate(Var, c("Guild", "Var") ) |>
  #   dplyr::filter(!Var == "Revenue",
  #                 !Guild == "Total",
  #                 !Time < 1985) |>
  #   dplyr::group_by(Time,  Var) |>
  #   dplyr::mutate(component = sum(Value)) |>
  #   ungroup()
  #
  # # dplyr::filter(#stringr::str_detect(Var, pattern="Total"),
  # #        !Var == "Total Revenue Change - Bennet",
  # #        !Time < 1985) |>
  # # dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Total Volume Index - Bennet", "Total Price Index - Bennet"),
  # #                                          to = c("Volume","Price"))) |>
  # # dplyr::group_by(Time) |>
  # # dplyr::mutate(New = sum(Value)) |>
  # # dplyr::group_by(Time, Var) |>
  # # dplyr::mutate(component = sum(Value))
  #
  # revchange <- ecodata::bennet |>
  #   dplyr::filter(EPU == "MAB",
  #                 #Var %in% c("Total Revenue Change - Bennet"),
  #                 !Time<1985)
  # #custom bar fill color (color-blind friendly)
  # #ind_fill <- c("#a6cee3", "#b2df8a", "#000001")
  #
  # #limits
  # y.lim <- c(-400,550)
  #
  # indicators$Guild<-factor(indicators$Guild, levels = c("Apex", "Piscivore", "Planktivore",
  #                                                       "Benthivore", "Benthos", "Other"))
  #
  # #plot
  # ggplot2::ggplot()+
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf)+
  #   ggplot2::geom_bar(data = indicators, aes(x = Time, y = Value, fill = Guild), stat="identity")+
  #   #ggplot2::scale_fill_manual(name = "Indicators", values = Guild) +
  #   ggplot2::geom_line(data = indicators, aes(x = Time, y = component, color = "$"))+
  #   ggplot2::facet_wrap(~Var)+
  #   ggplot2::scale_colour_grey(name ="Component") +
  #   ggplot2::ggtitle("Bennet Indicator")+
  #   ggplot2::labs(y="Value $1,000,000 ($2015)") +
  #   ggplot2::scale_x_continuous(breaks = seq(1990, 2020, by = 10), expand = c(0.01, 0.01)) +
  #   #ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100),
  #   #                            limits = y.lim, expand = c(0.01, 0.01)) +
  #   ggplot2::scale_fill_brewer(palette = "Set1")+
  #   ecodata::theme_ts() +
  #   ggplot2::xlab(element_blank())+
  #   ggplot2::theme(title = element_text(size = 10))+
  #   ecodata::theme_title()+
  #   ecodata::theme_facet()
  #
  #

