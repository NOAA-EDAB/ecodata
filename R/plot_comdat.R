#' plot comdat
#'
#' Plot time series of commercial landings or revenue for various aggregations.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("landings", "revenue")
#' @param plottype Character string. Which plot ("total", "guild")
#' @param NAFOyear Numeric value. Year that NAFO landings are separated.
#' Defaults to 2019, as NAFO data was missing 2019-2021 in 2022.
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_comdat <- function(shadedRegion = NULL,
                        report="MidAtlantic",
                        varName="landings",
                        plottype="total",
                        NAFOyear=2019) {

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
  #
  series.col2 <- c("indianred",  "black", "steelblue4") # total landings


  if(varName == "landings") {
    managed_landings <- ecodata::comdat  |>
      dplyr::filter(Var %in% c("Planktivore MAFMC managed species - Seafood Landings",
                               "Piscivore MAFMC managed species - Seafood Landings",
                               "Benthivore MAFMC managed species - Seafood Landings",
                               "Apex Predator MAFMC managed species - Seafood Landings",
                               "Benthos MAFMC managed species - Seafood Landings",
                               "Planktivore NEFMC managed species - Seafood Landings",
                               "Piscivore NEFMC managed species - Seafood Landings",
                               "Benthivore NEFMC managed species - Seafood Landings",
                               "Apex Predator NEFMC managed species - Seafood Landings",
                               "Benthos NEFMC managed species - Seafood Landings",
                               "Planktivore JOINT managed species - Seafood Landings",
                               "Piscivore JOINT managed species - Seafood Landings",
                               "Benthivore JOINT managed species - Seafood Landings",
                               "Apex Predator JOINT managed species - Seafood Landings",
                               "Benthos JOINT managed species - Seafood Landings"),
                    Time >= 1982) |>
      dplyr::filter(stringr::str_detect(Var, paste0("JOINT|", setup$council_abbr)))


    US_landings <- ecodata::comdat  |>
      dplyr::filter(Var == "Seafood Landings", # There may be US landings that aren't seafood
                    Time >= 1982)

    # Dec 2023:
    # Total is not total for 2019 on due to missing NAFO data
    # NAFO total dataset under construction will need to be added
    # NAFO_landings <- ecodata::NAFO |>
    #   dplyr::filter(Time >= NAFOyear)

    total_landings <- ecodata::comdat  |>
      dplyr::filter(Var %in% c("Planktivore Landings",
                               "Piscivore Landings",
                               "Benthivore Landings",
                               "Apex Predator Landings",
                               "Benthos Landings",
                               "Other Landings"),
                    Time >= 1982) |>
      dplyr::mutate(grouping = c("Total")) |>
      tidyr::separate(Var, into = c("feeding.guild"), sep = " ") #|>
      #dplyr::bind_rows(NAFO_landings) |>
      #dplyr::group_by(EPU,Time, feeding.guild) |>
      #dplyr::summarise(Value = sum(Value))

    managed_landings_guild <- managed_landings |>
      tidyr::separate(Var, into = c("feeding.guild"), sep = " ") |>
      dplyr::group_by(feeding.guild, EPU, Time) |>
      dplyr::summarise(Value = sum(Value)) |>
      dplyr::mutate(grouping = c("Council Seafood"),
                    Units = c("metric tons")) |>
      dplyr::ungroup()

    # guild plot input
    guilddat <- rbind(managed_landings_guild, total_landings) |>
      dplyr::filter(!stringr::str_detect(feeding.guild, "Apex|Other")) |>
      #tidyr::separate(Var, into = c("feeding.guild", "a", "grouping"), sep = " ") |>
      dplyr::mutate(#feeding.guild = stringr::str_extract(Var,c(feeding.guilds)),
        #grouping = recode(grouping, "Landings" = "total"),
        Var = paste(feeding.guild, grouping)) |>
      dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds)) |>
      dplyr::group_by(Var, EPU) |>
      dplyr::mutate(hline = mean(Value)) |>
      dplyr::filter(EPU %in% filterEPUs)

    total_landings_agg <- total_landings |>
      dplyr::group_by(EPU,Time) |>
      dplyr::summarise(Value = sum(Value)/1000) |>
      dplyr::mutate(Var = "Total",hline = mean(Value))

    us_total_landings_agg <- US_landings |>
      dplyr::group_by(EPU,Time) |>
      dplyr::summarise(Value = sum(Value)/1000) |>
      dplyr::mutate(Var = "US Seafood",hline = mean(Value))

    managed_landings_agg <- managed_landings |>
      dplyr::group_by(EPU,Time) |>
      dplyr::summarise(Value = sum(Value)/1000) |>
      dplyr::mutate(Var = "Council Seafood",hline = mean(Value))

    landings_agg <- rbind(total_landings_agg, managed_landings_agg, us_total_landings_agg)# |>
    #  dplyr::mutate(Value = Value/1000)
    # total plot input
    totdat<- landings_agg |>
      dplyr::filter(EPU %in% filterEPUs)

    ylabdat <- expression("Landings (10"^3*" metric tons)")
  }

  if(varName == "revenue") {

    # # Apex pred are MAB and NE so can't be added to NE EPU plots
    # lets leave them out of BOTH for now
    # apex<-ecodata::hms_landings |>
      # dplyr::filter(stringr::str_detect(Var, "Revenue"),
      #               Time<2021) |>
      # separate(Var, c("Var", "trash"), sep = "_") |>
      # group_by(Time) |>
      # summarise(Value = sum(Value)) |>
      # mutate(Var = c("HMS Revenue"),
      #        Units = c("metric tons"),
      #        EPU = c("MAB"))

    #Filtering and aggregation step
    rev_managed <- ecodata::comdat |>
      dplyr::filter(Var %in% c("Piscivore MAFMC managed species - Revenue",
                               "Planktivore MAFMC managed species - Revenue",
                               "Benthivore MAFMC managed species - Revenue",
                               "Benthos MAFMC managed species - Revenue",
                               "Piscivore NEFMC managed species - Revenue",
                               "Planktivore NEFMC managed species - Revenue",
                               "Benthivore NEFMC managed species - Revenue",
                               "Benthos NEFMC managed species - Revenue",
                               "Piscivore JOINT managed species - Revenue",
                               "Planktivore JOINT managed species - Revenue",
                               "Benthivore JOINT managed species - Revenue",
                               "Benthos JOINT managed species - Revenue")) |>
      #rbind(apex) |>
      dplyr::filter(stringr::str_detect(Var, paste0("JOINT|", setup$council_abbr))) |>
      dplyr::mutate(Status = c("Council Managed"))  #Create groups for

    rev_guild <- rev_managed |>
      tidyr::separate(Var, into = c("feeding.guild"), sep = " ") |>
      dplyr::group_by(feeding.guild, EPU, Time) |>
      dplyr::summarise(Value = sum(Value)/1000) |>
      dplyr::mutate(grouping = c("Council Managed"),
                       Units = c("10^3 US dollars")) |>
      dplyr::ungroup()

    rev_agg <- rev_managed |>
      dplyr::group_by(Status, Time, EPU) |>
      dplyr::summarise(Total = sum(Value)/1000000) |>
      dplyr::group_by(Status, EPU) |>
      dplyr::mutate(hline = mean(Total))

    rev_tot <- ecodata::comdat  |>
      dplyr::filter(Var %in% c("Planktivore Revenue",
                               "Piscivore Revenue",
                               "Benthivore Revenue",
                               "Apex Predator Revenue",
                               "Benthos Revenue",
                               "Other Revenue")) |>
      dplyr::mutate(grouping = c("Total")) |>
      tidyr::separate(Var, into = c("feeding.guild"), sep = " ") |>
      dplyr::mutate(Value = Value/1000)

    rev_total<- ecodata::comdat |>
      dplyr::filter(Var == "Revenue") |>
      dplyr::mutate(Status = c("Total")) |>
      dplyr::group_by(Status, Time, EPU) |>
      dplyr::summarise(Total = sum(Value)/1000000) |>
      dplyr::group_by(Status, EPU) |>
      dplyr::mutate(hline = mean(Total))

    guilddat <- rbind(rev_guild, rev_tot) |>
      dplyr::filter(!stringr::str_detect(feeding.guild, "Apex|Other")) |>
      #tidyr::separate(Var, into = c("feeding.guild", "a", "grouping"), sep = " ") |>
      dplyr::mutate(#feeding.guild = stringr::str_extract(Var,c(feeding.guilds)),
        #grouping = recode(grouping, "Landings" = "total"),
        Var = paste(feeding.guild, grouping)) |>
      dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds)) |>
      dplyr::group_by(Var, EPU) |>
      dplyr::mutate(hline = mean(Value)) |>
      dplyr::filter(Time >1982) |>
      dplyr::filter(EPU %in% filterEPUs)

    totdat <- rbind(rev_agg, rev_total) |>
      dplyr::mutate(Value = Total,
                    Var = Status) |>
      dplyr::filter(Time >1982) |>
      dplyr::filter(EPU %in% filterEPUs)

    ylabdat <- expression("Revenue (10"^6*" USD)")
  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  if(plottype == "total") {

    p <- totdat |>
      ggplot2::ggplot()+

      #Highlight last ten years
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ecodata::geom_gls(ggplot2::aes(x = Time, y = Value,
                                     group = Var),
                        alpha = setup$trend.alpha, size = setup$trend.size) +
      #ecodata::geom_lm(aes(x = Time, y = Value))+
      ggplot2::geom_line(ggplot2::aes(x = Time, y = Value, color = Var), linewidth = setup$lwd) +
      ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, color = Var), size = setup$pcex) +
      ggplot2::scale_x_continuous(breaks = seq(1980, 2020, by = 5), expand = c(0.01, 0.01)) +
      ggplot2::scale_color_manual(values = series.col2, aesthetics = "color")+
      ggplot2::facet_wrap(~EPU, scales = "free")+
      #ggplot2::guides(color = "none") +
      ggplot2::ylab(ylabdat) +
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())+
      ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                       color = Var),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty) +
      ecodata::theme_ts() +
      ggplot2::ggtitle(setup$region)+
      ecodata::theme_title() +
      ecodata::theme_facet()
  }

  if(plottype == "guild") {

    facet_names <- list("Apex" = expression("Apex"),
                        "Piscivores" = expression("Piscivores"),
                        "Planktivores" = expression("Planktivores"),
                        "Benthivores" = expression("Benthivores"),
                        "Benthos" = expression("Benthos"))
    p <- guilddat |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = grouping)) +

      #Highlight last ten years
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +

      #Test for trend and add lines
      ecodata::geom_gls(ggplot2::aes(x = Time, y = Value,
                            group = Var)) +
      # ecodata::geom_lm(aes(x = Time, y = Value,
      #              group = Var))+

      #Add time series
      ggplot2::geom_line(linewidth = setup$lwd) +
      ggplot2::geom_point(size = setup$pcex) +
      ggplot2::scale_color_manual(values = setup$series.col, aesthetics = "color")+
      ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                              color = grouping,
                              size = grouping),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty)+

      #Facet
      #facet_wrap(feeding.guild~., scales = "free", labeller = label, ncol = 1)+
      ggplot2::facet_wrap(feeding.guild~EPU,
                          ncol = length(filterEPUs),
                          scales = "free_y",
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE))+
      #ggplot2::facet_wrap(~feeding.guild, ncol = 1, scales = "free")+
      #Axis and theme
      #ggplot2::ylim(0, 200)+
      ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
      ggplot2::scale_x_continuous(breaks = seq(1980, 2020, by = 5), expand = c(0.01, 0.01)) +
      ggplot2::ylab(ylabdat) +
      ggplot2::xlab(ggplot2::element_blank())+
      ecodata::theme_facet() +
      ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_blank())+
      ggplot2::ggtitle(setup$region)+
      ecodata::theme_title()

  }


  # optional code for New England specific (2 panel) formatting
  # if (report == "NewEngland") {
  #   p <- p +
  #     ggplot2::theme(legend.position = "bottom",
  #                    legend.title = ggplot2::element_blank())
  #
  # }

  return(p)
}

attr(plot_comdat,"report") <- c("MidAtlantic","NewEngland")
attr(plot_comdat,"varName") <- c("landings","revenue")
attr(plot_comdat,"plottype") <- c("total","guild")
attr(plot_comdat,"NAFOyear") <- 2019

  # Paste commented original plot code chunk for reference
  # Sorry can't fit them all here, 6 different code chunks
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

