#' plot aggregate_biomass
#'
#' Plots faceted spring and fall survey biomass time series by aggregate group
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_aggregate_biomass <- function(shadedRegion = NULL,
                                   report="MidAtlantic",
                                   EPU="GB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- EPU
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # this code from NE reports, works for Mid but includes Spring 2020
  agg<-ecodata::aggregate_biomass %>%
    dplyr::filter(!stringr::str_detect(Var, "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA")) %>% #remove unused datasets
    tidyr::separate(Var, c("feeding.guild", "season", "Biomass", "Var1"), sep = " ") %>%
    tidyr::unite("Var", feeding.guild:season, sep = " ") %>%
    dplyr::mutate(stat = dplyr::recode(Var1, Index = "Mean",
                                Standard = "SD")) %>%
    dplyr::select(-Biomass, -Var1) %>%
    dplyr::group_by(Var, Time, EPU) %>%
    tidyr::spread(stat, Value) %>%
    dplyr::mutate(upper = Mean + (2*SD),
                  lower = Mean - (2*SD))

  agg_bio<-agg %>% dplyr::filter(EPU == filterEPUs,
                                 Time >= 1968) %>%
    dplyr::group_by(Var, EPU) %>%
    dplyr::mutate(hline = mean(Mean, na.rm = T)) %>%
    dplyr::ungroup()

  agg_bio$Var <- factor(agg_bio$Var,levels = c("Piscivore Spring",
                                               "Piscivore Fall",
                                               "Benthivore Spring",
                                               "Benthivore Fall",
                                               "Planktivore Spring",
                                               "Planktivore Fall",
                                               "Benthos Spring",
                                               "Benthos Fall"))

  series.col <- rep("black",length(unique(agg_bio$Var)))

  facet_names <- list("Piscivores" = expression("Piscivores"),
                      "Planktivores" = expression("Planktivores"),
                      "Benthivores" = expression("Benthivores"),
                      "Benthos" = expression("Benthos"))

  if(report == "MidAtlantic"){
    #Get NEAMAP
    neamap <- ecodata::mab_inshore_survey %>%
      tidyr::separate(Var, into = c("Var",  "Val"), sep = "-") %>%
      tidyr::pivot_wider(names_from = Val, values_from = Value) %>%
      dplyr::mutate(Value = as.numeric(Value),
                    CV = as.numeric(CV)) %>%
      dplyr::group_by(Var) %>%
      dplyr::mutate(hline = mean(Value),
                    SD = Value * CV, #calculate SD from CV
                    upper = Value + (2*SD),
                    lower = Value - (2*SD))

    neamap$Var <- factor(neamap$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                               "Benthivore Spring", "Benthivore Fall",
                                               "Planktivore Spring", "Planktivore Fall",
                                               "Benthos Spring", "Benthos Fall"))
    neamap.1<-neamap %>%
      dplyr::filter(stringr::str_detect(Var,"Piscivore"))
    neamap.2<-neamap %>%
      dplyr::filter(stringr::str_detect(Var,"Benthivore"))
    neamap.3<-neamap %>%
      dplyr::filter(stringr::str_detect(Var,"Planktivore"))
    neamap.4<-neamap %>%
      dplyr::filter(stringr::str_detect(Var,"Benthos"))

  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  # Build each group plot, assemble below
  ## Piscivore
  p1<-agg_bio %>%
    dplyr::filter(stringr::str_detect(Var,"Piscivore")) %>%
    ggplot2::ggplot() +

    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                      ymin = -Inf, ymax = Inf) +
    #Test for trend and add lines
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Mean,
                                   color = Var),
                      alpha = setup$trend.alpha, size = setup$trend.size) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                         alpha = 0.5,
                         fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Mean),size = setup$lwd-0.5) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Mean),size = setup$pcex-0.5) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
    ggplot2::guides(color = FALSE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     group = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::facet_wrap(Var~.,ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
    #ylim(0, 1200)+
    ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
    ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   axis.title.x=ggplot2::element_blank())

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p1 <- p1 +
      ggplot2::geom_ribbon(data = neamap.1,
                           ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                           alpha = 0.5,
                           fill = "pink")+
      ggplot2::geom_line(data = neamap.1,
                         ggplot2::aes(x = Time, y = Value),
                         color = "#ca0020")+
      ggplot2::geom_point(data = neamap.1,
                          ggplot2::aes(x = Time, y = Value),
                          size = setup$pcex-0.5,
                          color = "#ca0020")
  }


  ## Benthivore
  p2<-agg_bio %>%
    dplyr::filter(stringr::str_detect(Var,"Benthivore")) %>%
    ggplot2::ggplot() +

    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                      ymin = -Inf, ymax = Inf) +
    #Test for trend and add lines
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Mean,
                                   color = Var),
                      alpha = setup$trend.alpha, size = setup$trend.size) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                         alpha = 0.5,
                         fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Mean),size = setup$lwd-0.5) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Mean),size = setup$pcex-0.5) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
    ggplot2::guides(color = FALSE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     group = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::facet_wrap(Var~.,ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
    #ylim(0, 1200)+
    ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
    ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   axis.title.x=ggplot2::element_blank())

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p2 <- p2 +
      ggplot2::geom_ribbon(data = neamap.2,
                           ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                           alpha = 0.5,
                           fill = "pink")+
      ggplot2::geom_line(data = neamap.2,
                         ggplot2::aes(x = Time, y = Value),
                         color = "#ca0020")+
      ggplot2::geom_point(data = neamap.2,
                          ggplot2::aes(x = Time, y = Value),
                          size = setup$pcex-0.5,
                          color = "#ca0020")
  }


  ### Planktivore
  p3<-agg_bio %>%
    dplyr::filter(stringr::str_detect(Var,"Planktivore")) %>%
    ggplot2::ggplot() +

    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                      ymin = -Inf, ymax = Inf) +
    #Test for trend and add lines
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Mean,
                                   color = Var),
                      alpha = setup$trend.alpha, size = setup$trend.size) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                         alpha = 0.5,
                         fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Mean),size = setup$lwd-0.5) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Mean),size = setup$pcex-0.5) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
    ggplot2::guides(color = FALSE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     group = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::facet_wrap(Var~.,ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
    #ylim(0, 1200)+
    ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
    ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   axis.title.x=ggplot2::element_blank())

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p3 <- p3 +
      ggplot2::geom_ribbon(data = neamap.3,
                           ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                           alpha = 0.5,
                           fill = "pink")+
      ggplot2::geom_line(data = neamap.3,
                         ggplot2::aes(x = Time, y = Value),
                         color = "#ca0020")+
      ggplot2::geom_point(data = neamap.3,
                          ggplot2::aes(x = Time, y = Value),
                          size = setup$pcex-0.5,
                          color = "#ca0020")
  }


  ### Benthos
  p4<-agg_bio %>%
    dplyr::filter(stringr::str_detect(Var,"Benthos")) %>%
    #ggplot(aes(x = Time, y = Mean)) +
    ggplot2::ggplot() +
    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                      ymin = -Inf, ymax = Inf) +
    #Test for trend and add lines
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Mean,
                                   color = Var),
                      alpha = setup$trend.alpha, size = setup$trend.size) +
    # ecodata::geom_lm(aes(x = Time, y = Mean,
    #              color = Var),
    #            alpha = trend.alpha, size = trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Mean))+

    #Add time series
    ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                         alpha = 0.5,
                         fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Mean),size = setup$lwd-0.5) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Mean),size = setup$pcex-0.5) +
    ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
    ggplot2::guides(color = FALSE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     group = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::facet_wrap(Var~.,ncol = 2) +
    #Axis and theme
    ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
    #ylim(0, 1200)+
    ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
    ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   axis.title.x=ggplot2::element_blank())

  if (report == "MidAtlantic") {
    #Add NEAMAP
    p4 <- p4 +
      ggplot2::geom_ribbon(data = neamap.4,
                           ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                           alpha = 0.5,
                           fill = "pink")+
      ggplot2::geom_line(data = neamap.4,
                         ggplot2::aes(x = Time, y = Value),
                         color = "#ca0020")+
      ggplot2::geom_point(data = neamap.4,
                          ggplot2::aes(x = Time, y = Value),
                          size = setup$pcex-0.5,
                          color = "#ca0020")
  }


  p <- cowplot::plot_grid(p1, p2, p3, p4, nrow=4)

  return(p)

  # Paste commented original plot code chunk for reference: Mid Atlantic below, NE parts above
  # agg<-ecodata::aggregate_biomass %>%
  # dplyr::filter(EPU == "MAB",
  #               !stringr::str_detect(Var, "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA")) %>% #remove unused datasets
  #   tidyr::separate(Var, c("feeding.guild", "season", "Biomass", "Var1", "Null", "Area"), sep = " ") %>%
  #   tidyr::unite("Var", feeding.guild:season, sep = " ") %>%
  #   dplyr::mutate(stat = recode(Var1, Index = "Mean",
  #                               Standard = "SD")) %>%
  #   dplyr::select(-Biomass, -Var1, -Null, -Units) %>%
  #   # dplyr::filter(!Area == "-") %>%
  #   dplyr::group_by(Var, Time, EPU, Area) %>%
  #   tidyr::pivot_wider(id_cols = c(Var, Time, EPU, Area),names_from =  stat, values_from =  Value) %>%
  #   dplyr::filter(!Mean == "NULL",
  #                 !Time == 2020) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(Mean = as.numeric(Mean),
  #                 SD = as.numeric(SD))
  #
  # agg_bio<-agg %>% dplyr::filter(EPU == epu_abbr,
  #                                Time >= 1968) %>%
  #   dplyr::group_by(Var, EPU) %>%
  #   dplyr::mutate(hline = mean(Mean, na.rm = T),
  #                 upper = Mean + (2*SD),
  #                 lower = Mean - (2*SD)) %>%
  #   dplyr::ungroup()
  #
  #
  # agg_bio$Var <- factor(agg_bio$Var,levels = c("Piscivore Spring",
  #                                              "Piscivore Fall",
  #                                              "Benthivore Spring",
  #                                              "Benthivore Fall",
  #                                              "Planktivore Spring",
  #                                              "Planktivore Fall",
  #                                              "Benthos Spring",
  #                                              "Benthos Fall"))
  # series.col <- rep("black",length(unique(agg_bio$Var)))
  # facet_names <- list("Piscivores" = expression("Piscivores"),
  #                     "Planktivores" = expression("Planktivores"),
  #                     "Benthivores" = expression("Benthivores"),
  #                     "Benthos" = expression("Benthos"))
  # #Get NEAMAP
  # neamap <- ecodata::mab_inshore_survey %>%
  #   tidyr::separate(Var, into = c("Var",  "Val"), sep = "-") %>%
  #   tidyr::pivot_wider(names_from = Val, values_from = Value) %>%
  #   dplyr::mutate(Value = as.numeric(Value),
  #                 CV = as.numeric(CV)) %>%
  #   dplyr::group_by(Var) %>%
  #   dplyr::mutate(hline = mean(Value),
  #                 SD = Value * CV, #calculate SD from CV
  #                 upper = Value + (2*SD),
  #                 lower = Value - (2*SD))
  #
  # neamap$Var <- factor(neamap$Var,levels = c("Piscivore Spring","Piscivore Fall",
  #                                            "Benthivore Spring", "Benthivore Fall",
  #                                            "Planktivore Spring", "Planktivore Fall",
  #                                            "Benthos Spring", "Benthos Fall"))
  # ## Piscivore
  # neamap.1<-neamap %>%
  #   dplyr::filter(stringr::str_detect(Var,"Piscivore"))
  # p1<-agg_bio %>%
  #   dplyr::filter(stringr::str_detect(Var,"Piscivore")) %>%
  #   ggplot2::ggplot() +
  #
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max ,
  #                     ymin = -Inf, ymax = Inf) +
  #   #Test for trend and add lines
  #   ecodata::geom_gls(aes(x = Time, y = Mean,
  #                         color = Var),
  #                     alpha = trend.alpha, size = trend.size) +
  #   # ecodata::geom_lm(aes(x = Time, y = Mean,
  #   #              color = Var),
  #   #            alpha = trend.alpha, size = trend.size) +
  #   #ecodata::geom_lm(aes(x = Time, y = Mean))+
  #
  #   #Add time series
  #   ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                        alpha = 0.5,
  #                        fill = "grey") +
  #   ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  #   ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  #   ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  #   ggplot2::guides(color = FALSE) +
  #   ggplot2::geom_hline(aes(yintercept = hline,
  #                           group = Var),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty)+
  #   ggplot2::facet_wrap(Var~.,ncol = 2) +
  #   #Add NEAMAP
  #   ggplot2::geom_ribbon(data = neamap.1, aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                        alpha = 0.5,
  #                        fill = "pink")+
  #   ggplot2::geom_line(data = neamap.1, aes(x = Time, y = Value),
  #                      color = "#ca0020")+
  #   ggplot2::geom_point(data = neamap.1, aes(x = Time, y = Value),
  #                       size = pcex-0.5,
  #                       color = "#ca0020")+
  #
  #   #Axis and theme
  #   ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #   #ylim(0, 1200)+
  #   ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  axis.title.x=element_blank())
  #
  # ## Benthivore
  # neamap.2<-neamap %>%
  #   dplyr::filter(stringr::str_detect(Var,"Benthivore"))
  # p2<-agg_bio %>%
  #   dplyr::filter(stringr::str_detect(Var,"Benthivore")) %>%
  #   ggplot2::ggplot() +
  #
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max ,
  #                     ymin = -Inf, ymax = Inf) +
  #   #Test for trend and add lines
  #   ecodata::geom_gls(aes(x = Time, y = Mean,
  #                         color = Var),
  #                     alpha = trend.alpha, size = trend.size) +
  #   # ecodata::geom_lm(aes(x = Time, y = Mean,
  #   #              color = Var),
  #   #            alpha = trend.alpha, size = trend.size) +
  #   #ecodata::geom_lm(aes(x = Time, y = Mean))+
  #
  #   #Add time series
  #   ggplot2::geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                         alpha = 0.5,
  #                         fill = "grey") +
  #   ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  #   ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  #   ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  #   ggplot2::guides(color = FALSE) +
  #   ggplot2::geom_hline(aes(yintercept = hline,
  #                           group = Var),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty)+
  #   ggplot2::facet_wrap(Var~.,ncol = 2) +
  #   #Add NEAMAP
  #   ggplot2::geom_ribbon(data = neamap.2, aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                        alpha = 0.5, fill = "pink")+
  #   ggplot2::geom_line(data = neamap.2, aes(x = Time, y = Value),
  #                      color = "#ca0020")+
  #   ggplot2::geom_point(data = neamap.2, aes(x = Time, y = Value),
  #                       size = pcex-0.5,
  #                       color = "#ca0020")+
  #
  #   #Axis and theme
  #   ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #   ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  axis.title.x=element_blank())
  #
  #
  # ### Planktivore
  # neamap.3<-neamap %>%
  #   dplyr::filter(stringr::str_detect(Var,"Planktivore"))
  # p3<-agg_bio %>%
  #   dplyr::filter(stringr::str_detect(Var,"Planktivore")) %>%
  #   ggplot2::ggplot() +
  #
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max ,
  #                     ymin = -Inf, ymax = Inf) +
  #   #Test for trend and add lines
  #   ecodata::geom_gls(aes(x = Time, y = Mean,
  #                         color = Var),
  #                     alpha = trend.alpha, size = trend.size) +
  #   # ecodata::geom_lm(aes(x = Time, y = Mean,
  #   #              color = Var),
  #   #            alpha = trend.alpha, size = trend.size) +
  #   #ecodata::geom_lm(aes(x = Time, y = Mean))+
  #
  #   #Add time series
  #   ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                        alpha = 0.5,
  #                        fill = "grey") +
  #   ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  #   ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  #   ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  #   ggplot2::guides(color = FALSE) +
  #   ggplot2::geom_hline(aes(yintercept = hline,
  #                           group = Var),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty)+
  #   ggplot2::facet_wrap(Var~.,ncol = 2) +
  #   #Add NEAMAP
  #   ggplot2::geom_ribbon(data = neamap.3, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
  #                        fill = "pink", alpha = 0.5) +
  #   ggplot2::geom_line(data = neamap.3, aes(x = Time, y = Value),
  #                      color = "#ca0020")+
  #   ggplot2::geom_point(data = neamap.3, aes(x = Time, y = Value),
  #                       size = pcex-0.5,
  #                       color = "#ca0020")+
  #
  #   #Axis and theme
  #   ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #   #ylim(0, 600)+
  #   ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  axis.title.x=element_blank())
  #
  # ### Benthos
  # neamap.4<-neamap %>%
  #   dplyr::filter(stringr::str_detect(Var,"Benthos"))
  # p4<-agg_bio %>%
  #   dplyr::filter(stringr::str_detect(Var,"Benthos")) %>%
  #   #ggplot(aes(x = Time, y = Mean)) +
  #   ggplot2::ggplot() +
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max ,
  #                     ymin = -Inf, ymax = Inf) +
  #   #Test for trend and add lines
  #   ecodata::geom_gls(aes(x = Time, y = Mean,
  #                         color = Var),
  #                     alpha = trend.alpha, size = trend.size) +
  #   # ecodata::geom_lm(aes(x = Time, y = Mean,
  #   #              color = Var),
  #   #            alpha = trend.alpha, size = trend.size) +
  #   #ecodata::geom_lm(aes(x = Time, y = Mean))+
  #   #Add time series
  #   ggplot2::geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #                         alpha = 0.5,
  #                         fill = "grey") +
  #   ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  #   ggplot2::geom_point(aes(x = Time, y = Mean), size = pcex-0.5) +
  #   ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  #   ggplot2::guides(color = FALSE) +
  #   ggplot2::geom_hline(aes(yintercept = hline,
  #                           group = Var),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty)+
  #   ggplot2::facet_wrap(Var~.,ncol = 2) +
  #   #Add NEAMAP
  #   ggplot2::geom_ribbon(data = neamap.4, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
  #                        fill = "pink", alpha = 0.5) +
  #   ggplot2::geom_line(data = neamap.4, aes(x = Time, y = Value),
  #                      color = "#ca0020")+
  #   ggplot2::geom_point(data = neamap.4, aes(x = Time, y = Value),
  #                       size = pcex-0.5,
  #                       color = "#ca0020")+
  #
  #   ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #
  #   ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  axis.title.x=element_blank())
  # cowplot::plot_grid(p1, p2, p3, p4, nrow=4)
  #
  #

}
