
agg<-ecodata::aggregate_biomass %>%
  dplyr::filter(EPU == "MAB",
                !stringr::str_detect(Var, "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA")) %>% #remove unused datasets
  tidyr::separate(Var, c("feeding.guild", "season", "Biomass", "Var1", "Null", "Area"), sep = " ") %>%
  tidyr::unite("Var", feeding.guild:season, sep = " ") %>%
  dplyr::mutate(stat = recode(Var1, Index = "Mean",
                      Standard = "SD")) %>%
  dplyr::select(-Biomass, -Var1, -Null, -Source, -Units) %>%
  # dplyr::filter(!Area == "-") %>%
  dplyr::group_by(Var, Time, EPU, Area) %>%
  tidyr::pivot_wider(id_cols = c(Var, Time, EPU, Area),names_from =  stat, values_from =  Value) %>%
  dplyr::filter(!Mean == "NULL") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Mean = as.numeric(Mean))


agg_bio<-agg %>% dplyr::filter(EPU == epu_abbr,
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
#Get NEAMAP
neamap <- ecodata::mab_inshore_survey %>%
  dplyr::group_by(Var) %>%
  dplyr::mutate(hline = mean(Value),
         SD = Value * CV, #calculate SD from CV
         upper = Value + (2*SD),
         lower = Value - (2*SD))

neamap$Var <- factor(neamap$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                           "Benthivore Spring", "Benthivore Fall",
                                           "Planktivore Spring", "Planktivore Fall",
                                           "Benthos Spring", "Benthos Fall"))
## Piscivore
neamap.1<-neamap %>%
  dplyr::filter(str_detect(Var,"Piscivore"))
p1<-agg_bio %>%
  dplyr::filter(str_detect(Var,"Piscivore")) %>%
  ggplot2::ggplot() +

  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +

  #Add time series
  #ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #            alpha = 0.5,
  #            fill = "grey") +
  ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ggplot2::facet_wrap(Var~.,ncol = 2) +
     #Add NEAMAP
  ggplot2::geom_ribbon(data = neamap.1, aes(x = Time, ymin = pmax(lower,0), ymax = upper),
              alpha = 0.5,
              fill = "pink")+
  ggplot2::geom_line(data = neamap.1, aes(x = Time, y = Value),
            color = "#ca0020")+
  ggplot2::geom_point(data = neamap.1, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #ylim(0, 1200)+
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

## Benthivore
neamap.2<-neamap %>%
  dplyr::filter(str_detect(Var,"Benthivore"))
p2<-agg_bio %>%
  dplyr::filter(str_detect(Var,"Benthivore")) %>%
  ggplot2::ggplot() +

  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +

  #Add time series
  #ggplot2::geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #            alpha = 0.5,
  #            fill = "grey") +
  ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ggplot2::facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
  ggplot2::geom_ribbon(data = neamap.2, aes(x = Time, ymin = pmax(lower,0), ymax = upper),
              alpha = 0.5, fill = "pink")+
  ggplot2::geom_line(data = neamap.2, aes(x = Time, y = Value),
            color = "#ca0020")+
  ggplot2::geom_point(data = neamap.2, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())


### Planktivore
neamap.3<-neamap %>%
  dplyr::filter(str_detect(Var,"Planktivore"))
p3<-agg_bio %>%
  dplyr::filter(str_detect(Var,"Planktivore")) %>%
  ggplot2::ggplot() +

  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +

  #Add time series
  #ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #            alpha = 0.5,
  #            fill = "grey") +
  ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  ggplot2::geom_point(aes(x = Time, y = Mean),size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ggplot2::facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
  ggplot2::geom_ribbon(data = neamap.3, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
                fill = "pink", alpha = 0.5) +
  ggplot2::geom_line(data = neamap.3, aes(x = Time, y = Value),
            color = "#ca0020")+
  ggplot2::geom_point(data = neamap.3, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +
  #ylim(0, 600)+
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

### Benthos
neamap.4<-neamap %>%
  dplyr::filter(str_detect(Var,"Benthos"))
p4<-agg_bio %>%
  dplyr::filter(str_detect(Var,"Benthos")) %>%
  #ggplot(aes(x = Time, y = Mean)) +
  ggplot2::ggplot() +
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  #Add time series
  # ggplot2::geom_ribbon( aes(x = Time, ymin = pmax(lower,0), ymax = upper),
  #             alpha = 0.5,
  #             fill = "grey") +
  ggplot2::geom_line(aes(x = Time, y = Mean),size = lwd-0.5) +
  ggplot2::geom_point(aes(x = Time, y = Mean), size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  ggplot2::facet_wrap(Var~.,ncol = 2) +
       #Add NEAMAP
  ggplot2::geom_ribbon(data = neamap.4, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
              fill = "pink", alpha = 0.5) +
  ggplot2::geom_line(data = neamap.4, aes(x = Time, y = Value),
            color = "#ca0020")+
  ggplot2::geom_point(data = neamap.4, aes(x = Time, y = Value),
             size = pcex-0.5,
             color = "#ca0020")+

  ggplot2::scale_x_continuous(breaks = seq(1970, 2020, by = 10), expand = c(0.01, 0.01)) +

  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())
cowplot::plot_grid(p1, p2, p3, p4, nrow=4)
