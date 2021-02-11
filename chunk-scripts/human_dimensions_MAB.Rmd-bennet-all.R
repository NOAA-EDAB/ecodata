
#Filter data into two dataframes for plotting

indicators <- ecodata::bennet %>% 
  dplyr::filter(EPU == epu_abbr) 

indicators <- ecodata::bennet %>% 
  dplyr::filter(EPU == epu_abbr) 

indicators$Var<- gsub( "Predator", "", indicators$Var)
indicators$Var<- gsub( "Value", "Volume", indicators$Var)
indicators<- indicators %>% 
  separate(Var, c("Guild", "Var") ) %>% 
  dplyr::filter(!Var == "Revenue",
                !Guild == "Total", 
         !Time < 1985) %>% 
  dplyr::group_by(Time,  Var) %>% 
  dplyr::mutate(component = sum(Value)) %>% 
  ungroup()

  # dplyr::filter(#stringr::str_detect(Var, pattern="Total"),
  #        !Var == "Total Revenue Change - Bennet", 
  #        !Time < 1985) %>% 
  # dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Total Volume Index - Bennet", "Total Price Index - Bennet"),
  #                                          to = c("Volume","Price"))) %>% 
  # dplyr::group_by(Time) %>% 
  # dplyr::mutate(New = sum(Value)) %>% 
  # dplyr::group_by(Time, Var) %>% 
  # dplyr::mutate(component = sum(Value))

revchange <- ecodata::bennet %>% 
  dplyr::filter(EPU == "MAB",
         #Var %in% c("Total Revenue Change - Bennet"),
         !Time<1985)
#custom bar fill color (color-blind friendly)
#ind_fill <- c("#a6cee3", "#b2df8a", "#000001")

#limits
y.lim <- c(-400,550)

indicators$Guild<-factor(indicators$Guild, levels = c("Apex", "Piscivore", "Planktivore", 
                                                      "Benthivore", "Benthos", "Other"))

#plot
ggplot2::ggplot()+
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_bar(data = indicators, aes(x = Time, y = Value, fill = Guild), stat="identity")+
  #ggplot2::scale_fill_manual(name = "Indicators", values = Guild) +
  ggplot2::geom_line(data = indicators, aes(x = Time, y = component, color = "$"))+
  ggplot2::facet_wrap(~Var)+
  ggplot2::scale_colour_grey(name ="Component") +
  ggplot2::ggtitle("Bennet Indicator")+
  ggplot2::labs(y="Value $1,000,000 ($2015)") +
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), 
                              limits = y.lim, expand = c(0.01, 0.01)) +
  ggplot2::scale_fill_brewer(palette = "Set1")+
  ecodata::theme_ts() +
  ggplot2::xlab(element_blank())+
  ggplot2::theme(title = element_text(size = 10))
