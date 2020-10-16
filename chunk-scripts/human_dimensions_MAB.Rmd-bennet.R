
#Filter data into two dataframes for plotting

indicators <- ecodata::bennet %>% 
  dplyr::filter(EPU == epu_abbr) %>% 
  dplyr::filter(stringr::str_detect(Var, pattern="Total"),
         !Var == "Total Revenue Change - Bennet", 
         !Time < 1985) %>% 
  dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Total Volume Index - Bennet", "Total Price Index - Bennet"),
                                           to = c("Volume","Price"))) %>% 
  dplyr::group_by(Time) %>% 
  dplyr::mutate(New = sum(Value))

revchange <- ecodata::bennet %>% 
  dplyr::filter(EPU == "MAB",
         Var %in% c("Total Revenue Change - Bennet"),
         !Time<1985)
#custom bar fill color (color-blind friendly)
ind_fill <- c("#a6cee3", "#b2df8a")

#limits
y.lim <- c(-450,600)

#plot
ggplot2::ggplot()+
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_bar(data = indicators, aes(x = Time, y = Value, fill = Var), stat="identity")+
  ggplot2::scale_fill_manual(name = "Indicators", values = ind_fill) +
  ggplot2::geom_line(data = revchange, aes(x = Time, y = Value, color = "$"))+
  ggplot2::scale_colour_grey(name ="Revenue Change") +
  ggplot2::ggtitle("Bennet Indicator")+
  ggplot2::labs(y="Value $1,000,000 ($2015)") +
  ggplot2::scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), 
                              limits = y.lim, expand = c(0.01, 0.01)) +
  ecodata::theme_ts() +
  ggplot2::theme(title = element_text(size = 10))
