
cal <- ecodata::CalanusStage %>% 
  dplyr::filter(EPU == "GB") %>% 
  filter(Var %in% c("c3", "c4", "c5", "adt"))

cal$Var <- factor(cal$Var, levels = c("c3", "c4", "c5", "adt"))
cal$season <- factor(cal$season, levels = c("Spring", "Summer", "Fall"))

cal %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::facet_wrap(~season)+
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Calanus Stage Abundance") +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_facet()+
  scale_fill_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))+
  scale_color_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))


cal %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::facet_wrap(~season, ncol = 1, scales = "free")+
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Calanus Stage Abundance") +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  ecodata::theme_facet()+
  scale_fill_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))+
  scale_color_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))

cal %>% 
  ggplot2::ggplot() +
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_line(aes(x = Year, y = meanday))+
  ggplot2::geom_point(aes(x = Year, y = meanday))+
  #ecodata::geom_gls( aes(x = Year, y = meanday)) +
  ggplot2::facet_wrap(~season, ncol = 3, scales = "free")+
  ggplot2::ylab("Mean day of year") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Calanus Stage Mean DOY") +
  #ggplot2::theme(legend.position = "bottom", 
  #               legend.title = element_blank())+
  ecodata::theme_facet()

cal %>% 
  ggplot2::ggplot() +
    ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  ggplot2::geom_line(aes(x = Year, y = ndays))+
  ggplot2::geom_point(aes(x = Year, y = ndays))+
  #ecodata::geom_gls( aes(x = Year, y = ndays)) +
  ggplot2::facet_wrap(~season, ncol = 3, scales = "free")+
  ggplot2::ylab("Number of sampling days") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB Number of sampling days") +
  #ggplot2::theme(legend.position = "bottom", 
  #               legend.title = element_blank())+
  ecodata::theme_facet()
