
#Filter data into two dataframes for plotting

indicators <- ecodata::bennet %>% 
  dplyr::filter(!EPU == "MAB") %>% 
  dplyr::filter(stringr::str_detect(Var, pattern="Benth"),
         #!Var == "Total Revenue Change - Bennet", 
         !Time < 1985) %>% 
  dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Benthos Value Index - Bennet","Benthos Price Index - Bennet","Benthivore Value Index - Bennet","Benthivore Price Index - Bennet"),
                                    to = c("Benthos Volume","Benthos Price","Benthivore Volume","Benthivore Price"))) 
revchange1<-indicators %>% 
  dplyr::group_by(Time, EPU) %>% 
  dplyr::summarise(revchange.line = sum(Value))


revchange <- ecodata::bennet %>% 
  dplyr::filter(!EPU == "MAB",
         Var %in% c("Total Revenue Change - Bennet"),
         !Time<1985)
revchange.gom<-revchange1 %>% 
  dplyr::filter(EPU == "GOM") 
revchange.gb<-revchange1 %>% 
  dplyr::filter(EPU == "GB")
#custom bar fill color (color-blind friendly)
ind_fill <- c("#a6cee3", "#b2df8a")

#limits
y.lim <- c(-350,350)

#plot

gom_bennet <- indicators %>% 
  dplyr::filter(EPU == "GOM", 
         stringr::str_detect(Var, pattern="Benthivore")) %>% 
ggplot2::ggplot()+
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  #guides(color = F, fill = F)+
  ggplot2::geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
  ggplot2::scale_fill_manual(name = "Indicators", values = ind_fill, guide = FALSE) +
  ggplot2::geom_line(data = revchange.gom, 
            aes(x = Time, y = revchange.line, colour = "$"))+
  ggplot2::scale_colour_grey(name ="Revenue Change") +
  ggplot2::ggtitle("Gulf of Maine Benthivore Component")+
  ggplot2::labs(y="Value $1,000,000 ($2015)") +
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim,
                     expand = c(0.01, 0.01)) +
  ecodata::theme_ts() +
  ggplot2::theme(title = element_text(size = 10))+
  ggplot2::theme(legend.position="bottom", legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent"), 
        legend.title = element_blank(), legend.text = element_blank())

gb_bennet <- indicators %>% 
  dplyr::filter(EPU == "GB", 
         stringr::str_detect(Var, pattern="Benthos")) %>% 
ggplot2::ggplot()+
  
  #Highlight last ten years
 ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  
  ggplot2::geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
  ggplot2::scale_fill_manual(name = "Indicators", values = ind_fill) +
  ggplot2::geom_line(data = revchange.gb, 
            aes(x = Time, y = revchange.line, colour = "$"))+
  ggplot2::scale_colour_grey(name ="Revenue Change") +
  ggplot2::ggtitle("Georges Bank Benthos Component")+
  ggplot2::labs(y="") +
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), 
                              limits = y.lim, expand = c(0.01, 0.01)) +
  ecodata::theme_ts() +
  ggplot2::theme(title = element_text(size = 10)) +
  ggplot2::theme(legend.position="bottom", legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent"), 
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8)) +
  ggplot2::guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 0))

#cowplot::plot_grid(gom_bennet, gb_bennet, ncol = 2, rel_widths = c(0.7,1))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(gb_bennet)
p3 <- gridExtra::grid.arrange(gridExtra::arrangeGrob(gb_bennet + theme(legend.position="none"),
                         gom_bennet + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(6, 1))
