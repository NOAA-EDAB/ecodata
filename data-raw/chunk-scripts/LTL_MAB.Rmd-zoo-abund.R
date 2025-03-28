
ecodata::zoo_abundance_anom %>%
   dplyr::filter(EPU == "MAB", 
                 stringr::str_detect(Var, "LgCopepods|SmCopepods"), 
                 Time > 1991) %>%
   dplyr::group_by(Time) %>% 
   dplyr::mutate(Value = as.numeric(Value), 
                 hline = 0) %>% 
   ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
       xmin = x.shade.min , xmax = x.shade.max,
       ymin = -Inf, ymax = Inf) +
   #geom_gls() +
   ggplot2::geom_line() +
   ggplot2::geom_point() +
   ggplot2::ylab("Abundance anomaly") +
   ggplot2::xlab(element_blank())+
   ggplot2::ggtitle("Small and large-bodied copepod abundance anomaly") +
   ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
   ggplot2::geom_hline(aes(yintercept = hline),
            size = hline.size,
            alpha = hline.alpha,
            linetype = hline.lty)+
   ecodata::theme_ts()+
   ggplot2::theme(plot.title = element_text(size = 7),
                  strip.text=element_text(hjust=0,
                                 face = "italic"), 
         legend.title = element_blank())+
  ecodata::theme_title()
