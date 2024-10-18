
MAB<- ecodata::habitat_diversity %>% 
  dplyr::filter(!Var == "Shannon", 
                EPU == "MAB") %>% 
  tidyr::separate(Var, into = c("Var", "Richness")) %>% 
  tidyr::pivot_wider(values_from = "Value", names_from = "Var") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(mean = as.numeric(mean),
                lower = as.numeric(lower),
                upper = as.numeric(upper),
                Time = as.numeric(Time),
                hline = mean(mean))

ecodata::habitat_diversity %>% 
  dplyr::filter(!Var == "Shannon") %>% 
  tidyr::separate(Var, into = c("Var", "Richness")) %>% 
  tidyr::pivot_wider(values_from = "Value", names_from = "Var") %>% 
  dplyr::group_by(EPU) %>% 
  dplyr::mutate(mean = as.numeric(mean),
                lower = as.numeric(lower),
                upper = as.numeric(upper),
                Time = as.numeric(Time),
                hline = mean(mean)) %>% 
  ggplot2::ggplot(aes(x = Time, y = mean, fill = EPU))+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_ribbon(aes(x = Time, ymax = upper, ymin = lower), alpha = 0.3)+
  ggplot2::geom_point(aes(color = EPU))+
  ggplot2::geom_line(aes(color = EPU)) +
  ggplot2::geom_point(data = MAB, aes(x = Time, y = mean, color = EPU), size = 1)+
  ggplot2::geom_line(data = MAB, aes(x = Time, y = mean,color = EPU), size = 1) +
  ggplot2::ylab("Richness") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Species Richness from NEFSC Bottom Trawl Survey") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  # ggplot2::geom_hline(aes(yintercept = hline),
  #          size = hline.size,
  #          alpha = hline.alpha,
  #          linetype = hline.lty)+
  ecodata::theme_ts()+
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"))+
  ecodata::theme_title()
