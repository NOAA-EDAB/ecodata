
diet_div <- ecodata::seabird_ne %>% 
  dplyr::filter(!stringr::str_detect(Var, "Productvity"),
         !stringr::str_detect(Var, "Sum")) %>% 
  dplyr::mutate(Island = word(Var, 1),
         Var = word(Var, 4)) %>% 
  dplyr::group_by(Island, Time) %>%
  dplyr::summarise(evenness = diversity(Value)/log(specnumber(Value)),
                   shannon = diversity(Value),
                   simpson = diversity(Value, index = "simpson")) %>% 
  tidyr::gather(.,Var,Value,-Island, -Time) %>% 
  dplyr::group_by(Var, Time) %>%
  dplyr::summarize(Value = mean(Value, na.rm = T),
                   sd = sd(Value, na.rm = T),
                   n = n()) %>%
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T))


shannon <- diet_div %>% 
  dplyr::filter(Var == "shannon") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value)) +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  #geom_gls() +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01),limits = c(1992,2018)) +
  ggplot2::ggtitle("Common tern diet diversity")+
  ggplot2::ylab(expression("Shannon Diversity")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ecodata::theme_title()

shannon 
