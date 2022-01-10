
aggregate_prod <- ecodata::seabird_ne %>% 
    dplyr::filter(stringr::str_detect(Var, "Productivity"))  %>% 
  tidyr::separate(Var,c("Island", "COTE", "Spp", "Extra"), sep = " ") %>% 
  mutate(Diet = (paste(Spp, Extra, sep = " " ))) %>% 
  mutate(Diet = (gsub("NA", "", Diet))) %>% 
  dplyr::mutate(Island = plyr::mapvalues(Island, from = c("EER","JI","MR","OGI","PINWR","SINWR","STI"),
                                  to = c("Eastern Egg Rock", "Jenny Island", "Matinicus Rock", 
                                         "Outer Green Island", "Pond Island", "Seal Island",
                                         "Stratton Island"))) %>%
  dplyr::group_by(Time) %>% 
  dplyr::summarise(Mean = mean(Value, na.rm = T),
                   SE = sd(Value, na.rm = T)/sqrt(n()),
                   SD = sd(Value, na.rm = T),
                   n = n()) %>% 
  dplyr::mutate(Mean = ifelse(is.na(SE),NA,Mean),
         se.low = Mean - SE,
         se.high = Mean + SE,
         hline = mean(Mean, na.rm = T))

aggregate_prod %>% 
  ggplot2::ggplot() +
#Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = Time, y = Mean), size = lwd-0.75) +
  ggplot2::geom_point(aes(x = Time, y = Mean), size = pcex-0.75) +
  ecodata::geom_gls(aes(x = Time, y = Mean)) +
  ggplot2::geom_errorbar(aes(x = Time,
                    ymin = se.low,
                  ymax = se.high), 
                width = 0.25) +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2021)) +
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Common tern productivity") +
  ggplot2::ylab(expression("Fledged chicks per nest")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline),
           color = "black",
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_title()
