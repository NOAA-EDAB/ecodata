
#Managed landings
managed_landings <- ecodata::comdat  %>%
  dplyr::filter(stringr::str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
         !stringr::str_detect(Var, "Other"),
         Time >= 1986,
         Time <=2019,
         EPU == epu_abbr)

# HMS Landings
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Landings")) %>% 
  separate(Var, c("Var", "trash"), sep = "_") %>% 
  group_by(YEAR) %>% 
  summarise(Value = sum(Value)) %>% 
  rename( Time = YEAR) %>% 
  mutate(Var = c("HMS Landings"), 
         Units = c("metric tons"), 
         EPU = c("MAB")) %>% 
  dplyr::filter(Time <=2019)

#Total landings
total_landings <- ecodata::comdat  %>%
  dplyr::filter(!stringr::str_detect(Var, "managed species"),
         !stringr::str_detect(Var, "Other"),
         !stringr::str_detect(Var, "Apex"),
         stringr::str_detect(Var, "Landings"),
         Time >= 1986,
         Time <=2019,
         EPU == epu_abbr) %>% 
  rbind(apex)

total_landings_agg <- total_landings %>%
  dplyr::group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Total",hline = mean(Value))

managed_landings_agg <- managed_landings %>%
  dplyr::group_by(Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Managed",hline = mean(Value))

landings_agg <- rbind(total_landings_agg, managed_landings_agg)


ggplot2::ggplot(data = landings_agg)+
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ylab(expression("Landings (10"^3*"mt)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ggplot2::ggtitle("Total Landings")+
  ggplot2::theme(axis.title.y = element_text(size = 7))+
  ecodata::theme_ts()+
  ecodata::theme_title()
