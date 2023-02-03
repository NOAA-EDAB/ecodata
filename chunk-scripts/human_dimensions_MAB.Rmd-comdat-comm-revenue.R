
## Apex pred
apex<-ecodata::hms_landings %>% 
  dplyr::filter(stringr::str_detect(Var, "Revenue"), 
                Time<2021) %>% 
  separate(Var, c("Var", "trash"), sep = "_") %>% 
  group_by(Time) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Var = c("HMS Revenue"), 
         Units = c("metric tons"), 
         EPU = c("MAB"))
#Filtering and aggregation step
rev_agg <- ecodata::comdat %>% 
  dplyr::filter(Var %in% c("Piscivore NEFMC managed species - Revenue",
                           "Planktivore NEFMC managed species - Revenue",
                           "Benthivore NEFMC managed species - Revenue", 
                           "Benthos NEFMC managed species - Revenue", 
                           "Piscivore JOINT managed species - Revenue",
                           "Planktivore JOINT managed species - Revenue",
                           "Benthivore JOINT managed species - Revenue", 
                           "Benthos JOINT managed species - Revenue")) %>% 
  rbind(apex) %>% 
  dplyr::mutate(Status = c("Managed")) %>% #Create groups for 
  dplyr::group_by(Status, Time, EPU) %>% 
  dplyr::summarise(Total = sum(Value)) %>% 
  dplyr::group_by(Status, EPU) %>% 
  dplyr::mutate(hline = mean(Total))

rev_total<- ecodata::comdat %>% 
  dplyr::filter(Var == "Revenue") %>% 
  dplyr::mutate(Status = c("Total")) %>% 
  dplyr::group_by(Status, Time, EPU) %>% 
  dplyr::summarise(Total = sum(Value)) %>% 
  dplyr::group_by(Status, EPU) %>% 
  dplyr::mutate(hline = mean(Total))
  
series.col <- c("indianred","black")

rev<- rbind(rev_agg, rev_total) %>% 
  dplyr::filter(EPU == "MAB", 
                Time >1986)
#Plotting
ggplot2::ggplot(data = rev) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+  
  
  #lines
  ecodata::geom_gls(aes(x = Time, y = Total,
               group = Status),
             alpha = trend.alpha, size = trend.size) +
  # ecodata::geom_lm(aes(x = Time, y = Total,
  #              group = Status))+
  ggplot2::geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Total, color = Status), size = 1.5) +

  #axes
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
      scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Total Commercial Revenue") +
  ggplot2::ylab(expression("Revenue (10"^6*"USD)")) +
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
