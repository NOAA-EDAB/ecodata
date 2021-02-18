
council_abbr <- "NEFMC"
#Managed landings
managed_landings <- ecodata::comdat  %>%
  dplyr::filter(stringr::str_detect(Var, "NEFMC managed species - Landings weight|JOINT managed species - Landings weight"),
         !stringr::str_detect(Var, "Other"),
         Time >= 1986)

# #Total landings
total_landings <- ecodata::comdat  %>%
  dplyr::filter(!stringr::str_detect(Var, "managed species"),
         !stringr::str_detect(Var, "Other"),
         stringr::str_detect(Var, "Landings"),
         Time >= 1986)

total_landings_agg <- total_landings %>%
  dplyr::group_by(EPU,Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Total",hline = mean(Value))

managed_landings_agg <- managed_landings %>%
  dplyr::group_by(EPU,Time) %>%
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Var = "Managed",hline = mean(Value))

landings_agg <- rbind(total_landings_agg, managed_landings_agg) 

gom_total <- landings_agg %>% dplyr::filter(EPU == "GOM") %>% 
ggplot2::ggplot()+
  
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
  ggplot2::scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ylab(expression("Landings (10"^3*"metric tons)")) +
  ggplot2::xlab(element_blank())+

  ggplot2::geom_hline(aes(yintercept = hline,
               
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::ggtitle("Gulf of Maine - Total Revenue")

gb_total <- landings_agg %>% dplyr::filter(EPU == "GB") %>% 
ggplot2::ggplot()+
  
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
  ggplot2::scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ylab(expression("Landings (10"^3*"metric tons)")) +
  ggplot2::xlab(element_blank())+

  ggplot2::geom_hline(aes(yintercept = hline,
               
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ggplot2::ggtitle("Georges Bank - Total Revenue")

cowplot::plot_grid(gb_total, gom_total, ncol = 2)
