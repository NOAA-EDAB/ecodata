
landings_rec <- ecodata::recdat %>% 
  dplyr::filter(EPU == region_abbr,
         Var == "Recreational Seafood") %>% 
  dplyr::mutate(hline = mean(Value))

series.col <- "black"

ggplot2::ggplot(data = landings_rec)+
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +

  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Recreational seafood harvest") +
  ggplot2::ylab(expression("Landings (N)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_title()
# 
# landings_rec %>% 
#   dplyr::filter(Time %in% c(max(Time), max(Time-1))) %>% 
#   dplyr::summarise(m = mean(Value))
# 
# landings_rec %>% 
#   dplyr::filter(Time %in% c(max(Time-2), max(Time-3),  max(Time-4))) %>% 
#   dplyr::summarise(m= mean(Value))
