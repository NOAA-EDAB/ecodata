
#Filtering and aggregation step
rev_agg <- ecodata::comdat %>% 
  dplyr::filter(stringr::str_detect(Var, "Revenue"),
         !stringr::str_detect(Var, "prop|Other|NEFMC"), #Remove proportions, "Other" category species, NEFMC managed species in MAB
         EPU == epu_abbr,
         Time >= 1986) %>% 
  dplyr::mutate(Status = ifelse(str_detect(Var, "Revenue weight"), 
                         "Managed","Total")) %>% #Create groups for aggregation
  dplyr::group_by(Status, Time) %>% 
  dplyr::summarise(Total = sum(Value)) %>% 
  dplyr::group_by(Status) %>% 
  dplyr::mutate(hline = mean(Total))

series.col <- c("indianred","black")

#Plotting
ggplot2::ggplot(data = rev_agg) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+  
  
  #lines
  ecodata::geom_gls(aes(x = Time, y = Total,
               group = Status),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +

  #axes
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
      scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Total revenue") +
  ggplot2::ylab(expression("Revenue (10"^6*"USD)")) +
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()
