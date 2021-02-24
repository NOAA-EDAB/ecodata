
#Filtering and aggation step
rev_agg <- ecodata::comdat %>% 
  dplyr::filter(stringr::str_detect(Var, "Revenue"),
         !stringr::str_detect(Var, "Apex|prop|Other|MAFMC"), #Remove proportions, "Other" category species, NEFMC managed species in MAB
         EPU %in% c("GOM", "GB"),
         Time >= 1986) %>% 
  dplyr::mutate(Status = ifelse(str_detect(Var, "managed"), 
                         "Managed","Total")) %>% #Create groups for aggregation
  dplyr::group_by(EPU,Status, Time) %>% 
  dplyr::summarise(Total = sum(Value)) %>% 
  dplyr::group_by(EPU,Status) %>% 
  dplyr::mutate(hline = mean(Total))

series.col <- c("indianred","black")

#Plotting
gom_rev_agg <- rev_agg %>% dplyr::filter(EPU == "GOM") %>% 
ggplot2::ggplot() +
  
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
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ylab(expression("Revenue (10"^6*"USD)")) +
  
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
    ggtitle("Gulf of Maine")+
  ecodata::theme_title()

gb_rev_agg <- rev_agg %>% dplyr::filter(EPU == "GB") %>% 
ggplot2::ggplot() +
  
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
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ylab(expression("Revenue (10"^6*"USD)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
    ggtitle("Georges Bank")+
  ecodata::theme_title()

cowplot::plot_grid(gb_rev_agg, gom_rev_agg, ncol = 2)
