
recdat <- ecodata::recdat %>% 
  dplyr::filter(EPU == region_abbr) %>% 
  dplyr::group_by(Var) %>% 
  plyr::mutate(hline = mean(Value))

ylim_re <- c(5e6, 30e6)
ylim_rd <- c(1.75,2.75)
ylim_ra  <- c(0, 2e6)

x.shade.min <- max(recdat$Time, na.rm = T) - 9
x.shade.max <- max(recdat$Time, na.rm = T)

series.col <- "black"

rec_div <- recdat %>% 
  dplyr::filter(Var == "Recreational fleet effort diversity across modes") %>% 
  dplyr::mutate(hline = mean(Value)) %>% 
  ggplot2::ggplot() + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
    #label
  # annotate("text", 
  #          x = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$xloc,
  #          y = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$yloc,
  #          label = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$labels,
  #          size = letter_size)+
  ecodata::geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  ggplot2::ylim(ylim_rd)+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.02)) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Rec. fleet effort diversity")+
  ggplot2::ylab("Effective Shannon") +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_title()
rec_div
