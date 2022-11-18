
recdat <- ecodata::recdat %>% 
  dplyr::filter(EPU == region_abbr) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))

ylim_re <- c(5e6, 30e6)
ylim_rd <- c(1.75,2.75)
ylim_ra  <- c(1e6, 3.5e6)

# #Create dataframe for label locations
# label_loc <- data.frame(xloc = min(recdat$Time)+0.3,
#                         yloc = c(ylim_re[2]*0.975,
#                                  ylim_rd[2]*0.975,
#                                  ylim_ra[2]*0.975),
#                         labels = LETTERS[1:3],
#                         Var = c("Recreational Effort",
#                                 "Recreational fleet effort diversity across modes",
#                                 "Recreational anglers"))

series.col <- "black"
# x.shade.min <- max(recdat$Time, na.rm = T) - 9
# x.shade.max <- max(recdat$Time, na.rm = T)

rec_effort <- recdat %>% 
  dplyr::filter(Var == "Recreational Effort") %>% 
  ggplot2::ggplot() + 
 #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #label
  # annotate("text", 
  #          x = label_loc[label_loc$Var == "Recreational Effort",]$xloc,
  #          y = label_loc[label_loc$Var == "Recreational Effort",]$yloc,
  #          label = label_loc[label_loc$Var == "Recreational Effort",]$labels,
  #          size = letter_size)+
  ecodata::geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  # ecodata::geom_lm(aes(x = Time, y = Value,
  #              group = Var))+
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  
  ggplot2::scale_x_continuous(expand = c(0.01, 0.02)) +
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000000}, limits = ylim_re)+
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::ggtitle("Recreational effort")+
  ggplot2::ylab(expression("Days fished (10"^6*" N)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  ecodata::theme_ts() +
  ecodata::theme_title()



rec_effort
