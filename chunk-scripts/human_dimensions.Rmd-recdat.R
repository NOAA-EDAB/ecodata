
recdat <- ecodata::recdat %>% 
  dplyr::filter(EPU == region_abbr) %>% 
  dplyr::group_by(Var) %>% 
  dplyr::mutate(hline = mean(Value))

ylim_re <- c(2e7, 7e7)
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
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #label
  # annotate("text", 
  #          x = label_loc[label_loc$Var == "Recreational Effort",]$xloc,
  #          y = label_loc[label_loc$Var == "Recreational Effort",]$yloc,
  #          label = label_loc[label_loc$Var == "Recreational Effort",]$labels,
  #          size = letter_size)+
  geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(l){trans = l / 1000000}, limits = ylim_re)+
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ggtitle("Recreational effort")+
  ylab(expression("Days fished (10"^6*" N)")) +
  xlab("")+
  geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() 

rec_div <- recdat %>% 
  dplyr::filter(Var == "Recreational fleet effort diversity across modes") %>% 
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  # annotate("text", 
  #          x = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$xloc,
  #          y = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$yloc,
  #          label = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$labels,
  #          size = letter_size)+
  geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  ylim(ylim_rd)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ggtitle("Rec. fleet effort diversity")+
  ylab(expression("Effective Shannon")) +
  xlab("")+
  geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts()

rec_div_catch <- recdat %>% 
  dplyr::filter(Var == "Recreational Diversity of Catch") %>% 
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
    # annotate("text", 
    #        x = label_loc[label_loc$Var == "Recreational anglers",]$xloc,
    #        y = label_loc[label_loc$Var == "Recreational anglers",]$yloc,
    #        label = label_loc[label_loc$Var == "Recreational anglers",]$labels,
    #        size = letter_size)+
  geom_gls(aes(x = Time, y = Value,
               group = Var),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +

  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ggtitle("Rec. diversity of catch")+
  ylab(expression("Effective Shannon")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts()


cowplot::plot_grid(rec_effort, 
                   rec_div, 
                   rec_div_catch,
                   ncol = 1, 
                   align = "hv") +
    theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
