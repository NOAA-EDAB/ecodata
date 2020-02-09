
comm_div <- ecodata::commercial_div %>% 
  filter(EPU == region_abbr) %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value))

ylim_fc <- c(min(comm_div[comm_div$Var == "Fleet count",]$Value) - 10, max(comm_div[comm_div$Var == "Fleet count",]$Value) + 10 )
ylim_fd <- c(3, max(comm_div[comm_div$Var == "Fleet diversity in revenue",]$Value) + 3 )

# #Create dataframe for label locations
# label_loc <- data.frame(xloc = min(comm_div$Time)+0.25,
#                         yloc = c(ylim_fc[2]*0.95, ylim_fd[2]*0.95),
#                         labels = LETTERS[1:2],
#                         Var = c("Fleet count","Fleet diversity in revenue"))

series.col = c("black")

fleet_count <- comm_div %>% 
  filter(Var == "Fleet count") %>% 
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #label
  # annotate("text", x = label_loc[label_loc$Var == "Fleet count",]$xloc,
  #          y = label_loc[label_loc$Var == "Fleet count",]$yloc,
  #          label = label_loc[label_loc$Var == "Fleet count",]$label,
  #          size = letter_size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  ylim(ylim_fc)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ggtitle("Fleet count") +
  ylab(expression("Count (n)")) +
  xlab("")+
  geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts()

fleet_div <- comm_div %>% 
  filter(Var == "Fleet diversity in revenue") %>% 
  ggplot() + 
 #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #label
  # annotate("text", x = label_loc[label_loc$Var == "Fleet diversity in revenue",]$xloc,
  #          y = label_loc[label_loc$Var == "Fleet diversity in revenue",]$yloc,
  #          label = label_loc[label_loc$Var == "Fleet diversity in revenue",]$label,
  #          size = letter_size) +
  geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
  geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
  ylim(ylim_fd)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ggtitle("Fleet diversity in revenue") +
  ylab(expression("Effective Shannon")) +

  geom_hline(aes(yintercept = hline,
               color = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
 theme_ts()

cowplot::plot_grid(fleet_count, fleet_div, ncol = 1, align = "hv") + 
  theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
