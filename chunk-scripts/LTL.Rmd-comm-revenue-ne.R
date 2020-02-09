
#Filtering and aggregation step
rev_agg <- ecodata::comdat %>% 
  filter(str_detect(Var, "Revenue"),
         !str_detect(Var, "prop|Other|MAFMC"), #Remove proportions, "Other" category species, NEFMC managed species in MAB
         EPU %in% epu_abbr,
         Time >= 1986) %>% 
  mutate(Status = ifelse(str_detect(Var, "Revenue weight"), 
                         "Managed","Total")) %>% #Create groups for aggregation
  group_by(EPU,Status, Time) %>% 
  dplyr::summarise(Total = sum(Value)) %>% 
  group_by(EPU,Status) %>% 
  mutate(hline = mean(Total))

series.col <- c("indianred","black")

#Plotting
gom_rev_agg <- rev_agg %>% filter(EPU == "GOM") %>% 
ggplot() +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+  
  
  #lines
  geom_gls(aes(x = Time, y = Total,
               group = Status),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
  geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +

  #axes
  scale_y_continuous(labels = function(l){trans = l / 1000000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ylab(expression("Revenue (10"^6*"USD)")) +
  geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() +
    ggtitle("Gulf of Maine")

gb_rev_agg <- rev_agg %>% filter(EPU == "GB") %>% 
ggplot() +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+  
  
  #lines
  geom_gls(aes(x = Time, y = Total,
               group = Status),
             alpha = trend.alpha, size = trend.size) +
  geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
  geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +

  #axes
  scale_y_continuous(labels = function(l){trans = l / 1000000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  ylab(expression("Revenue (10"^6*"USD)")) +
  geom_hline(aes(yintercept = hline,
               color = Status),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() +
    ggtitle("Georges Bank")

cowplot::plot_grid(gb_rev_agg, gom_rev_agg, ncol = 2)

# rev_inline <- rev_agg %>% 
#   dplyr::select(Status, Time, Total) %>% 
#   spread(.,Status, Total) %>% 
#   group_by(EPU) %>% 
#   mutate(percent_managed = round(Managed/Total * 100)) %>% 
#   filter(Time > latest_landings_data - 5, Time <= latest_landings_data) %>% 
#   pull(percent_managed)
# 
# min_rev_perc <- min(rev_inline)
# max_rev_perc <- max(rev_inline)
