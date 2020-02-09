
# Set lat/lon window for maps
xmin = -70.5
xmax = -68.25
ymin = 43
ymax = 44.5
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)


island_loc <- data.frame(Island = c("EER","JI","MR","OGI",
                                    "PINWR","SINWR","STI"),
                         Latitude = c(43.861,43.764,43.785,43.650,
                                      43.739,43.886,43.505),
                         Longitude = c(-69.382,-69.909,-68.854,-70.124,
                                       -69.771,-68.742,-70.312))

islands <- ggplot() +
  geom_sf(data = new_england, size = map.lwd) +
  geom_point(data = island_loc, aes(x = Longitude, y = Latitude)) +
  geom_label_repel(data = island_loc, aes(x = Longitude, y = Latitude,
                                          label = Island), nudge_y  = -0.1) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  theme_map() +
  ggtitle("Common tern study sites") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_text(size = 8))

# inset_states <- new_england %>% filter(STATE_NAME %in% c("Maine",
#                                                          "New Hampshire",
#                                                          "Massachusetts"))


# Set lat/lon window for maps
xmin2 = -72
xmax2 = -66.25
ymin2 = 42.5
ymax2 = 47.5
xlims2 <- c(xmin2, xmax2)
ylims2 <- c(ymin2, ymax2)

NE <- ggplotGrob(
ggplot()+
  geom_sf(data = new_england, size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims2, ylim = ylims2) +
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = "black",
           size = 0.1, fill = "transparent") +
  theme_map() +
  xlab("") +
  ylab("") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        legend.key = element_blank(),
        axis.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text=element_text(hjust=0),
        axis.text = element_blank())
)

full_map <- islands +
  annotation_custom(grob = NE, xmin = -69, xmax = -68.25, ymin = 42.9, ymax = 43.6)


full_map
