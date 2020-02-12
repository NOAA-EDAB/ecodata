
##Plot
apex_landings<-apex %>% 
  filter(EPU == "NE") %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value)) %>% 

  ggplot(aes(x = Time, y = Value, color = Var)) +
  
  #Add time series
  geom_line(size = lwd) +
  geom_point(size = pcex) +
  stat_summary(fun.y = sum, color = "black", geom = "line")+
  #scale_color_manual(values = series.col, aesthetics = "color")+
  #guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 color = Var,
                 size = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), limits = c(1985, 2018)) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0), 
        legend.position = c(0.4, 0.7), 
        legend.direction = "horizontal", 
        legend.title = element_blank())+
  ylab("")

#Managed landings
managed_landings <- ecodata::comdat  %>%
  filter(str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
         !str_detect(Var, "Other"),
         Time >= 1986)

# #Total landings
total_landings <- ecodata::comdat  %>%
  filter(!str_detect(Var, "managed species"),
         !str_detect(Var, "Other"),
         str_detect(Var, "Landings"),
         Time >= 1986)

# #Assign feeding guild column for plotting with ggplot
landings <-
  rbind(managed_landings, total_landings) %>%
  filter(Var != "Apex Predator Landings") %>%
  mutate(feeding.guild = str_extract(Var,paste(feeding.guilds1, collapse = "|")),
         grouping = factor(ifelse(str_detect(Var,council_abbr), "managed",
                                  ifelse(str_detect(Var, "JOINT"), "joint","total"))),
         Var = paste(word(feeding.guild), grouping)) %>%
  mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))
 
# #Add JOINT landings to MANAGED landings and remove
landings[landings$Var ==  "Piscivore managed",]$Value <- landings[landings$Var ==  "Piscivore managed",]$Value + landings[landings$Var ==  "Piscivore joint",]$Value

landings <- landings %>%
  filter(Var != "Piscivore joint")  %>%
  group_by(Var, EPU) %>%
  mutate(hline = mean(Value))

#Define constants for figure plot
series.col <- c("indianred","black") 

#facet names for titles
facet_names <- list("Apex" = expression("Apex"),
                    "Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))


gom_landings<- landings %>% 
  filter(EPU == "GOM") %>% 
  ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               group = Var)) +
  
  #Add time series
  geom_line(size = lwd) +
  geom_point(size = pcex) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 color = grouping,
                 size = grouping),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  #facet_wrap(feeding.guild~., scales = "free", labeller = label, ncol = 1)+
  facet_wrap(~feeding.guild, ncol = 1, scales = "free")+
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Landings (10"^3*"metric tons)")) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0))+
  ggtitle("Gulf of Maine")

gb_landings <- landings %>% filter(EPU == "GB") %>% 
  ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               group = Var)) +
  
  #Add time series
  geom_line(size = lwd) +
  geom_point(size = pcex) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 color = grouping,
                 size = grouping),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  #facet_wrap(feeding.guild~.,scales = "free_y", labeller = label, ncol = 1) +
  facet_wrap(~feeding.guild, ncol = 1, scales = "free")+
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Landings (10"^3*"metric tons)")) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0)) +
  ggtitle("Georges Bank")

plot_grid(apex_landings, gb_landings, gom_landings, ncol = 1, align = 'v', rel_heights = c(1,4,4))
