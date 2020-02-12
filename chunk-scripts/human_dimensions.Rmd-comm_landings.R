
#Get data for plotting
## Apex pred
apex<-ecodata::hms_landings_weight

apex$sp.group <- ifelse(grepl("Shark", apex$Var, ignore.case = T), "shark", 
         ifelse(grepl("TUNA", apex$Var, ignore.case = T), "tuna", "swordfish"))

apex<-apex %>% 
  dplyr::group_by(sp.group, Region, Time) %>% 
  dplyr::summarise(Value = sum(Value)) %>% 
  dplyr::mutate(Units = c("metric tons"),
         feeding.guild = factor(c("Apex Predator")),
         Value = (Value/2024.6)) %>% 
  dplyr::rename(EPU = Region, 
           Var = sp.group) %>% 
  dplyr::mutate(grouping = factor(c("total")))


#Define constants for figure plot
series.col <- c("indianred","black")

##Plot
p1<-apex %>% 
  filter(EPU == "MA") %>% 
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
         Time >= 1986,
         EPU == epu_abbr)

#Total landings
total_landings <- ecodata::comdat  %>%
  filter(!str_detect(Var, "managed species"),
         !str_detect(Var, "Other"),
         str_detect(Var, "Landings"),
         Time >= 1986,
         EPU == epu_abbr)

#Assign feeding guild column for plotting with ggplot
landings <- 
  rbind(managed_landings, total_landings) %>%
  filter(Var != "Apex Predator Landings") %>% 
  mutate(feeding.guild = str_extract(Var,paste(feeding.guilds1, collapse = "|")),
         grouping = factor(ifelse(str_detect(Var,council_abbr), "managed",
                                  ifelse(str_detect(Var, "JOINT"), "joint","total"))),
         Var = paste(word(feeding.guild), grouping)) %>% 
  mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))

#Add JOINT landings to MANAGED landings and remove
landings[landings$Var ==  "Piscivore managed",]$Value <- landings[landings$Var ==  "Piscivore managed",]$Value + landings[landings$Var ==  "Piscivore joint",]$Value

landings <- landings %>%
  filter(Var != "Piscivore joint")  %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value))


#facet names for titles
facet_names <- list("Apex" = expression("Apex"),
                    "Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))


p2<- landings %>% 
  filter(EPU == "MAB") %>% 
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
  facet_wrap(feeding.guild~., scales = "free", labeller = label, ncol = 1)+
  
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Landings (10"^3*"metric tons)")) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0))

plot_grid(p1, p2, ncol = 1, align = 'v', rel_heights = c(1,4))
