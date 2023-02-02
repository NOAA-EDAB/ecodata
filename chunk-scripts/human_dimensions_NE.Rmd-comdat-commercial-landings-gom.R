
#Managed landings
managed_landings <- ecodata::comdat  %>%
  dplyr::filter(Var %in% c("Planktivore NEFMC managed species - Seafood Landings",
                           "Piscivore NEFMC managed species - Seafood Landings",
                           "Benthivore NEFMC managed species - Seafood Landings",
                           "Apex Predator NEFMC managed species - Seafood Landings",
                           "Benthos NEFMC managed species - Seafood Landings"),
         Time >= 1986) %>% 
  dplyr::mutate(grouping = c("managed")) %>% 
  tidyr::separate(Var, into = c("feeding.guild"), sep = " ")

#Total landings
total_landings <- ecodata::comdat  %>% 
  dplyr::filter(Var %in% c("Planktivore Landings",
                           "Piscivore Landings",
                           "Benthivore Landings",
                           "Apex Predator Landings",
                           "Benthos Landings"),
         Time >= 1986) %>% 
  dplyr::mutate(grouping = c("total")) %>% 
  tidyr::separate(Var, into = c("feeding.guild"), sep = " ")

# #Assign feeding guild column for plotting with ggplot
landings <-
  rbind(managed_landings, total_landings) %>%
  dplyr::filter(!stringr::str_detect(feeding.guild, "Apex")) %>%
  #tidyr::separate(Var, into = c("feeding.guild", "a", "grouping"), sep = " ") %>% 
  dplyr::mutate(#feeding.guild = stringr::str_extract(Var,c(feeding.guilds)),
         #grouping = recode(grouping, "Landings" = "total"),
         Var = paste(feeding.guild, grouping)) %>%
  dplyr::mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))
 
# #Add JOINT landings to MANAGED landings and remove
# landings[landings$Var ==  "Piscivore managed",]$Value <- landings[landings$Var ==  "Piscivore managed",]$Value + landings[landings$Var ==  "Piscivore joint",]$Value

landings <- landings   %>%
  dplyr::group_by(Var, EPU) %>%
  dplyr::mutate(hline = mean(Value))

#Define constants for figure plot
series.col <- c("indianred","black") 


#facet names for titles
facet_names <- list("Apex" = expression("Apex"),
                    "Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))

gom_landings<- landings %>%
  dplyr::filter(EPU == "GOM") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = Time, y = Value,
               group = Var)) +
  #Add time series
  ggplot2::geom_line(size = lwd) +
  ggplot2::geom_point(size = pcex) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 color = grouping,
                 size = grouping),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  #facet_wrap(feeding.guild~., scales = "free", labeller = label, ncol = 1)+
  ggplot2::facet_wrap(~feeding.guild, ncol = 1)+
  #ggplot2::facet_wrap(~feeding.guild, ncol = 1, scales = "free")+
  #Axis and theme
  ggplot2::ylim(0, NA)+
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Landings (10"^3*"metric tons)")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0))+
  ggplot2::ggtitle("Gulf of Maine")+
  ecodata::theme_title()

gom_landings 
