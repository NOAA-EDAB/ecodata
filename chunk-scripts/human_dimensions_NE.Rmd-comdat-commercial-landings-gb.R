
#Managed landings
managed_landings <- ecodata::comdat  %>%
  dplyr::filter(stringr::str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
         !stringr::str_detect(Var, "Other"),
         Time >= 1986)

# #Total landings
total_landings <- ecodata::comdat  %>%
  dplyr::filter(!stringr::str_detect(Var, "managed species"),
         !stringr::str_detect(Var, "Other"),
         stringr::str_detect(Var, "Landings"),
         Time >= 1986)

# #Assign feeding guild column for plotting with ggplot
landings <-
  rbind(managed_landings, total_landings) %>%
  dplyr::filter(Var != "Apex Predator Landings") %>%
  dplyr::mutate(feeding.guild = stringr::str_extract(Var,paste(feeding.guilds, collapse = "|")),
         grouping = factor(ifelse(stringr::str_detect(Var,council_abbr), "managed",
                                  ifelse(stringr::str_detect(Var, "JOINT"), "joint","total"))),
         Var = paste(word(feeding.guild), grouping)) %>%
  dplyr::mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))
 
# #Add JOINT landings to MANAGED landings and remove
landings[landings$Var ==  "Piscivore managed",]$Value <- landings[landings$Var ==  "Piscivore managed",]$Value + landings[landings$Var ==  "Piscivore joint",]$Value

landings <- landings %>%
  dplyr::filter(Var != "Piscivore joint")  %>%
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


gb_landings <- landings %>% dplyr::filter(EPU == "GB") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
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
  #facet_wrap(feeding.guild~.,scales = "free_y", labeller = label, ncol = 1) +
  ggplot2::facet_wrap(~feeding.guild, ncol = 1)+
  #ggplot2::facet_wrap(~feeding.guild, ncol = 1, scales = "free")+
  #Axis and theme
  ggplot2::ylim(1,NA)+
  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
  ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Landings (10"^3*"metric tons)")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0)) +
  ggplot2::ggtitle("Georges Bank")+
  ecodata::theme_title()

#p1
gb_landings
#, ncol = 1, align = 'v', rel_heights = c(1,4,4))
