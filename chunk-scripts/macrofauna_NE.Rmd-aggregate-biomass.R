
agg<-ecodata::aggregate_biomass %>% 
  dplyr::filter(!stringr::str_detect(Var, "Apex|inshore|offshore|managed|NEFMC|MAFMC|JOINT|NA")) %>% #remove unused datasets
  tidyr::separate(Var, c("feeding.guild", "season", "Biomass", "Var1"), sep = " ") %>% 
  tidyr::unite("Var", feeding.guild:season, sep = " ") %>% 
  dplyr::mutate(stat = recode(Var1, Index = "Mean", 
                      Standard = "SD")) %>% 
  dplyr::select(-Biomass, -Var1) %>% 
  dplyr::group_by(Var, Time, EPU) %>% 
  tidyr::spread(stat, Value) %>% 
  dplyr::mutate(upper = Mean + (2*SD), 
         lower = Mean - (2*SD))

agg_bio<-agg %>% dplyr::filter(EPU %in% c("GOM","GB"),
         Time >= 1968) %>% 
  dplyr::group_by(Var, EPU) %>% 
  dplyr::mutate(hline = mean(Mean, na.rm = T)) %>% 
  dplyr::ungroup() 

agg_bio$Var <- factor(agg_bio$Var,levels = c("Piscivore Spring",
                                                   "Piscivore Fall",
                                                    "Benthivore Spring",
                                                   "Benthivore Fall",
                                                    "Planktivore Spring",
                                                    "Planktivore Fall",
                                                    "Benthos Spring",
                                                   "Benthos Fall"))
series.col <- rep("black",length(unique(agg_bio$Var)))


#facet names for titles
facet_names <- list("Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))
gb_surv <- agg_bio %>% 
  dplyr::filter(EPU == "GB") 

## plot 1
p1<- gb_surv %>% 
  dplyr::filter(str_detect(Var, "Piscivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
 
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("GB NEFSC BTS") +
  ecodata::theme_title()+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

## plot 2
p2<- gb_surv %>% 
  dplyr::filter(str_detect(Var, "Benthivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

## plot 3
p3<- gb_surv %>% 
  dplyr::filter(str_detect(Var, "Planktivore")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") +
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
 
  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +

  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0),
        axis.title.x=element_blank())

## plot 4
p4<- gb_surv %>% 
  dplyr::filter(str_detect(Var, "Benthos")) %>% 
  ggplot2::ggplot(aes(x = Time, y = Mean)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  ecodata::geom_gls(aes(x = Time, y = Mean,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(lower,0), ymax = upper), 
              alpha = 0.5,
              fill = "grey") + 
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ggplot2::scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~., ncol = 2) +

  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Biomass (kg tow"^-1*")")) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))

cowplot::plot_grid(p1, p2, p3, p4, nrow=4)
