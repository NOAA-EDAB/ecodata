
df.symbol <- ecodata::wind_port %>% filter(EPU == "NE", 
                                           !Var %in% c("wea_rev", "total_rev", 
                                                     "perc_rev_min", "perc_rev_max")) %>% 
   pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::mutate(City = paste0(City,State)) %>% 
  dplyr::select(City, EJ, Gentrification) %>% 
  pivot_longer(cols = c(EJ, Gentrification), names_to = "Variable") %>% 
  filter(!value == "NA") %>% 
  dplyr::mutate(symbol = recode(Variable, EJ = -7, Gentrification = -3), 
                Variable = recode(Variable,"EJ"= "Mid-High to High EJ Concerns" , 
                                  "Gentrification" ="Mid-High to High Gentrificaiton Concerns"))

# Percentage plot

df.all.perc<- ecodata::wind_port %>% filter(EPU == "NE") %>%
  pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::mutate(ordering = wea_rev,  
                City = paste0(City, State)) %>% 
  pivot_longer(cols = c(perc_rev_min,  perc_rev_max, total_rev), names_to="Var", values_to = "Value") %>% 
  dplyr::arrange(desc(ordering)) %>%
  dplyr::mutate(City = factor(City, levels = unique(City))) %>% 
  dplyr::filter(!Var %in% c("wea_rev", "EJ", "Gentrification")) %>% 
  dplyr::mutate(Var = recode(Var,"perc_rev_min"= "WEA Revenue" , 
                                  "perc_rev_max" ="WEA Revenue Min-Max Range", 
                             "total_rev" = "Non-WEA Revenue"), 
                Var = factor(Var, levels = c("Non-WEA Revenue", 
                                             "WEA Revenue Min-Max Range", 
                                             "WEA Revenue")))
p2<-ggplot2::ggplot()+
  ggplot2::geom_bar(data = df.all.perc, aes(fill = Var, y = reorder(City, ordering), x = Value), stat="identity" )+
  scale_fill_brewer()+
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank(), 
                 legend.box="vertical", legend.margin=margin())+
  ggplot2::geom_point(data = df.symbol, aes(x = symbol,y = City, shape = Variable)) +
  scale_shape_manual(values = c(17, 16)) +
  ggplot2::ggtitle("Port Revenue from Wind Energy Area")+
  ggplot2::xlab(expression("Port Revenue (%)"))+
  ggplot2::ylab(element_blank())+
  ecodata::theme_ts()

p2
