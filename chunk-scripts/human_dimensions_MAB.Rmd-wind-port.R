
df.symbol <- ecodata::wind_port %>% filter(EPU == "MAB", 
                                           Value >= 3) %>% 
   pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::select(City, EJ, gentrification) %>% 
  pivot_longer(cols = c(EJ, gentrification), names_to = "Variable") %>% 
  filter(!value == "NA") %>% 
  dplyr::mutate(symbol = recode(Variable, EJ = -7, gentrification = -3), 
                Variable = recode(Variable,"EJ"= "Mid-High to High EJ Concerns" , 
                                  "gentrification" ="Mid-High to High Gentrificaiton Concerns"))

# Percentage plot
df.all.perc<- ecodata::wind_port %>% filter(EPU == "MAB") %>%
  pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::mutate(ordering = wea_rev, 
                perc.wea = wea_rev/total_rev * 100, 
                perc.total = 100-perc.wea) %>% 
  pivot_longer(cols = c(perc.wea, perc.total, total_rev), names_to="Var", values_to = "Value") %>% 
  dplyr::arrange(desc(ordering)) %>%
  dplyr::mutate(City = factor(City, levels = unique(City))) %>% 
  dplyr::filter(!Var == "total_rev")
p2<-ggplot2::ggplot()+
  ggplot2::geom_bar(data = df.all.perc, aes(fill = Var, y = reorder(City, ordering), x = Value), stat="identity" )+
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank(), 
                 legend.box="vertical", legend.margin=margin())+
  ggplot2::geom_point(data = df.symbol, aes(x = symbol,y = City, shape = Variable)) +
  scale_shape_manual(values = c(17, 16)) +
  ggplot2::ggtitle("Port Revenue from Wind Energy Area")+
  ggplot2::xlab(expression("Port Revenue (%)"))+
  ggplot2::ylab(element_blank())+
  scale_fill_discrete(name = "", 
                      labels = c("Total Revenue", "Revenue from WEA"))+
  ecodata::theme_ts()
  # guides(fill = guide_legend(nrow = 2), 
  #        point = guide_legend(nrow = 2)) 


p2

# ggpubr::ggarrange(p1, p2,
#                   ncol = 2, 
#                    common.legend = TRUE,
#                   legend = "bottom", 
#                    widths = c(3, 1.5))
