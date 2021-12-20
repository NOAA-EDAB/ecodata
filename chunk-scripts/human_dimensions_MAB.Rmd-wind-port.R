
df.symbol <- ecodata::wind_port %>% filter(EPU == "MAB", 
                                           Value >= 3) %>% 
   pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::select(City, EJ, gentrification) %>% 
  pivot_longer(cols = c(EJ, gentrification), names_to = "Variable") %>% 
  filter(!value == "NA") %>% 
  dplyr::mutate(symbol = recode(Variable, EJ = -1, gentrification = -3))

df.all<- ecodata::wind_port %>% filter(EPU == "MAB") %>%
  pivot_wider( names_from = Var, values_from = Value) %>%
  dplyr::mutate(ordering = total_rev) %>% 
  pivot_longer(cols = c(wea_rev, total_rev), names_to="Var", values_to = "Value") %>% 
  dplyr::arrange(desc(ordering)) %>%
  dplyr::mutate(City = factor(City, levels = unique(City)),  
                Value = Value/1000000)
p1<-ggplot2::ggplot()+
  ggplot2::geom_bar(data = df.all, aes(fill = Var, y = reorder(City, ordering), x = Value), stat="identity" )+
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = element_blank())+
  geom_point(data = df.symbol, aes(x = symbol,y = City, shape = Variable)) +
  #scale_shape_manual(values = c(21, 24)) +
  ggplot2::ggtitle("Port Revenue from WEA")+
  ggplot2::xlab("Port Revenue 10^6")+
  ggplot2::ylab(element_blank())+
  scale_fill_discrete(name = "", labels = c("total revenue", "revenue from WEA"))+
  ecodata::theme_ts()

p1
