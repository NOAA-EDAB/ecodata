
mean<- ecodata::abc.acl %>% 
  dplyr::filter(EPU == "NE", 
                Time >= 2012) %>% 
  tidyr::separate(col = Var, into = c("FMP", "Species", "Var"), sep = "_") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::filter(!Catch == "NA", 
                !ACL == "NA") %>% 
  dplyr::mutate(Value = (Catch/ACL), 
                Time = as.character(Time)) %>% 
  dplyr::group_by(Time) %>% 
  dplyr::summarise(val = mean(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Time = as.numeric(Time)) %>% 
  dplyr::filter(Time > 2012 & Time < 2021 )

above<- ecodata::abc.acl %>% 
  filter(EPU == "NE", 
         Time >=2012) %>%  
  tidyr::separate(col = Var, into = c("FMP", "Species","Var"), sep = "_") %>%   tidyr::separate(col = Species, into = c("Species", "Location"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(Value = Catch/ACL, 
                Time = as.numeric(Time)) %>% 
  dplyr::filter(Time > 2012 & Time < 2021 , 
                Value > 1)

ecodata::abc.acl %>% 
  filter(EPU == "NE", 
         Time >=2012) %>%  
  tidyr::separate(col = Var, into = c("FMP", "Species","Var"), sep = "_") %>%   tidyr::separate(col = Species, into = c("Species", "Location"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(Value = Catch/ACL, 
                Time = as.numeric(Time)) %>% 
  dplyr::filter(Time > 2012 & Time < 2021, 
                Value < 1) %>% 
  ggplot2::ggplot()+
  #geom_boxplot()+
  geom_point(aes(x = Time, y = Value))+
  geom_point(data = above, aes(x = Time, y = Value, color = Species))+
  geom_point(data = mean, aes(x = Time, y = val), color = "red")+
  geom_line(data = mean, aes(x = Time, y = val), color = "red")+
  geom_hline(yintercept = 1, linetype='dashed', col = 'gray')+
  # geom_text_repel(aes(x = Time, y = Value,
  #                     label=ifelse(Value>1,as.character(Species),'')),
  #                 hjust=0,vjust=0, size= 2, force= 75,
  #                 direction = "both", 
  #                 nudge_x = 1, 
  #                 nudge_y = 0.5)+
  ggplot2::ggtitle("NEFMC Catch per Quota")+
  ggplot2::ylab(expression("Catch / Quota"))+
  ggplot2::theme()+
  ggplot2::xlab(element_blank())+
  scale_x_continuous(breaks=c(2014,2016, 2018,2020))+
  #scale_shape_manual(values=c(18, 20, 16))+
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  ecodata::theme_ts()+
  ecodata::theme_title()
