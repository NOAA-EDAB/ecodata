
mean<- ecodata::abc.acl %>% 
  dplyr::filter(EPU == "NE", 
                Time >= 2012) %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  # dplyr::rename("abc.acl" = " ABC/ACL", 
  #               "Catch" = " Catch") %>% 
  dplyr::mutate(Value = Catch_Landings/Quota, 
                Time = as.character(Time)) %>% 
  dplyr::group_by(Time) %>% 
  dplyr::summarise(val = mean(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Time = as.numeric(Time))

ecodata::abc.acl %>% 
  filter(EPU == "NE", 
         Time >=2012) %>%  
  tidyr::separate(col = Var, into = c("Fishery", "Var", "Quota_Category"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::mutate(Value = Catch_Landings/Quota, 
                Time = as.numeric(Time)) %>% 
  ggplot2::ggplot()+
  #geom_boxplot()+
  geom_point(aes(x = Time, y = Value, shape = Quota_Category, color =  Quota_Category))+
  geom_point(data = mean, aes(x = Time, y = val), color = "black")+
  geom_line(data = mean, aes(x = Time, y = val), color = "black")+
  geom_hline(yintercept = 1, linetype='dashed', col = 'gray')+
  geom_text_repel(aes(x = Time, y = Value,
                      label=ifelse(Value>1,as.character(Fishery),'')),
                  hjust=0,vjust=0, size= 2, force= 75,
                  direction = "both", 
                  nudge_x = 1, 
                  nudge_y = 0.5)+
  ggplot2::ggtitle("NEFMC Catch per Quota")+
  ggplot2::ylab(expression("Catch / Quota"))+
  ggplot2::theme()+
  ggplot2::xlab(element_blank())+
  scale_x_continuous(breaks=c(2012,2014,2016, 2018,2020,2022))+
  #scale_shape_manual(values=c(18, 20, 16))+
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  ecodata::theme_ts()+
  ecodata::theme_title()
