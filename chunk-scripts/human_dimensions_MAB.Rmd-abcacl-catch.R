
mean<- ecodata::abc.acl %>% 
  #group_by(Time) %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::rename("abc.acl" = " ABC/ACL", 
                "Catch" = " Catch") %>% 
  dplyr::mutate(Value = Catch/abc.acl, 
                Time = as.character(Time)) %>% 
  filter(!Value == "NA", 
         !Fishery == "Chub mackerel ") %>%
  dplyr::group_by(Time) %>% 
  dplyr::summarise(val = mean(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Time = as.numeric(Time))

ecodata::abc.acl %>% 
  #group_by(Time) %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::rename("abc.acl" = " ABC/ACL", 
                "Catch" = " Catch") %>% 
  dplyr::mutate(Value = Catch/abc.acl, 
                Time = as.numeric(Time)) %>% 
  filter(!Value == "NA", 
         !Fishery == "Chub mackerel ") %>% 
  ggplot2::ggplot()+
  #geom_boxplot()+
  geom_point(aes(x = Time, y = Value))+
  geom_point(data = mean, aes(x = Time, y = val), color = "red")+
  geom_line(data = mean, aes(x = Time, y = val), color = "red")+
  geom_hline(yintercept = 1, linetype='dashed', col = 'gray')+
  ggplot2::ggtitle("MAFMC Catch per ABC or ACL")+
  ggplot2::ylab(expression("Catch / ABC or ACL"))+
  ggplot2::theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
