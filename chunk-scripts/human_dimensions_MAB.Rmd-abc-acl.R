
ecodata::abc.acl %>% 
  #group_by(Time) %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% 
  dplyr::rename("abc.acl" = " ABC/ACL", 
                "Catch" = " Catch") %>% 
  dplyr::mutate(Value = Catch/abc.acl, 
                Time = as.character(Time)) %>% 
  filter(!Value == "NA", 
         !Fishery == "Chub mackerel ") %>% 
  ggplot2::ggplot(aes(x = Time, y = Value))+
  geom_boxplot()+
  #geom_point(aes(x = Time, y = Value,color = Fishery))+
  ggplot2::ggtitle("MAFMC Catch per ABC/ACL")+
  ggplot2::ylab(expression("Ratio of Catch to ABC/ACL"))+
  ggplot2::theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
