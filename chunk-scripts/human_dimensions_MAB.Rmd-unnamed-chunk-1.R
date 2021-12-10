
ecodata::abc.acl %>% 
  group_by(Time) %>% 
  filter(str_detect(Var, "ABC/ACL")) %>% 
  mutate(Value = Value/100000) %>% 
  summarise(Total = sum(Value)) %>% 
  ungroup() %>% 
  mutate(hline = mean(Total)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Total))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_hline(aes(yintercept = hline), linetype = "dashed")+
  ggplot2::ggtitle("MAFMC Total ABC/ABL")+
  ggplot2::ylab(expression("Total ABC/ACL 10"^5*" tons"))+
  ggplot2::theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()

ecodata::abc.acl %>% 
  group_by(Time) %>% 
  filter(str_detect(Var, "Catch")) %>% 
  mutate(Value = Value/100000) %>% 
  summarise(Total = sum(Value)) %>% 
  ungroup() %>% 
  mutate(hline = mean(Total)) %>% 
  ggplot2::ggplot(aes(x = Time, y = Total))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_hline(aes(yintercept = hline), linetype = "dashed")+
  ggplot2::ggtitle("MAFMC Total Catch")+
  ggplot2::ylab(expression("Total Catch 10"^5*" tons"))+
  ggplot2::theme(legend.title = element_blank())+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
