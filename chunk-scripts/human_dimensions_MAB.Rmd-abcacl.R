
ecodata::abc.acl %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  dplyr::filter(Var == " ABC/ACL", 
                !Fishery == "Atlantic mackerel Comm ") %>% 
  dplyr::mutate(Fishery = recode(Fishery, "Atlantic mackerel Rec " = "Atlantic Mackerel")) %>% 
  dplyr::group_by(Fishery) %>% 
  #dplyr::mutate(Value = Value/10000) %>% 
  dplyr::summarise(Value_mean = mean(Value), 
                   Value_sd = sd(Value)) %>%
  dplyr::mutate(upper = Value_mean + Value_sd, 
                lower = Value_mean - Value_sd) %>% 
  ungroup() %>% 
  ggplot2::ggplot()+
  ggplot2::geom_bar(aes( y = Fishery, x = Value_mean), stat="identity" )+
  ggplot2::geom_errorbar(aes( y = Fishery, xmin = lower, xmax=upper),
                         stat="identity")+
  #ggplot2::geom_point()+
  #ggplot2::geom_line()+
  ggplot2::ggtitle("ABC/ACL for MAFMC Managed Species")+
  #ggplot2::ylab(expression("ABC/ACL"))+
  ggplot2::xlab("ABC/ACL")+
  ecodata::theme_ts()+
  ecodata::theme_title()
