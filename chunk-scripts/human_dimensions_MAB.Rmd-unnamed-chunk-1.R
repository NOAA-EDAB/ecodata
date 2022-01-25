
ecodata::abc.acl %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  dplyr::filter(Var == " ABC/ACL") %>% 
  group_by(Time) %>% 
  dplyr::mutate(Value = Value/10000) %>% 
  summarise(Value_mean = mean(Value), 
            Value_sd = sd(Value)) %>%
  dplyr::mutate(upper = Value_mean + Value_sd, 
                lower = Value_mean - Value_sd) %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = Time, y=Value_mean))+
  ggplot2::geom_ribbon(aes(x = Time, ymin = lower, ymax= upper), fill = "grey70")+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::ggtitle("ABC/ACL for MAFMC Managed Species")+
  ggplot2::ylab(expression("ABC/ACL (10"^4*")"))+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
