
mean<- ecodata::abc.acl %>% 
  dplyr::filter(EPU == "MAB") %>% 
  tidyr::separate(col = Var, into = c("FMP", "Var"), sep = "_") %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  #tidyr::separate(Catch, into = c("Catch", "X"), sep = ",") %>% 
  dplyr::mutate(Catch = as.numeric(stringr::str_extract(Catch, pattern = "\\d+")), 
                Quota = as.numeric(stringr::str_extract(Quota, pattern = "\\d+")),
                Value = Catch/Quota, 
                Time = as.character(Time)) %>% 
  filter(!Value == "NA") %>%
  dplyr::group_by(Time) %>% 
  dplyr::summarise(val = mean(Value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Time = as.numeric(Time))

ecodata::abc.acl %>% 
  dplyr::filter(EPU == "MAB") %>% 
  tidyr::separate(col = Var, into = c("FMP", "Var"), sep = "_") %>% 
  tidyr::pivot_wider(names_from = Var, values_from = Value)  %>% 
  #tidyr::separate(Catch, into = c("Catch", "X"), sep = ",") %>% 
  dplyr::mutate(Catch = as.numeric(stringr::str_extract(Catch, pattern = "\\d+")), 
                Quota = as.numeric(stringr::str_extract(Quota, pattern = "\\d+")),
                Value = Catch/Quota, 
                Time = as.numeric(Time))%>% 
  filter(!Value == "NA") %>% 
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
