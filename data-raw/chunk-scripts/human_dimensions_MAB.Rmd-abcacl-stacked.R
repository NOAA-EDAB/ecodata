
ecodata::abc.acl %>% 
  dplyr::filter(EPU == "MAB") %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") %>% 
  dplyr::filter(Var == "Quota") %>% 
  dplyr::mutate(Fishery = gsub("Commercial", "C", Fishery), 
                Fishery = gsub("Recreational", "R", Fishery)) %>% 
   dplyr::group_by(Fishery, Time) %>% 
   dplyr::summarise(Value = sum(Value)) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_bar(aes( y = Value, x = Time, fill = Fishery), stat="identity", position = "stack" )+
  ggplot2::ggtitle("ABC or ACL for MAFMC Managed Species")+
  ggplot2::theme(legend.text = element_text(size = 8), 
                 legend.key.height = unit(2, "mm"))+
  ggplot2::ylab("ABC or ACL")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ggplot2::guides(fill=guide_legend(ncol=2))+
  ecodata::theme_title()
