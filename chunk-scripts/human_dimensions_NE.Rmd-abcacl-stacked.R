
groundfish<- ecodata::abc.acl %>% filter(EPU == "NE") %>%
  tidyr::separate(col = Var, into = c("FMP", "Species", "Var"), sep = "_") %>%
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
  filter(stringr::str_detect(FMP, "Multispecies"), 
         !ACL == "NA") %>%
  dplyr::group_by(Time) %>%
  dplyr::summarise(ACL, Catch) %>%
  dplyr::mutate(Fishery = "Groundfish",
                EPU = "NE",
                Units = "mt")

# sm_mesh<- abc.acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   filter(stringr::str_detect(Fishery, "Small Mesh")) %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Quota, Catch_Landings) %>%
#   dplyr::mutate(Fishery = "Small Mesh",
#                 EPU = "NE",
#                 Units = "mt")

ecodata::abc.acl %>% filter(EPU == "NE") %>%
  tidyr::separate(col = Var, into = c("FMP", "Species", "Var"), sep = "_") %>%
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
  dplyr::filter(!stringr::str_detect(FMP, "Groundfish")) %>%
  #rbind(groundfish) %>%
  dplyr::mutate(Value = Catch/ACL) %>% 

  ggplot2::ggplot()+
  ggplot2::geom_bar(aes( y = ACL, x = Time, fill = FMP), stat="identity", position = "stack" )+
  ggplot2::ggtitle("Quota for NEFMC Managed Species")+
  #ggplot2::ylab(expression("ABC/ACL"))+
  ggplot2::theme(legend.text = element_text(size = 10), 
                 legend.key.height = unit(2, "mm"))+
  ggplot2::ylab("Quota")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
