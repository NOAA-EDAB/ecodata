
test<- ecodata::abc.acl %>% filter(EPU == "NE") %>%
  tidyr::separate(col = Var, into = c("FMP", "Species", "Var"), sep = "_") %>%
  tidyr::separate(col = Species, into = c("Species", "Location"), sep = "-")  %>%
  dplyr::filter(!Value == "NA") %>% 
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>% 
  dplyr::mutate(Species = dplyr::recode(Species, "Atlantic cod " = "Groundfish", 
                                        "Acadian redfish " = "Groundfish", 
                                        "American plaice " = "Groundfish", 
                                        "Atlantic halibut " = "Groundfish", 
                                        "Haddock "    = "Groundfish", 
                                        "Pollock " = "Groundfish", 
                                        "White hake " = "Groundfish",
                                        "Winter flounder " = "Groundfish",
                                        "Witch flounder "  = "Groundfish",
                                        "Yellowtail flounder "  = "Groundfish",
                                        "Atlantic wolffish "  = "Groundfish",
                                        "Ocean pout "    = "Groundfish",
                                        "Windowpane "  = "Groundfish", 
                                        "Red hake "  = "Small Mesh", 
                                        "Silver hake " = "Small Mesh", 
                                        "Offshore hake " = "SMall Mesh")) %>% 
  dplyr::group_by(Time, Species) %>%
  dplyr::summarise(ACL = sum(ACL), 
                   Catch = sum(Catch)) %>% 
  dplyr::mutate(Value = Catch/ACL) %>% 
  dplyr::filter(Time > 2012 & Time <2021) %>% 

  ggplot2::ggplot()+
  ggplot2::geom_bar(aes( y = ACL, x = Time, fill = Species), stat="identity", position = "stack" )+
  ggplot2::ggtitle("Quota for NEFMC Managed Species")+
  #ggplot2::ylab(expression("ABC/ACL"))+
  ggplot2::theme(legend.text = element_text(size = 10), 
                 legend.key.height = unit(2, "mm"))+
  ggplot2::ylab("Quota")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
  
test
