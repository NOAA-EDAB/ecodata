
micro_chlor <- ecodata::phyto_size %>% 
  dplyr::filter(EPU %in% c("GB", "GOM"),
                Var %in% c("CLIMATOLOGICAL_WEEK_MICRO_PERCENTAGE_MEDIAN", 
                           "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") %>% 
  dplyr::filter(!Value == "NA") %>% 
  dplyr::select(-Year1, -Year2, -Cat,  -Units) %>% 
  tidyr::pivot_wider(id_cols= c(EPU, WEEK), names_from = Var, values_from = Value) %>% 
  dplyr::mutate(Month = rep(1:4, each = 26)) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_point( aes(x = CLIMATOLOGICAL_WEEK_MICRO_PERCENTAGE_MEDIAN, 
                           y = CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN, color = factor(Month))) +
  
  
  ggplot2::scale_color_discrete(name = "Season", labels=c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))+
  ggplot2::ggtitle("") +
  ggplot2::facet_wrap(~EPU)+
  ggplot2::ylab("Chlorophyll a (mg m^-3)") +
  ggplot2::xlab("Microplankton % of Total Phytoplankton Composition ") +
  ecodata::theme_facet()+
  ecodata::theme_title()
#micro_chlor
