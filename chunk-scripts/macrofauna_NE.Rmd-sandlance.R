
ecodata::sandlance %>%
  dplyr::filter(Var %in% c("mean_sand_lance","n_samples",
                           "n_samples_w_fish","propn_nonzero_samples")) %>% 
  dplyr::mutate(Var = recode(Var, "propn_nonzero_samples" = "Proportion of Non-Zero Samples", 
                             "mean_sand_lance" = "Mean Sandlance", 
                             "n_samples" = "Number of Samples", 
                             "n_samples_w_fish" = "Number of Samples with Fish")) %>% 
  ggplot()+
  ggplot2::geom_point(aes(x=Time, y = Value))+
  ggplot2::geom_line(aes(x=Time, y = Value))+
  #ggplot2::ggtitle("Sandlance")+
  ggplot2::facet_wrap(~Var, scales = "free")+
  #ggplot2::ylab("Number of Individuals")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()+
  ggplot2::theme(legend.title = element_blank(), 
                 legend.position = "bottom")+
  ecodata::theme_facet()
