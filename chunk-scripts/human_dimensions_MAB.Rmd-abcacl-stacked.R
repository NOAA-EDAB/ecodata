
ecodata::abc.acl %>% 
  tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>% 
  dplyr::filter(Var == " ABC/ACL", 
                !Fishery == "Atlantic mackerel Comm ") %>% 
  dplyr::mutate(Fishery = recode(Fishery, "Atlantic mackerel Rec " = "Atlantic Mackerel"),
                Fishery = recode(Fishery, "Bluefish " = "All Others"), 
                Fishery = recode(Fishery, "Chub mackerel " = "All Others"),
                Fishery = recode(Fishery, "Golden Tilefish " = "All Others"),
                Fishery = recode(Fishery, "Black Sea Bass Recreational " = "All Others"),
                Fishery = recode(Fishery, "Black Sea Bass Commercial " = "All Others"),
                Fishery = recode(Fishery, "Summer Flounder Recreational " = "All Others"),
                Fishery = recode(Fishery, "Summer Flounder Commercial " = "All Others"),
                Fishery = recode(Fishery, "Scup Recreational " = "All Others"), 
                Fishery = recode(Fishery, "Scup Commercial " = "All Others")) %>% 
   dplyr::group_by(Fishery, Time) %>% 
   dplyr::summarise(Value = sum(Value)) %>% 
  # dplyr::summarise(Value_mean = mean(Value), 
  #                  Value_sd = sd(Value)) %>%
  # dplyr::mutate(upper = Value_mean + Value_sd, 
  #               lower = Value_mean - Value_sd) %>% 
  # ungroup() %>% 
  ggplot2::ggplot()+
  ggplot2::geom_bar(aes( y = Value, x = Time, fill = Fishery), stat="identity", position = "stack" )+
  # ggplot2::geom_errorbar(aes( y = Fishery, xmin = lower, xmax=upper),
  #                        stat="identity")+
  #ggplot2::geom_point()+
  #ggplot2::geom_line()+
  ggplot2::ggtitle("ABC or ACL for MAFMC Managed Species")+
  #ggplot2::ylab(expression("ABC/ACL"))+
  #ggplot2::theme(legend.position = "bottom")+
  ggplot2::ylab("ABC or ACL")+
  ggplot2::xlab(element_blank())+
  ecodata::theme_ts()+
  ecodata::theme_title()
