
mafmc_ne <- ecodata::nefsc_survey_disaggregated %>% 
  dplyr::distinct() %>% 
  dplyr::filter(EPU %in% c("GOM","GB"),
         Management == "MAFMC",
         !stringr::str_detect(`Feeding guild`,"Other")) %>% 
  dplyr::group_by(EPU, `Feeding guild`, Season, Time) %>% 
  dplyr::summarise(Value = sum(Proportion,na.rm = T)) %>% #IMPORTANT: Turn zeros to NA before taking mean
  dplyr::mutate(Value = ifelse(Value == 0, NA, Value)) %>% #Turn zeros to NA
  tidyr::unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
  dplyr::group_by(EPU,Var) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  dplyr::filter(stringr::str_detect(Var,"Planktivore")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = paste(stringr::str_to_title(stringr::str_extract(Var, "fall|spring")),
         "survey"))
mafmc_ne$Var <- factor(mafmc_ne$Var,levels = c("Spring survey","Fall survey"))
gom_mafmc_props <- mafmc_ne %>% 
  dplyr::filter(EPU == "GOM") %>% 
 ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  # scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("MAFMC planktivores in Gulf of Maine")+
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Proportion of GOM survey")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))

gb_mafmc_props <- mafmc_ne %>% 
  dplyr::filter(EPU == "GB") %>% 
ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  # geom_gls(aes(x = Time, y = Value,
  #              color = Var),
  #            alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  # scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("MAFMC planktivores on Georges Bank")+
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab("Proportion of GB survey") +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))


gom_mafmc_props + gb_mafmc_props + patchwork::plot_layout(ncol = 1)
