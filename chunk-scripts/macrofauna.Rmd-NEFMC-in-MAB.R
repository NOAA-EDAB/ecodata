
nefmc_ma <- nefsc_survey_disaggregated %>% 
  # distinct() %>% 
  dplyr::filter(EPU %in% c("MAB"),
         Management == "NEFMC",
         !stringr::str_detect(`Feeding guild`,"Other")) %>% 
  dplyr::group_by(EPU, `Feeding guild`, Season, Time) %>% 
  dplyr::summarise(Value = sum(Proportion,na.rm = T)) %>% #IMPORTANT: Turn zeros to NA before taking mean
  dplyr::mutate(Value = ifelse(Value == 0, NA, Value)) %>% #Turn zeros to NA
  tidyr::unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
  dplyr::group_by(EPU,Var, .drop = FALSE) %>% 
  dplyr::mutate(hline = mean(Value, na.rm = T)) %>% 
  dplyr::filter(stringr::str_detect(Var,"Benthivore")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Var = paste(stringr::str_to_title(stringr::str_extract(Var, "spring|fall")),
         "survey"))

nefmc_ma$Var <- factor(nefmc_ma$Var,levels = c("Spring survey","Fall survey"))

mab_nefmc_props <- nefmc_ma %>% 
 ggplot2::ggplot(aes(x = Time, y = Value, group = Var)) +
  
  #Highlight last ten years
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Add time series
  ggplot2::geom_line(size = lwd-0.5) +
  ggplot2::geom_point(size = pcex-0.5) +
  ecodata::geom_gls(aes(x = Time, y = Value)) +
  # scale_color_manual(values = series.col, aesthetics = "color")+
  ggplot2::guides(color = FALSE) +
  ggplot2::geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  ggplot2::facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  ggplot2::ggtitle("NEFMC benthivores in the Mid-Atlantic")+
  #Axis and theme
  ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ggplot2::ylab(expression("Proportion of MAB survey")) +
  ecodata::theme_facet()+
  ggplot2::theme(strip.text=element_text(hjust=0))

mab_nefmc_props
