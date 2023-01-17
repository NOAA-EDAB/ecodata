
month <- seq(as.Date("2022-01-01"), 
             as.Date("2022-12-31"), 
             by = "1 month")
month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

phyto_year_nano<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GB"),
                Var %in% c("WEEKLY_PSC_FNANO_MEDIAN", 
                           "WEEKLY_PSC_FMICRO_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  # dplyr::mutate(filter_me = 1:2600) %>% 
  # dplyr::filter(!filter_me == 2463,
  #               !filter_me == 1214) %>%
  # dplyr::select(!filter_me) %>% 
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
  dplyr::mutate(nano = as.numeric(WEEKLY_PSC_FNANO_MEDIAN) +
                  as.numeric(WEEKLY_PSC_FMICRO_MEDIAN)) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  tidyr::pivot_longer(cols = c("nano"), 
                      names_to = "Var", values_to = "Value") %>% 
  dplyr::filter(year == 2022, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)


phyto_year_micro<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GB"),
                Var == c("WEEKLY_PSC_FMICRO_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  dplyr::filter(year == 2022, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)

out_phyto<-  ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GB"),
                stringr::str_detect(Var, ("CLIMATOLOGICAL_WEEK"))) %>% #, 
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_PICO_MEDIAN",
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_NANO_MEDIAN",
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_MICRO_MEDIAN") %>% 
  #tidyr::pivot_longer(cols = Var, names_to = "Var2", values_to = "Vaue2" )
  tidyr::separate(Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") %>% 
  dplyr::filter(!Value == "NA", 
                !Var == "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")  %>%
  dplyr::mutate(Value = Value*100) 
  
  
p<-  ggplot2::ggplot() +
  geom_area(aes(x=as.numeric(out_phyto$WEEK), y=out_phyto$Value, 
                fill = factor(out_phyto$Var, c("CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN",
                                               "CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN",
                                               "CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN"))), alpha=0.6)+
  #ggplot2::geom_point(data = chlor, aes(x = as.numeric(WEEK), y = Value)) +
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_nano$wk), 
                          y = phyto_year_nano$Value), color = "#FC8D62", size = 1.5)+
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_micro$wk), 
                          y = phyto_year_micro$Value), color = "#66C2A5", size = 1.5)+
  #ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::ggtitle("Georges Bank") +
  ggplot2::ylab("Percent") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet() +
  ggplot2::theme(axis.text.x = element_text(angle=45, hjust = 1),
                 panel.spacing = unit(.5, "lines"),
                 plot.margin = unit(c(0.1, 0, 0, 0), "cm"), 
                 legend.position= "none")+
  scale_fill_manual(values=c("#8DA0CB", "#FC8D62","#66C2A5"), name = "",
                    labels = c("Picoplankton",
                               "Nanoplankton", "Microplankton"))+
  #scale_y_continuous( name = "Phytoplankton Size Fraction", sec.axis = sec_axis(~.*2, name="Chlorophyll a (mg m^-3)"))+
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label)+
  ecodata::theme_title()

month <- seq(as.Date("2021-01-01"),
             as.Date("2021-12-01"),
             by = "1 month")
month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

### GOM
phyto_year_nano2<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                Var %in% c("WEEKLY_PSC_FNANO_MEDIAN", 
                           "WEEKLY_PSC_FMICRO_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  # dplyr::mutate(filter_me = 1:2600) %>% 
  # dplyr::filter(!filter_me == 2463,
  #               !filter_me == 1214) %>%
  # dplyr::select(!filter_me) %>% 
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
  dplyr::mutate(nano = as.numeric(WEEKLY_PSC_FNANO_MEDIAN) +
                  as.numeric(WEEKLY_PSC_FMICRO_MEDIAN)) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  tidyr::pivot_longer(cols = c("nano"), 
                      names_to = "Var", values_to = "Value") %>% 
  dplyr::filter(year == 2022, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)


phyto_year_micro2<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                Var == c("WEEKLY_PSC_FMICRO_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  dplyr::filter(year == 2022, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)

out_phyto2<-  ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                stringr::str_detect(Var, ("CLIMATOLOGICAL_WEEK"))) %>% #, 
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_PICO_MEDIAN",
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_NANO_MEDIAN",
                #!Var == "CLIMATOLOGICAL_WEEK_PSC_MICRO_MEDIAN") %>% 
  #tidyr::pivot_longer(cols = Var, names_to = "Var2", values_to = "Vaue2" )
  tidyr::separate(Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") %>% 
  dplyr::filter(!Value == "NA", 
                !Var == "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")  %>%
  dplyr::mutate(Value = Value*100) 
  
  
p2<-  ggplot2::ggplot() +
  geom_area(aes(x=as.numeric(out_phyto2$WEEK), y=out_phyto2$Value, 
                fill = factor(out_phyto2$Var, c("CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN",
                                               "CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN",
                                               "CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN"))), alpha=0.6)+
  #ggplot2::geom_point(data = chlor, aes(x = as.numeric(WEEK), y = Value)) +
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_nano2$wk), 
                          y = phyto_year_nano2$Value), color = "#FC8D62", size = 1.5)+
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_micro2$wk), 
                          y = phyto_year_micro2$Value), color = "#66C2A5", size = 1.5)+
  #ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::ggtitle("Gulf of Maine") +
  ggplot2::ylab(element_blank()) +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet() +
  ggplot2::theme(axis.text.x = element_text(angle=45, hjust = 1),
                 panel.spacing = unit(.5, "lines"),
                 plot.margin = unit(c(0.1, 0, 0, 0), "cm"), 
                 legend.position= "none", 
  axis.text.y = element_blank(), 
  axis.ticks.y = element_blank())+
  scale_fill_manual(values=c("#8DA0CB", "#FC8D62","#66C2A5"), name = "",
                    labels = c("Picoplankton",
                               "Nanoplankton", "Microplankton"))+
  #scale_y_continuous( name = "Phytoplankton Size Fraction", sec.axis = sec_axis(~.*2, name="Chlorophyll a (mg m^-3)"))+
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label)+
  ecodata::theme_title()


plot_row<- cowplot::plot_grid(p, p2, ncol = 2)
title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Phytoplankton Size Class",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0,1)
  )


plot_w_title <- cowplot::plot_grid(title, plot_row, ncol = 1, 
                   rel_heights = c(0.1, 1))
legend_b <- cowplot::get_legend(
  p + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

cowplot::plot_grid(plot_w_title, legend_b, ncol = 1,rel_heights = c(1, 0.1) )
