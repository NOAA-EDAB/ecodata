
month <- seq(as.Date("2021-01-01"), 
             as.Date("2021-12-01"), 
             by = "1 month")
month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

### GOM
phyto_year_nano<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                Var %in% c("WEEKLY_NANO_PERCENTAGE_MEDIAN", 
                           "WEEKLY_MICRO_PERCENTAGE_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  dplyr::filter(!Value == "NA") %>% 
  tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
  group_by(Time) %>% 
  dplyr::mutate(nano = as.numeric(WEEKLY_NANO_PERCENTAGE_MEDIAN[1]) +
                  as.numeric(WEEKLY_MICRO_PERCENTAGE_MEDIAN[1])) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  tidyr::pivot_longer(cols = c("nano"), 
                      names_to = "Var", values_to = "Value") %>% 
  dplyr::filter(year == 2021, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)


phyto_year_micro<- ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                Var == c("WEEKLY_MICRO_PERCENTAGE_MEDIAN")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
  dplyr::mutate(year = stringr::str_sub(WEEK, 1,4), 
                wk = stringr::str_sub(WEEK, 5,6)) %>% 
  dplyr::filter(year == 2021, 
                !Value == "NA") %>% 
  dplyr::mutate(Value = Value*100)

out_phyto<-  ecodata::phyto_size %>% 
  dplyr::filter(EPU == c("GOM"),
                stringr::str_detect(Var, ("CLIMATOLOGICAL_WEEK")), 
                !Var == "CLIMATOLOGICAL_WEEK_PICO_MEDIAN",
                !Var == "CLIMATOLOGICAL_WEEK_NANO_MEDIAN",
                !Var == "CLIMATOLOGICAL_WEEK_MICRO_MEDIAN") %>% 
  #tidyr::pivot_longer(cols = Var, names_to = "Var2", values_to = "Vaue2" )
  tidyr::separate(Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") %>% 
  dplyr::filter(!Value == "NA", 
                !Var == "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")  %>%
  dplyr::mutate(Value = Value*100) 
  
  
p2<-  ggplot2::ggplot() +
  geom_area(aes(x=as.numeric(out_phyto$WEEK), y=out_phyto$Value, 
                fill = factor(out_phyto$Var, c("CLIMATOLOGICAL_WEEK_PICO_PERCENTAGE_MEDIAN",
                                     "CLIMATOLOGICAL_WEEK_NANO_PERCENTAGE_MEDIAN", 
                                     "CLIMATOLOGICAL_WEEK_MICRO_PERCENTAGE_MEDIAN"))), alpha=0.6)+
  #ggplot2::geom_point(data = chlor, aes(x = as.numeric(WEEK), y = Value)) +
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_nano$wk), 
                          y = phyto_year_nano$Value), color = "#FC8D62", size = 1.5)+
  ggplot2::geom_line( aes(x = as.numeric(phyto_year_micro$wk), 
                          y = phyto_year_micro$Value), color =  "#66C2A5", size = 1.5)+
  #ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::ggtitle("Gulf of Maine Phytoplankton Size Class") +
  ggplot2::ylab("Percent") +
  ggplot2::xlab(element_blank())+
  ecodata::theme_facet() +
  ggplot2::theme(axis.text.x = element_text(angle=45, hjust = 1),
                 panel.spacing = unit(.5, "lines"),
                 plot.margin = unit(c(0.1, 0, 0, 0), "cm"), 
                 legend.position= "top")+
  scale_fill_manual(values=c("#8DA0CB", "#FC8D62","#66C2A5"), name = "", 
                    labels = c("Picoplankton", 
                               "Nanoplankton", "Microplankton"))+
  #scale_y_continuous( name = "Phytoplankton Size Fraction", sec.axis = sec_axis(~.*2, name="Chlorophyll a (mg m^-3)"))+
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label)+
  ecodata::theme_title()
p2
