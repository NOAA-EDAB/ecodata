library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

nano_csv<- "19980101_20221231-OCCCI_GLOBCOLOUR-PSC_FNANO-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv"
pico_csv<- "19980101_20221231-OCCCI_GLOBCOLOUR-PSC_FPICO-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv"
micro_csv<- "19980101_20221231-OCCCI_GLOBCOLOUR-PSC_FMICRO-STATS_ANOMS-NES_EPU_NOESTUARIES-SOE_V2023-SOE_FORMAT.csv"

nano<- read.csv(file.path(raw.dir, nano_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)
pico<- read.csv(file.path(raw.dir, pico_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)
micro<- read.csv(file.path(raw.dir, micro_csv)) %>%
  dplyr::select(PERIOD, UNITS, VARIABLE, VALUE, SUBAREA) %>%
  dplyr::rename(Time = PERIOD, Units = UNITS, Var = VARIABLE,
                EPU =SUBAREA, Value = VALUE)


phyto_size <- nano %>% rbind( pico, micro)%>%
  dplyr::select(Time, Var, Value, EPU, Units) %>%
  tibble::as_tibble()

# metadata ----
attr(phyto_size, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/phytoplankton-size-class.html"
attr(phyto_size, "data_files")   <- list(
)
attr(phyto_size, "data_steward") <- c(
  "Kimberly Hyde <kimberly.hyde@noaa.gov>")
attr(phyto_size, "plot_script") <- list(
  `ltl_MAB_weekly` = "LTL_MAB.Rmd-weekly-phyto-size.R",
  `ltl_NE_weekly` = "LTL_NE.Rmd-weekly-phyto-size.R",
  `ltl_NE_weekly-gb` = "LTL_NE.Rmd-weekly-phyto-size-gb.R",
  `ltl_NE_weekly-gom` = "LTL_NE.Rmd-weekly-phyto-size-gom.R")

usethis::use_data(phyto_size, overwrite = T)



# phyto_year_nano<- phyto_size %>%
#   dplyr::filter(EPU == c("GB"),
#                 Var %in% c("WEEKLY_NANO_PERCENTAGE_MEDIAN",
#                            "WEEKLY_MICRO_PERCENTAGE_MEDIAN")) %>%
#   mutate(Value = as.numeric(Value)) %>%
#   dplyr::filter(!Value == "NA") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   group_by(Time) %>%
#   dplyr::mutate(nano = as.numeric(WEEKLY_NANO_PERCENTAGE_MEDIAN[1]) +
#                   as.numeric(WEEKLY_MICRO_PERCENTAGE_MEDIAN[1])) %>%
#   tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
#   dplyr::mutate(year = stringr::str_sub(WEEK, 1,4),
#                 wk = stringr::str_sub(WEEK, 5,6)) %>%
#   tidyr::pivot_longer(cols = c("nano"),
#                       names_to = "Var", values_to = "Value") %>%
#   dplyr::filter(year == 2021,
#                 !Value == "NA") %>%
#   dplyr::mutate(Value = Value*100)
#
#
# phyto_year_micro<- phyto_size %>%
#   dplyr::filter(EPU == c("GB"),
#                 Var == c("WEEKLY_MICRO_PERCENTAGE_MEDIAN")) %>%
#   mutate(Value = as.numeric(Value)) %>%
#   tidyr::separate(.,Time, into = c("Cat", "WEEK"), sep = "_") %>%
#   dplyr::mutate(year = stringr::str_sub(WEEK, 1,4),
#                 wk = stringr::str_sub(WEEK, 5,6)) %>%
#   dplyr::filter(year == 2021,
#                 !Value == "NA") %>%
#   dplyr::mutate(Value = Value*100)
#
# out_phyto<-  phyto_size %>%
#   dplyr::filter(EPU == c("GB"),
#                 stringr::str_detect(Var, ("CLIMATOLOGICAL_WEEK")),
#                 !Var == "CLIMATOLOGICAL_WEEK_PICO_MEDIAN",
#                 !Var == "CLIMATOLOGICAL_WEEK_NANO_MEDIAN",
#                 !Var == "CLIMATOLOGICAL_WEEK_MICRO_MEDIAN") %>%
#   #tidyr::pivot_longer(cols = Var, names_to = "Var2", values_to = "Vaue2" )
#   tidyr::separate(Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") %>%
#   dplyr::filter(!Value == "NA",
#                 !Var == "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")  %>%
#   dplyr::mutate(Value = Value*100)
#
#
# p<-  ggplot2::ggplot() +
#   geom_area(aes(x=as.numeric(out_phyto$WEEK), y=out_phyto$Value,
#                 fill = factor(out_phyto$Var, c("CLIMATOLOGICAL_WEEK_PICO_PERCENTAGE_MEDIAN",
#                                      "CLIMATOLOGICAL_WEEK_NANO_PERCENTAGE_MEDIAN",
#                                      "CLIMATOLOGICAL_WEEK_MICRO_PERCENTAGE_MEDIAN"))), alpha=0.6)+
#   #ggplot2::geom_point(data = chlor, aes(x = as.numeric(WEEK), y = Value)) +
#   ggplot2::geom_line( aes(x = as.numeric(phyto_year_nano$wk),
#                           y = phyto_year_nano$Value), color = "#FC8D62")+
#   ggplot2::geom_line( aes(x = as.numeric(phyto_year_micro$wk),
#                           y = phyto_year_micro$Value), color = "#66C2A5")+
#   #ggplot2::facet_wrap(EPU~., ncol = 2)+
#   ggplot2::ggtitle("Georges Bank Phytoplankton Size Class") +
#   ggplot2::ylab("Percent") +
#   ggplot2::xlab(element_blank())+
#   ecodata::theme_facet() +
#   ggplot2::theme(axis.text.x = element_text(angle=45, hjust = 1),
#                  panel.spacing = unit(.5, "lines"),
#                  plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
#                  legend.position= "top")+
#   scale_fill_manual(values=c("#8DA0CB", "#FC8D62","#66C2A5"), name = "",
#                     labels = c("Picoplankton",
#                                "Nanoplankton", "Microplankton"))+
#   #scale_y_continuous( name = "Phytoplankton Size Fraction", sec.axis = sec_axis(~.*2, name="Chlorophyll a (mg m^-3)"))+
#   scale_x_continuous(breaks = month_numeric,
#                      labels = month_label)+
#   ecodata::theme_title()










