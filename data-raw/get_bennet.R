# Process Bennet indicator; price and volume indicators

library(dplyr)
library(tidyr)
library(magrittr)

raw.dir <- here::here('data-raw')
bennet_Rdata<- "Bennet_Index_23 (1).Rdata"
get_bennet <- function(save_clean = F){

  load(file.path(raw.dir, bennet_Rdata))
  bennet <- bennet %>%
    dplyr::rename(EPU = Region) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  # metadata ----
  attr(bennet, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/bennet-indicator.html"
  attr(bennet, "data_files")   <- list(
    bennet_Rdata = bennet_Rdata)
  attr(bennet, "data_steward") <- c(
    "John Walden <john.walden@noaa.gov>",
    "Geret DePiper <geret.depiper@noaa.gov>")
  attr(bennet, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-bennet.R",
    `hd_MAB_2` = "human_dimensions_MAB.Rmd-bennet2.R",
    `hd_MAB_all` = "human_dimensions_MAB.Rmd-bennet-all.R",
    `hd_MAB_table` = "human_dimensions_MAB.Rmd-bennet-table.R",
    `hd_NE` = "human_dimensions_NE.Rmd-bennet.R",
    `hd_NE_2` = "human_dimensions_NE.Rmd-bennet2.R",
    `hd_NE_all` = "human_dimensions_NE.Rmd-bennet-all.R")

  if (save_clean){
    usethis::use_data(bennet, overwrite = T)
  } else {
    return(bennet)
  }
}
get_bennet(save_clean = T)



#
# indicators <- bennet %>%
#   dplyr::filter(!EPU == "MAB")
#
# indicators$Var<- gsub( "Predator", "", indicators$Var)
# indicators$Var<- gsub( "Value", "Volume", indicators$Var)
# indicators<- indicators %>%
#   separate(Var, c("Guild", "Var") ) %>%
#   dplyr::filter(!Var == "Revenue",
#                 !Guild == "Total",
#                 !Time < 1985) %>%
#   dplyr::group_by(Time,  Var, EPU) %>%
#   dplyr::mutate(component = sum(Value)) %>%
#   ungroup()
#
# # dplyr::filter(#stringr::str_detect(Var, pattern="Total"),
# #        !Var == "Total Revenue Change - Bennet",
# #        !Time < 1985) %>%
# # dplyr::mutate(Var, Var = plyr::mapvalues(Var, from = c("Total Volume Index - Bennet", "Total Price Index - Bennet"),
# #                                          to = c("Volume","Price"))) %>%
# # dplyr::group_by(Time) %>%
# # dplyr::mutate(New = sum(Value)) %>%
# # dplyr::group_by(Time, Var) %>%
# # dplyr::mutate(component = sum(Value))
#
# revchange <- bennet %>%
#   dplyr::filter(!EPU == "MAB",
#                 #Var %in% c("Total Revenue Change - Bennet"),
#                 !Time<1985)
# #custom bar fill color (color-blind friendly)
# #ind_fill <- c("#a6cee3", "#b2df8a", "#000001")
#
# #limits
# y.lim <- c(-400,300)
#
# indicators$Guild<-factor(indicators$Guild, levels = c("Apex", "Piscivore", "Planktivore",
#                                                       "Benthivore", "Benthos", "Other"))
#
# #plot
# ggplot2::ggplot()+
#   #Highlight last ten years
#   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
#                     xmin = x.shade.min , xmax = x.shade.max,
#                     ymin = -Inf, ymax = Inf)+
#   ggplot2::geom_bar(data = indicators, aes(x = Time, y = Value, fill = Guild), stat="identity")+
#   #ggplot2::scale_fill_manual(name = "Indicators", values = Guild) +
#   ggplot2::geom_line(data = indicators, aes(x = Time, y = component, color = "$"))+
#   ggplot2::scale_x_continuous(breaks = seq(1965, 2020, by = 10), expand = c(0.01, 0.01)) +
#   ggplot2::facet_grid(EPU~Var, scales = "free")+
#   ggplot2::scale_colour_grey(name ="Component") +
#   ggplot2::ggtitle("Bennet Indicator")+
#   ggplot2::labs(y="Value $1,000,000 ($2015)") +
#
#   ggplot2::xlab(element_blank())+
#   #ggplot2::scale_x_continuous(breaks = seq(1965, 2020, by = 10), expand = c(0.01, 0.01)) +
#   #ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100),
#   #                            limits = y.lim, expand = c(0.01, 0.01)) +
#   ecodata::theme_ts() +
#   ggplot2::scale_fill_brewer(palette = "Set1")+
#   ggplot2::theme(title = element_text(size = 10))+
#   ecodata::theme_title()+
#   ecodata::theme_facet()
