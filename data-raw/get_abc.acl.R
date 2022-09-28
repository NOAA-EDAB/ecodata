#### get ABC/ACL Catch from MAFMC

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

abc.acl_xlsx <- "ABC_ACL_catch - Brandon Muffley - NOAA Affiliate.xlsx"
NEFMC_abc_acl_xlsx <- "NEFMC_abc_acl.xlsx"

get_abc.acl <- function(save_clean = F){
  # import data
  ma<-readxl::read_excel(file.path(raw.dir,abc.acl_xlsx)) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::rename(spec = "Species/Sector") %>%
    tidyr::pivot_longer(cols = c("ABC/ACL 2012",   "Catch 2012",     "ABC/ACL 2013" ,
                                 "Catch 2013",     "ABC/ACL 2014" ,  "Catch 2014",     "ABC/ACL 2015",
                                 "Catch 2015" ,    "ABC/ACL 2016",   "Catch 2016",     "ABC/ACL 2017",
                                 "Catch 2017",     "ABC/ACL 2018",   "Catch 2018",     "ABC/ACL 2019",
                                 "Catch 2019",     "ABC/ACL 2020",   "Catch 2020"),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(Var, c("Var", "Time"), sep = " ") %>%
    dplyr::mutate(Var = paste(spec,"-", Var),
                  EPU = c("MAB")) %>%
    dplyr::select(!c(Notes, spec) ) %>%
    dplyr::filter(! Value == "-",
                  !Value == "n/a") %>%
    dplyr::mutate(Value = as.numeric(Value),
                  Time = as.numeric(Time),
                  Units = c("mt")) %>%
    dplyr::select(Time, Var, Value, EPU, Units) %>%
    dplyr::filter(!Var %in% c("Blueline Tilefish Commercial - ABC/ACL", "Blueline Tilefish Commercial - Catch",
                              "Blueline Tilefish Recreational - ABC/ACL", "Blueline Tilefish Recreational - Catch"))


  blueline<-   readxl::read_excel(file.path(raw.dir,abc.acl_xlsx)) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::rename(spec = "Species/Sector") %>%
    tidyr::pivot_longer(cols = c("ABC/ACL 2012",   "Catch 2012",     "ABC/ACL 2013" ,
                                 "Catch 2013",     "ABC/ACL 2014" ,  "Catch 2014",     "ABC/ACL 2015",
                                 "Catch 2015" ,    "ABC/ACL 2016",   "Catch 2016",     "ABC/ACL 2017",
                                 "Catch 2017",     "ABC/ACL 2018",   "Catch 2018",     "ABC/ACL 2019",
                                 "Catch 2019",     "ABC/ACL 2020",   "Catch 2020"),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(Var, c("Var", "Time"), sep = " ") %>%
    dplyr::mutate(Var = paste(spec,"-", Var),
                  EPU = c("MAB")) %>%
    dplyr::select(!c(Notes, spec) ) %>%
    dplyr::filter(! Value == "-",
                  !Value == "n/a") %>%
    dplyr::mutate(Value = as.numeric(Value),
                  Time = as.numeric(Time)) %>%
    dplyr::filter(Var %in% c("Blueline Tilefish Commercial - ABC/ACL", "Blueline Tilefish Commercial - Catch",
                              "Blueline Tilefish Recreational - ABC/ACL", "Blueline Tilefish Recreational - Catch")) %>%
    dplyr::mutate(Value = Value/1000000,
                  Units = c("mt")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  ne<- readxl::read_excel(file.path(raw.dir, NEFMC_abc_acl_xlsx)) %>%
    tidyr::pivot_longer(cols = c("Quota", "Catch_Landings"), names_to = "Var",
                        values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Species, "-", Var),
                  EPU = c("NE"),
                  Units = c("mt")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  abc.acl <- ma %>% rbind(blueline, ne)
  # metadata ----
  attr(abc.acl, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/mafmc-abcacl-and-catch.html"
  attr(abc.acl, "data_files")   <- list(
    abc.acl_xlsx = abc.acl_xlsx)
  attr(abc.acl, "data_steward") <- c(
    "Brandon Muffley <bmuffley@mafmc.org>")
  attr(abc.acl, "plot_script") <- list(
    `mf_MAB` = "human_dimensions_MAB.Rmd-abc-acl.R")

  if (save_clean){
    usethis::use_data(abc.acl, overwrite = T)
  } else {
    return(abc.acl)
  }
}
get_abc.acl(save_clean = T)

# groundfish<- abc.acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   filter(stringr::str_detect(Fishery, "Groundfish")) %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Quota, Catch_Landings) %>%
#   dplyr::mutate(Fishery = "Groundfish",
#                 EPU = "NE",
#                 Units = "mt")
#
# sm_mesh<- abc.acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   filter(stringr::str_detect(Fishery, "Small Mesh")) %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Quota, Catch_Landings) %>%
#   dplyr::mutate(Fishery = "Small Mesh",
#                 EPU = "NE",
#                 Units = "mt")
#
# abc.acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   dplyr::filter(!stringr::str_detect(Fishery, "Groundfish"),
#                 !stringr::str_detect(Fishery, "Small Mesh")) %>%
#   rbind(groundfish, sm_mesh) %>%
#   dplyr::mutate(Value = Catch_Landings/Quota) %>%
#   ggplot2::ggplot()+
#   ggplot2::geom_bar(aes( y = Value, x = Time, fill = Fishery),
#                     stat="identity", position = "stack" )+
#   ggplot2::ggtitle("ABC or ACL for NEFMC Managed Species")+
#   #ggplot2::ylab(expression("ABC/ACL"))+
#   ggplot2::theme(legend.text = element_text(size = 10),
#                  legend.key.height = unit(2, "mm"))+
#   ggplot2::ylab("ABC or ACL")+
#   ggplot2::xlab(element_blank())+
#   ecodata::theme_ts()+
#   ecodata::theme_title()


