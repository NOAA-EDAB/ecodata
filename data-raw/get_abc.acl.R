#### get ABC/ACL Catch

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

abc_acl_xlsx <- "MAFMC_ABC_ACL_catch through 2023 - Brandon Muffley - NOAA Affiliate.xlsx"
NEFMC_abc_acl_xlsx <- "NEFMC_abc_acl.xlsx"

get_abc_acl <- function(save_clean = F){
  # import data
  ma<-readxl::read_excel(file.path(raw.dir,abc_acl_xlsx)) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::rename("spec" = "Species/Sector") %>%
    tidyr::pivot_longer(cols = c("ABC or ACL 2012", "Catch 2012",
                                 "ABC or ACL 2013", "Catch 2013",
                                 "ABC or ACL 2014", "Catch 2014",
                                 "ABC or ACL 2015", "Catch 2015" ,
                                 "ABC or ACL 2016",   "Catch 2016",
                                 "ABC or ACL 2017", "Catch 2017",
                                 "ABC or ACL 2018",   "Catch 2018",
                                 "ABC or ACL 2019","Catch 2019",
                                 "ABC or ACL 2020","Catch 2020",
                                 "ABC or ACL 2021","Catch 2021",
                                 "ABC or ACL 2022","Catch 2022",
                                 "ABC or ACL 2023","Catch 2023"),
                        names_to = "Var", values_to = "Value") %>%
    #tidyr::separate(Var, c("Var", "Time"), sep = " ") %>%
    dplyr::mutate(#Var = paste(spec,"-", Var),
                  Time = stringr::str_extract(Var, pattern = "\\d+"),
                  Var = stringr::str_extract(Var, pattern = "."),
                  Var = dplyr::recode(Var, "A" = "Quota",
                                      "C" = "Catch"),
                  EPU = c("MAB"),
                  Var = paste0(spec, "_", Var)) %>%
    dplyr::select(!c(Notes, spec)) %>%
    dplyr::filter(!Value == "-",
                  !Value == "n/a",
                  !Var == "NA",
                  !Var == "Ecosystem Component forage species_Catch") %>%
    dplyr::mutate(Value = as.numeric(Value),
                  Time = as.numeric(Time),
                  Units = c("mt"),
                  EPU = c("MAB")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  ne<- readxl::read_excel(file.path(raw.dir, NEFMC_abc_acl_xlsx), sheet = 3) %>%
    dplyr::mutate(Catch = as.numeric(Catch),
                  ABC = as.numeric(ABC),
                  ACL = as.numeric(ACL),
                  TAL = as.numeric(TAL)) %>%
    dplyr::filter(!FMP == "NA") %>%
    tidyr::pivot_longer(cols = c("Catch", "ABC", "ACL", "TAL"), names_to = "Var",
                        values_to = "Value") %>%
    dplyr::mutate(Var = paste0(FMP, "_", Species, "_", Var),
                  EPU = c("NE")) %>%
    dplyr::select(Time, Var, Value, EPU, Units)


  abc_acl <- ma %>% rbind(ma, ne) %>%
    tibble::as_tibble()


  # metadata ----
  attr(abc_acl, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/mafmc-abcacl-and-catch.html"
  attr(abc_acl, "data_files")   <- list(
    abc_acl_xlsx = abc_acl_xlsx)
  attr(abc_acl, "data_steward") <- c(
    "Brandon Muffley <bmuffley@mafmc.org>")
  attr(abc_acl, "plot_script") <- list(
    `mf_MAB` = "human_dimensions_MAB.Rmd-abc-acl.R")

  if (save_clean){
    usethis::use_data(abc_acl, overwrite = T)
  } else {
    return(abc_acl)
  }
}
get_abc_acl(save_clean = T)

# groundfish<- abc_acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   filter(stringr::str_detect(Fishery, "Groundfish")) %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Quota, Catch_Landings) %>%
#   dplyr::mutate(Fishery = "Groundfish",
#                 EPU = "NE",
#                 Units = "mt")
#
# sm_mesh<- abc_acl %>% filter(EPU == "NE") %>%
#   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "Value") %>%
#   filter(stringr::str_detect(Fishery, "Small Mesh")) %>%
#   dplyr::group_by(Time) %>%
#   dplyr::summarise(Quota, Catch_Landings) %>%
#   dplyr::mutate(Fishery = "Small Mesh",
#                 EPU = "NE",
#                 Units = "mt")
#
# abc_acl %>% filter(EPU == "NE") %>%
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


