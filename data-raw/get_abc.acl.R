#### get ABC/ACL Catch from MAFMC

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

abc.acl_xlsx <- "ABC_ACL_catch - Brandon Muffley - NOAA Affiliate.xlsx"
get_abc.acl <- function(save_clean = F){
  # import data
  abc.acl<-read_excel(file.path(raw.dir,abc.acl_xlsx)) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::rename(spec = "Species/Sector") %>%
    tidyr::pivot_longer(cols = c("ABC/ACL 2012",   "Catch 2012",     "ABC/ACL 2013" ,
                                 "Catch 2013",     "ABC/ACL 2014" ,  "Catch 2014",     "ABC/ACL 2015",
                                 "Catch 2015" ,    "ABC/ACL 2016",   "Catch 2016",     "ABC/ACL 2017",
                                 "Catch 2017",     "ABC/ACL 2018",   "Catch 2018",     "ABC/ACL 2019",
                                 "Catch 2019",     "ABC/ACL 2020",   "Catch 2020"),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(Var, c("Var", "Time"), sep = " ") %>%
    dplyr::mutate(Var = paste(spec, Var),
                  EPU = c("MAB"))  %>%
    dplyr::select(!c(Notes, spec) ) %>%
    dplyr::filter(! Value == "-",
                  !Value == "n/a") %>%
    dplyr::mutate(Value = as.numeric(Value),
                  Time = as.numeric(Time))


  if (save_clean){
    usethis::use_data(abc.acl, overwrite = T)
  } else {
    return(abc.acl)
  }
}
get_abc.acl(save_clean = T)





