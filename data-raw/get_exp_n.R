

library(dplyr)
library(stringr)
library(tidyr)

raw.dir <- here::here("data-raw")
exp_n_rdata <- "Expected_number_of_species_23.RData"
get_exp_n <- function(save_clean = F){

  load(file.path(raw.dir, exp_n_rdata))



  exp_n2<-ESn.epu %>% filter(str_detect(Var,  'Species - Bigelow|Species - Albatross')) %>%
    tidyr::separate(Var, c("Season", "EPU", "trash1","trash2", "trash3", "trash4", "Var")) %>%
    dplyr::select(-trash1, -trash2, -trash3, -trash4)

  exp_n<-ESn.epu %>% filter(str_detect(Var,  'Standard Deviation')) %>%
    tidyr::separate(Var, c("Season", "EPU", "trash1", "trash2", "trash3","trash4",
                           "trash5", "trash6","Var")) %>%
    dplyr::mutate(Var=recode(Var,
                             `Albatross`="AlbatrossSD",
                             `Bigelow`="BigelowSD")) %>%
    dplyr::select(-trash1, -trash2, -trash3, -trash4, -trash5, -trash6) %>%
    rbind(exp_n2)%>%
    tibble::as_tibble() %>%
    dplyr::mutate(Var = paste0(Var, "-", Season)) %>%
    dplyr::select(Time, Var, Value, EPU, Units)

  fill<- data.frame(Time = c(2020,2020,2020,2020,2020,2020,2020,
                             2020,2020,2020,2020,2020,2020,2020,
                             2020,2020,2020,2020,2020,2020,2020),
                    Var = c("AlbatrossSD-FALL","AlbatrossSD-SPRING",
                            "BigelowSD-FALL", "BigelowSD-SPRING",
                            "Albatross-FALL","Albatross-SPRING",
                            "Bigelow-FALL",
                            "AlbatrossSD-FALL","AlbatrossSD-SPRING",
                            "BigelowSD-FALL", "BigelowSD-SPRING",
                            "Albatross-FALL","Albatross-SPRING",
                            "Bigelow-FALL",
                            "AlbatrossSD-FALL","AlbatrossSD-SPRING",
                            "BigelowSD-FALL", "BigelowSD-SPRING",
                            "Albatross-FALL","Albatross-SPRING",
                            "Bigelow-FALL"),
                    Value = c(NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA),
                    EPU = c("MAB", "MAB","MAB","MAB","MAB","MAB","MAB",
                            "GOM","GOM","GOM","GOM","GOM","GOM","GOM",
                            "GB","GB","GB","GB","GB","GB","GB"),
                    Units = c(NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA))

  exp_n<- exp_n %>% rbind(fill)
  # metadata ----
  attr(exp_n, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/expected-number-of-species.html"
  attr(exp_n, "data_files")   <- list(
    exp_n_rdata = exp_n_rdata)
  attr(exp_n, "data_steward") <- c(
    "Sean Lucey <sean.lucey@noaa.gov>")
  attr(exp_n, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-exp-n.R",
    `mf_MAB_spring` = "macrofauna_MAB.Rmd-exp-n-spring.R",
    `mf_NE` = "macrofauna_NE.Rmd-exp-n.R",
    `mf_NE_spring` = "macrofauna_NE.Rmd-exp-n-spring.R")

  if (save_clean){
    usethis::use_data(exp_n, overwrite = T)
  } else {
    return(exp_n)
  }
}
get_exp_n(save_clean = T)
