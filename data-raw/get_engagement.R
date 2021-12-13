# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-doc/fishery-reliance-and-social-vulnerability.html

library(tidyverse)
library(readxl)
library(janitor)


raw.dir <- here::here("data-raw")

engagement_xlsx<-"EJ_in_Top_Fishing_Communities_WENG_121021 - Changhua Weng - NOAA Affiliate.xlsx"
get_eng_rel <- function(save_clean = F){

  ## MAB
  d1 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "Mid-Atlantic_Bubble Chart")

  dta <- d1 %>%
    dplyr::slice(-(15:17)) %>% # Removes the rows containing new and original names
    dplyr::rename("Eng" = "Commercial Engagement Index",
                  "Rel" = "Commercial Reliance Index") %>%
    dplyr::mutate(Fishery = c(rep("Commercial",14),rep("Recreational",16)),
                  Region = c("MAB"))
  ## NE
  d2 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "New England_Bubble Chart")

  dta2 <- d2 %>%
    slice( -(18:22)) %>% # Removes the rows containing new and original names
    dplyr::rename("Eng" = "Commercial Engagement Index",
                  "Rel" = "Commercial Reliance Index")  %>%
    dplyr::mutate(Fishery = c(rep("Commercial",17),rep("Recreational",16)),
                  Region = c("NE"))


  engagement <- dta %>% rbind(dta2) %>%
    dplyr::rename("Community" = "Commuity Name",
                  "Rating" = "EJ Rating") %>%
    dplyr::mutate(Eng = as.numeric(Eng),
                  Rel = as.numeric(Rel)) %>%
    dplyr::filter(!Rating == "NA")


  # metadata ----
  attr(engagement, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/community-engagement.html"
  attr(engagement, "data_files")   <- list(
    engagement_xlsx = engagement_xlsx)
  attr(engagement, "data_steward") <- c(
    "Lisa Colburn <lisa.colburn@noaa.gov>",
    "Changhua Weng <changhua.weng@noaa.gov>")
  attr(engagement, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-engagement.R",
    `hd_MAB_commercial` = "human_dimensions_MAB.Rmd-commercial-engagement.R",
    `hd_MAB_recreational` = "human_dimensions_MAB.Rmd-recreational-engagement.R",
    `hd_NE` = "human_dimensions_NE.Rmd-engagement.R",
    `hd_NE_commercial` = "human_dimensions_NE.Rmd-commercial-engagement.R",
    `hd_NE_recreational` = "human_dimensions_NE.Rmd-recreational-engagement.R")

  if (save_clean){
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
}
get_eng_rel(save_clean = T)



