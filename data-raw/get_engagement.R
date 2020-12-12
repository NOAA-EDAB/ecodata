# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-doc/fishery-reliance-and-social-vulnerability.html

library(tidyverse)
library(readxl)
library(janitor)


raw.dir <- here::here("data-raw")

engagement_xlsx<-"SocVul_in_Top_Fishing_Communities_120820_WENG - Changhua Weng - NOAA Affiliate.xlsx"
get_eng_rel <- function(save_clean = F){

  ## MAB
  d1 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "Top 10 Communities_Mid-Atlantic")
  col_names <- d1 %>%
    slice(1)
  dta <- d1 %>%
    slice(-1, -(15:34)) %>% # Removes the rows containing new and original names
    rlang::set_names(., nm = col_names) %>%
    dplyr::mutate(Fishery = c(rep("Commercial",13),rep("Recreational",17)))
  ## NE
  d2 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "Top 10 Communities_New England")
  col_names <- d2 %>%
    slice(1)
  dta2 <- d2 %>%
    slice(-1, -(19:34)) %>% # Removes the rows containing new and original names
    rlang::set_names(., nm = col_names) %>%
    dplyr::mutate(Fishery = c(rep("Commercial",17),rep("Recreational",14)))

  col.names <-c("Region", "StAbb", "Community", "Eng", "Rel", "Rating", "Fishery")
  engagement <- dta %>% rbind(dta2) %>%
    dplyr::mutate(ComEng = as.numeric(ComEng),
                  ComRel = as.numeric(ComRel))
  colnames(engagement) = col.names

  if (save_clean){
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
  # metadata ----
  attr(engagement, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/community-engagement.html"
  attr(engagement, "data_files")   <- list(
    engagement_xlsx = engagement_xlsx)
  attr(engagement, "data_steward") <- c(
    "Lisa Colburn <lisa.colburn@noaa.gov>",
    "Changhua Weng <changhua.weng@noaa.gov>")
}
get_eng_rel(save_clean = T)



