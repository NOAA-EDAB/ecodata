# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-doc/fishery-reliance-and-social-vulnerability.html

library(tidyverse)
library(readxl)
library(janitor)


raw.dir <- here::here("data-raw")

engagement_xlsx<-"EJ_in_Top_Fishing_Communities_WENG_120623 - Changhua Weng - NOAA Affiliate.xlsx"
get_eng_rel <- function(save_clean = F){

  ## MAB
  d1 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "Mid-Atlantic_Bubble Chart")

  dta <- d1 %>%
    dplyr::slice(-(17:21), -39) %>% # Removes the rows containing new and original names
    dplyr::rename("Eng" = "Commercial Engagement Index",
                  "Rel" = "Commercial Reliance Index") %>%
    dplyr::mutate(Fishery = c(rep("Commercial",16),rep("Recreational",17)),
                  Region = c("MAB"))

  # d3 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "Mid-Atlantic_Radar Graph")
  #
  # dta3 <- d3 %>%
  #   dplyr::slice(-(16:19)) %>% # Removes the rows containing new and original names
  #   dplyr::rename("Eng" = "Commercial Engagement Index",
  #                 "Rel" = "Commercial Reliance Index") %>%
  #   dplyr::mutate(Fishery = c(rep("Commercial",14),rep("Recreational",16)),
  #                 Region = c("MAB"))%>%
  #   left_join(dta)



  ## NE
  d2 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "New England_Bubble Chart")

  dta2 <- d2 %>%
    slice( -(18:21)) %>% # Removes the rows containing new and original names
    dplyr::rename("Eng" = "Commercial Engagement Index",
                  "Rel" = "Commercial Reliance Index")  %>%
    dplyr::mutate(Fishery = c(rep("Commercial",17),rep("Recreational",18)),
                  Region = c("NE"))

  ### NE EJ
  # d4 <-read_excel(file.path(raw.dir, engagement_xlsx), sheet =  "New England_Radar Graph")
  #
  # dta4 <- d4 %>%
  #   dplyr::slice(-(18:22)) %>% # Removes the rows containing new and original names
  #   dplyr::rename("Eng" = "Commercial Engagement Index",
  #                 "Rel" = "Commercial Reliance Index") %>%
  #   dplyr::mutate(Fishery = c(rep("Commercial",17),rep("Recreational",16)),
  #                 Region = c("NE")) %>%
  #   left_join(dta2) %>%
  #   dplyr::select("Commuity Name","Eng","Rel",
  #                 "Personal Disruption Index",
  #                 "Population Composition Index",
  #                 "Poverty Index" ,"1 std","0.5 std",
  #                 "Fishery", "EJ Rating", "Region"  )

  engagement <- dta %>% rbind(dta2) %>%
    dplyr::rename("Community" = "Community Name",
                  "EPU" = "Region") %>%
    dplyr::mutate(Eng = as.numeric(Eng),
                  Rel = as.numeric(Rel),
                  `EJ Rating` = dplyr::recode(`EJ Rating`,
                                              "All Others Communities" = "All Other Communities"))



  if (save_clean){
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
}
get_eng_rel(save_clean = T)



