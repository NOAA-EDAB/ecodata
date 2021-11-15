## HMS sharks occurence on the shelf
# Data from MRIP - https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads
# Seleted species from categories from Duartes lists from observer data
library(tidyverse)

raw.hms.dir <- here::here("data-raw/hms-mrip")
raw.dir <- here::here("data-raw")

get_lps_sharks <- function(save_clean = F){
  ## Bring in data
  lps_csv <- "LPS_shark_catch.csv"
  #species_list <- "species_list.csv"
  hms_cat<- "hms_sp_category.csv"

  d <- read.csv(file.path(raw.dir,lps_csv))
  sp <- read.csv(file.path(raw.hms.dir,species_list)) %>%
    dplyr::rename(SP_CODE = sp_code) %>%
    dplyr::select(SP_CODE, COMMON_NAME)
  sp_cat <- read.csv(file.path(raw.hms.dir,hms_cat))

 region<- data.frame(State.Area = c("VIRGINIA" ,"MARYLAND/DELAWARE","SOUTH NEW JERSEY", "NORTH NEW JERSEY","NEW YORK",
            "CONNECTICUT/RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE/MAINE"),
                    EPU = c("MAB","MAB","MAB","MAB","MAB","MAB","NE", "NE"))

  ## select those in NEUS Shelf
  sharks<- d %>%
    dplyr::rename("COMMON_NAME" = Species,
                  "Total_Catch" = "Total.Catch..Kept.Alive.Dead.") %>%
    dplyr::mutate(COMMON_NAME=recode(COMMON_NAME, 'PORBEAGLE SHARK'='PORBEAGLE',
                                     'COMMON THRESHER'='THRESHER SHARK')) %>%
    dplyr::filter(!Total_Catch == ".") %>%
    left_join(sp_cat, by= "COMMON_NAME") %>%
    left_join(region, by = "State.Area") %>%
    dplyr::group_by(Year, SP_CATEGORY, EPU) %>%
    dplyr::mutate(Total_Catch = as.numeric(Total_Catch)) %>%
    dplyr::summarise(Value = sum(Total_Catch)) %>%
    dplyr::rename( Time = Year,
                   Var = SP_CATEGORY) %>%
    dplyr::mutate(Units = "N of Fish")

  # metadata ----
  attr(sharks, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html"
  attr(sharks, "data_files")   <- list(
    lps_csv = lps_csv,
    hms_cat = hms_cat)
  attr(sharks, "data_steward") <- c(
    "Kimberly Bastille <kimberly.bastille@noaa.gov>")
  attr(sharks, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-rec_hms.R",
    `hd_NE` = "human_dimensions_NE.Rmd-rec_hms.R")

  if (save_clean){
    usethis::use_data(sharks, overwrite = T)
  } else {
    return(sharks)
  }
}
get_lps_sharks(save_clean = T)




# library(tidyverse)
#
# raw.hms.dir <- here::here("data-raw/hms-mrip")
# raw.dir <- here::here("data-raw")
#
# get_rec_hms <- function(save_clean = F){
#   ## Bring in data
#   mrip_csv <- "mrip_shark.xlsx"
#   species_list <- "species_list.csv"
#   hms_cat<- "hms_sp_category.csv"
#
#   d <- readxl::read_xlsx(file.path(raw.hms.dir,mrip_csv)) %>%
#     dplyr::rename(COMMON_NAME = `Common Name`)
#   sp <- read.csv(file.path(raw.hms.dir,species_list)) %>%
#     dplyr::rename(SP_CODE = sp_code) %>%
#     dplyr::select(SP_CODE, COMMON_NAME)
#   sp_cat <- read.csv(file.path(raw.hms.dir,hms_cat))
#
#   region<- data.frame(State = c("VIRGINIA" ,"MARYLAND","DELAWARE","SOUTH NEW JERSEY", "NORTH NEW JERSEY",
#                                 "NEW YORK", "NEW JERSEY",
#                                      "CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE/MAINE"),
#                       EPU = c("MAB","MAB","MAB","MAB", "MAB","MAB", "MAB","MAB","MAB","NE", "NE"))
#
#   ## select those in NEUS Shelf
#   rec_hms<- d %>%
#     #dplyr::filter(SUB_REG <= 5) %>%  # 4 = North Atlantic, 5 = Mid atlantic
#     left_join(sp_cat, by= "COMMON_NAME") %>% # merge catch year with common names and category
#     left_join(region, by = "State") %>%
#     dplyr::rename(Total_Catch = `Total Catch (A+B1+B2)`) %>%
#     dplyr::group_by(Year, SP_CATEGORY, EPU) %>%
#     dplyr::mutate(Total_Catch = as.numeric(Total_Catch)) %>%
#     dplyr::summarise(Value = sum(Total_Catch)) %>%
#     dplyr::rename( Time = Year,
#                    Var = SP_CATEGORY) %>%%>%
#     dplyr::mutate(Units = "N of Fish") %>%
#     dplyr::filter(!EPU == "NA")
#
#   # metadata ----
#   attr(rec_hms, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/slopewater-proportions.html"
#   attr(rec_hms, "data_files")   <- list(
#     mrip_csv = mrip_csv,
#     hms_cat = hms_cat)
#   attr(rec_hms, "data_steward") <- c(
#     "Kimberly Bastille <kimberly.bastille@noaa.gov>")
#   attr(rec_hms, "plot_script") <- list(
#     `hd_MAB` = "human_dimensions_MAB.Rmd-rec_hms.R",
#     `hd_NE` = "human_dimensions_NE.Rmd-rec_hms.R")
#
#   if (save_clean){
#     usethis::use_data(rec_hms, overwrite = T)
#   } else {
#     return(rec_hms)
#   }
# }
# get_rec_hms(save_clean = T)




#
# lps<- ecodata::sharks %>%
#   mutate(source = "LPS")
#
# ecodata::rec_hms %>%
#   mutate(source = "MRIP") %>%
#   rbind(lps) %>%
#   filter(EPU == "MAB",
#          Var == "Pelagic") %>%
#   ggplot()+
#   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
#                     xmin = x.shade.min , xmax = x.shade.max,
#                     ymin = -Inf, ymax = Inf) +
#   ggplot2::geom_point(aes(x=Time, y = Value, color = source))+
#   ggplot2::geom_line(aes(x=Time, y = Value, color = source))+
#   ggplot2::ggtitle("MAB Pelagic Rec Sharks")+
#   ggplot2::ylab("Number of Fish")+
#   ggplot2::xlab(element_blank())+
#   ggplot2::scale_color_discrete(name = "Source")+
#   ecodata::theme_ts()+
#   ecodata::theme_title()




library(tidyverse)

raw.hms.dir <- here::here("data-raw/hms-mrip")
raw.dir <- here::here("data-raw")

get_rec_hms <- function(save_clean = F){
  ## Bring in data
  mrip_csv <- "mrip_estim_catch_year_1981_2021.csv"
  species_list <- "species_list.csv"
  hms_cat<- "hms_sp_category.csv"

  d <- read.csv(file.path(raw.hms.dir,mrip_csv))
  sp <- read.csv(file.path(raw.hms.dir,species_list)) %>%
    dplyr::rename(SP_CODE = sp_code) %>%
    dplyr::select(SP_CODE, COMMON_NAME)
  sp_cat <- read.csv(file.path(raw.hms.dir,hms_cat))



  ## select those in NEUS Shelf
  rec_hms<- d %>%
    dplyr::filter(SUB_REG <= 5) %>%  # 4 = North Atlantic, 5 = Mid atlantic
    left_join(sp_cat, by= "SP_CODE") %>% # merge catch year with common names and category
    dplyr::group_by(YEAR, SP_CATEGORY, SUB_REG) %>%
    dplyr::summarise(Value = sum(TOT_CAT)) %>% #  Definition of Landings. The total number of fish removed from the fishery resource.
    #May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
    dplyr::rename( Time = YEAR,
                   Var = SP_CATEGORY,
                   Region = SUB_REG) %>%
    dplyr::mutate(Region = as.character(Region)) %>%
    dplyr::mutate(EPU = dplyr::recode(Region,
                                         `4` = "NE",
                                         `5` = "MAB"))
  rec_hms$Var[is.na(rec_hms$Var)]<- "Unclassified"
  # metadata ----
  attr(rec_hms, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/slopewater-proportions.html"
  attr(rec_hms, "data_files")   <- list(
    mrip_csv = mrip_csv,
    hms_cat = hms_cat)
  attr(rec_hms, "data_steward") <- c(
    "Kimberly Bastille <kimberly.bastille@noaa.gov>")
  attr(rec_hms, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-rec_hms.R",
    `hd_NE` = "human_dimensions_NE.Rmd-rec_hms.R")

  if (save_clean){
    usethis::use_data(rec_hms, overwrite = T)
  } else {
    return(rec_hms)
  }
}
get_rec_hms(save_clean = T)


#############
# test<- rec_hms %>% filter(SP_CATEGORY == "Pelagic",
#                           SUB_REG == 4)  %>%
#   ggplot()+
#   geom_bar(aes(x = YEAR, y = LANDING, fill = COMMON_NAME), stat = "identity")
#



###### Test plot for individual spp
# rec_hms<- d %>%
#   dplyr::filter(SUB_REG <= 5) %>%  # 4 = North Atlantic, 5 = Mid atlantic
#   left_join(sp_cat, by= "SP_CODE") %>% # merge catch year with common names and category
#   dplyr::group_by(YEAR, COMMON_NAME, SUB_REG) %>%
#   dplyr::summarise(Value = sum(LANDING)) %>% #  Definition of Landings. The total number of fish removed from the fishery resource.
#   #May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
#   dplyr::rename( Time = YEAR,
#                  Var = COMMON_NAME,
#                  Region = SUB_REG) %>%
#   dplyr::mutate(Region = as.character(Region)) %>%
#   dplyr::mutate(Region = dplyr::recode(Region,
#                                        `4` = "New England",
#                                        `5` = "Mid-Atlantic")) %>%
#   filter(Region == "Mid-Atlantic") %>%
#   ggplot(aes(x=Time, y=Value))+
#   geom_point()+
#   geom_line()+
#   facet_wrap(~Var, scale = "free")
