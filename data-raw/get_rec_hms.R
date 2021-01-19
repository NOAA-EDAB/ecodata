## HMS sharks occurence on the shelf
# Data from MRIP - https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads
# Seleted species from categories from Duartes lists from observer data
library(tidyverse)

raw.hms.dir <- here::here("data-raw/hms-mrip")
raw.dir <- here::here("data-raw")

get_rec_hms <- function(save_clean = F){
## Bring in data
mrip_csv <- "mrip_estim_catch_year_1981_2020.csv"
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
  dplyr::summarise(Value = sum(LANDING)) %>% #  Definition of Landings. The total number of fish removed from the fishery resource.
  #May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
  dplyr::rename( Time = YEAR,
                 Var = SP_CATEGORY,
                 Region = SUB_REG) %>%
  dplyr::mutate(Region = as.character(Region)) %>%
  dplyr::mutate(Region = dplyr::recode(Region,
                                       `4` = "New England",
                                       `5` = "Mid-Atlantic"))

if (save_clean){
  usethis::use_data(rec_hms, overwrite = T)
} else {
  return(rec_hms)
}
# metadata ----
attr(rec_hms, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/slopewater-proportions.html"
attr(rec_hms, "data_files")   <- list(
  mrip_csv = mrip_csv,
  hms_cat = hms_cat)
attr(rec_hms, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")
}
get_rec_hms(save_clean = T)




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
