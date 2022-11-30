## HMS sharks occurence on the shelf
# Data from MRIP - https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads

# https://www.st.nmfs.noaa.gov/SASStoredProcess/do? is the data portal
# 1) Select most recent years to add to total dataset
# 2) Data Type: "Estimate: Catch"
# 3) Wave Options: "All Waves"
# 4) Geographical Area: "Unisted States by State"
# 5) Select 30 species from list (ecodata/data-raw/hms-mrip/hms_sp_category.csv)
# 6) Output Form: "Download CSV as Zip File"
# 7) Submit Query

# Seleted species from categories from Duartes lists from observer data
library(tidyverse)

raw.hms.dir <- here::here("data-raw/hms-mrip")
raw.dir <- here::here("data-raw")

get_rec_hms <- function(save_clean = F){
  ## Bring in data
  mrip_csv <- "mrip_estim_catch_year_1981_2022.csv"
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
    dplyr::mutate(EPU = dplyr::recode(Region,
                                      `4` = "NE",
                                      `5` = "MAB") ,
                  Region = dplyr::recode(Region,
                                         `4` = "New England",
                                         `5` = "Mid-Atlantic")) %>%
    dplyr::mutate(Var = paste0(Var, "-", EPU)) %>%
    dplyr::select(Time, Var, Value, EPU)

  # metadata ----
  attr(rec_hms, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html"
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
