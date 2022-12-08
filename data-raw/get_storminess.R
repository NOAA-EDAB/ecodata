## Storminess Indicator

library(tidyverse)

raw.dir <- here::here("data-raw/")
storm.xlsx <- "Wind_wave_count - Arthur T DeGaetano - NOAA Affiliate.xlsx"

get_storminess <- function(save_clean = F){

  d <- readxl::read_excel(file.path(raw.dir, storm.xlsx))
  storminess <- d %>%
    dplyr::select(!...2) %>%
    tidyr::pivot_longer(cols = c("Southern Mid-Atlantic Bight",
                                 "Northern Mid-Atlantic Bight",
                                 "Georges Bank",
                                 "Western Gulf of Maine",
                                 "Eastern Gulf of Maine"),
      names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(units = "Number of Events",
                  EPU = dplyr::recode(Var,
                                      "Southern Mid-Atlantic Bight" = "MAB",
                                      "Northern Mid-Atlantic Bight" = "MAB",
                                      "Georges Bank" = "GB",
                                      "Western Gulf of Maine" = "GOM",
                                      "Eastern Gulf of Maine" = "GOM"))



  if (save_clean){
    usethis::use_data(storminess, overwrite = T)
  } else {
    return(storminess)
  }
}
get_storminess(save_clean = T)


