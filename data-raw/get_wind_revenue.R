library(dplyr)
library(tidyr)
library(readxl)

raw.dir <- here::here("data-raw")
wind_rev_xlsx<- "Active OSW Projects_Landings-Revenues_SOE 2026_Christel - Douglas Christel - NOAA Federal.xlsx"

get_wind_revenue<- function(save_clean = F){
  ne_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "NEFMC Land-Revenue") %>%
    dplyr::slice(-1) %>%
    janitor::row_to_names(.,1) %>%
    dplyr::mutate(EPU = "NE")

  ne_wind_revenue <- ne_wind_revenue[-c(307:407),] %>%
    dplyr::rename(Time = Year,
                  sum_value = "Sum of Value (2024 dollars)",
                  sum_landing = "Sum of Landings (pounds*)")

  mab_wind_revenue <- readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                         sheet = "MAFMC Land-Revenue") %>%
    dplyr::slice(-1) %>%
    janitor::row_to_names(.,1) %>%
        dplyr::mutate(EPU = "MAB")

  mab_wind_revenue <- mab_wind_revenue %>%
    dplyr::group_by(Species) %>%
    dplyr::arrange(Year, .by_group = T) %>%
    dplyr::rename(Time = Year,
                  sum_value = "Sum of 2024 Value ($)",
                  sum_landing = "Sum of Landings (pounds*)")

  wind_revenue <- ne_wind_revenue %>%
    rbind(mab_wind_revenue) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Atlantic Surfclam" = "Surfclam")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Atlantic Herring" = "Atlantic herring")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Atlantic Halibut" = "Atlantic halibut")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Atlantic Cod" = "Atlantic cod")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Witch Flounder" = "Witch flounder")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Winter Flounder" = "Winter flounder")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Windowpane Flounder" = "Windowpane flounder")) %>%
    dplyr::mutate(Species = dplyr::recode(Species, "Yellowtail Flounder" = "Yellowtail flounder")) %>%
    tidyr::pivot_longer(cols = c(sum_landing, sum_value),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Species, "-", Var)) %>%
    dplyr::select(!Species) %>%
    tibble::as_tibble() %>%
    dplyr::select(Time, Var, Value, EPU) %>%
    dplyr::mutate(Value = as.double(Value),
                  Time = as.integer(Time)) %>%
    dplyr::mutate(Value = dplyr::na_if(Value, 0))

  if (save_clean){
    usethis::use_data(wind_revenue, overwrite = TRUE)
  } else {
    return(wind_revenue)
  }
}
get_wind_revenue(save_clean = T)
