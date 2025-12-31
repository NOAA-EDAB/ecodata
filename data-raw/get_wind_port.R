# Wind Port Revenue and Social Indicators

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

#### Angela Silva
wind_port_csv <- "wind_port_revenue_tyrell_2025-12-05 - Abigail Tyrell - NOAA Federal.csv"

get_wind_port <- function(save_clean = F) {
  # df<- data.frame(State = c(" ME", " MA", " NH", " RI", " CT", " NY", " NJ", " MD", " VA", " NC"),
  #                 EPU = c("NE", "NE", "NE", "NE", "NE","MAB","MAB","MAB","MAB","MAB"))

  # import data
  wind_port <- read.csv(file.path(raw.dir, wind_port_csv)) %>%
    # dplyr::filter(lease_status == "All leases") %>%
    tidyr::separate(PORT_STATE, into = c("City", "State"), sep = ",") %>%
    # don't think these are necessary anymore
    dplyr::mutate(
      City = dplyr::recode(City, "DAVISVILLE" = "NORTH KINGSTOWN"),
      City = dplyr::recode(City, "HAMPTON BAY" = "HAMPTON BAY/SHINNECOCK"),
      City = dplyr::recode(City, "SHINNECOCK" = "HAMPTON BAY/SHINNECOCK"),
      City = dplyr::recode(City, "BARNEGAT" = "BARNEGAT LIGHT"),
      City = dplyr::recode(City, "LONG BEACH" = "BARNEGAT LIGHT"),
      City = dplyr::recode(City, "BASS RIVER/YARMOUTH" = "BASS RIVER")
    ) %>%
    dplyr::mutate(
      perc_MAX = perc_MAX * 100,
      perc_MIN = perc_MIN * 100,
      perc_AVG = perc_AVG * 100,
      total_rev = 100 - perc_MIN - perc_MAX
    ) %>%
    dplyr::rename(Gentrification = Gent, MaxVal = WEA_MAX) %>%
    dplyr::mutate(
      EPU = dplyr::case_when(
        region == "MidAtlantic" ~ "MAB",
        region == "NewEngland" ~ "NE"
      )
    ) %>%
    dplyr::select(
      State,
      City,
      EPU,
      perc_MIN,
      perc_AVG,
      perc_MAX,
      Demographics,
      Gentrification,
      MaxVal,
      lease_status
    ) %>%
    tidyr::pivot_wider(
      names_from = lease_status,
      values_from = c(
        perc_MIN,
        perc_AVG,
        perc_MAX,
        Demographics,
        Gentrification,
        MaxVal
      ),
      names_sep = "-"
    ) %>%
    tidyr::pivot_longer(
      cols = -(1:3),
      names_to = "Var",
      values_to = "Value"
    ) %>%
    #tidyr::separate(City, into = c("City", "State"), sep = ",") %>%
    dplyr::arrange(State)

  if (save_clean) {
    usethis::use_data(wind_port, overwrite = T)
  } else {
    return(wind_port)
  }
}
get_wind_port(save_clean = T)
