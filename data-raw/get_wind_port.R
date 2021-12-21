# Wind Port Revenue and Social Indicators

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

#### Angela Silva
wind_port_xlsx<-"SoE2022_Wind_Ports_Social Indicators - Angela Silva - NOAA Affiliate.xlsx"

get_wind_port <- function(save_clean = F){
  df<- data.frame(State = c(" ME", " MA", " RI", " CT", " NY", " NJ", " MD", " VA", " NC"),
                  EPU = c("NE", "NE", "MAB", "MAB","MAB","MAB","MAB","MAB","MAB"))

  # import data
  wind_port<-read_excel(file.path(raw.dir,wind_port_xlsx)) %>%
    #tidyr::separate(VTR_PORT_ST, into = c("City", "State"), sep = ",") %>%
    dplyr::rename("perc_rev" ="%MAX_WEA_DOLLAR_08_19",
                  "City" = "VTR_PORT_ST",
                  "wea_rev" = "PORT_MAX_WEA_DOLLAR_08-19",
                  "pop_comp" = "Population Composition",
                  "pers_disr" = "Personal Disruption",
                  "house_disr" = "Housing Disruption",
                  "ret_mig" = "Retiree Migration",
                  "urb_sprawl"="Urban Sprawl") %>%
    dplyr::mutate(City = dplyr::recode(City,"DAVISVILLE, RI" = "NORTH KINGSTOWN, RI"),
           City = dplyr::recode(City,"HAMPTON BAY, NY" ="HAMPTON BAY/SHINNECOCK, NY"),
           City = dplyr::recode(City,"SHINNECOCK, NY" = "HAMPTON BAY/SHINNECOCK, NY"),
           City = dplyr::recode(City,"BARNEGAT, NJ" = "BARNEGAT LIGHT, NJ" ),
           City = dplyr::recode(City,"LONG BEACH, NJ" = "BARNEGAT LIGHT, NJ"),
           City = dplyr::recode(City,"MENEMSHA, MA" = "CHILMARK, MA"),
           City = dplyr::recode(City,"BASS RIVER/YARMOUTH, MA" = "BASS RIVER, MA")) %>%
    dplyr::group_by(City) %>%
    dplyr::mutate( wea_rev = sum(wea_rev)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
           perc_rev = perc_rev*100,
           total_rev = (wea_rev*100)/perc_rev,
           EJ = pmax(Poverty, pop_comp, pers_disr ),
           gentrification = pmax(house_disr, ret_mig, urb_sprawl)) %>%
    dplyr::select(City, wea_rev, total_rev, EJ, gentrification) %>%
    tidyr::pivot_longer(cols = c(wea_rev, total_rev, EJ, gentrification),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(City, into = c("City", "State"), sep = ",") %>%
    left_join(df, by = "State")


  if (save_clean){
    usethis::use_data(wind_port, overwrite = T)
  } else {
    return(wind_port)
  }
}
get_wind_port(save_clean = T)
