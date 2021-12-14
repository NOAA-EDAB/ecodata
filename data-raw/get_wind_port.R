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
  port_total<-read_excel(file.path(raw.dir,wind_port_xlsx)) %>%
    dplyr::select(VTR_PORT_ST,PORT_MAX_WEA_DOLLAR_08_19 ) %>%
    tidyr::separate(VTR_PORT_ST, into = c("City", "State"), sep = ",") %>%
    dplyr::rename(port_total = "PORT_MAX_WEA_DOLLAR_08_19")
  # import data
  wind_port<-read_excel(file.path(raw.dir,wind_port_xlsx)) %>%
    #tidyr::separate(VTR_PORT_ST, into = c("City", "State"), sep = ",") %>%
    dplyr::rename("perc_rev" ="%MAX_WEA_DOLLAR_08_19",
                  "City" = "VTR_PORT_ST") %>%
    mutate(perc_rev = perc_rev*100,
      perc_remain = 100 - perc_rev) %>%
    dplyr::select(City, perc_rev, perc_remain) %>%
    tidyr::pivot_longer(cols = c(perc_rev, perc_remain),
                        names_to = "Var", values_to = "Value") %>%
    tidyr::separate(City, into = c("City", "State"), sep = ",") %>%
    left_join(df, by = "State") %>%
    left_join(port_total)


  if (save_clean){
    usethis::use_data(wind_port, overwrite = T)
  } else {
    return(wind_port)
  }
}
get_wind_port(save_clean = T)
