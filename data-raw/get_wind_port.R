# Wind Port Revenue and Social Indicators

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

#### Angela Silva
wind_port_csv<-"wind_ej_output_2024-11-13.csv"

get_wind_port <- function(save_clean = F){
  df<- data.frame(State = c(" ME", " MA", " RI", " CT", " NY", " NJ", " MD", " VA", " NC"),
                  EPU = c("NE", "NE", "NE", "NE","MAB","MAB","MAB","MAB","MAB"))

  # import data
  wind_port<-read.csv(file.path(raw.dir, wind_port_csv)) %>%
    tidyr::separate(PORT_STATE, into = c("City", "State"), sep = ",") %>%
    dplyr::mutate(City = dplyr::recode(City,"DAVISVILLE" = "NORTH KINGSTOWN"),
           City = dplyr::recode(City,"HAMPTON BAY" ="HAMPTON BAY/SHINNECOCK"),
           City = dplyr::recode(City,"SHINNECOCK" = "HAMPTON BAY/SHINNECOCK"),
           City = dplyr::recode(City,"BARNEGAT" = "BARNEGAT LIGHT" ),
           City = dplyr::recode(City,"LONG BEACH" = "BARNEGAT LIGHT"),
           City = dplyr::recode(City,"BASS RIVER/YARMOUTH" = "BASS RIVER")) %>%
    dplyr::mutate(perc_MAX = perc_MAX*100,
           perc_MIN = perc_MIN*100,
           total_rev = 100 - perc_MIN - perc_MAX) %>%
    dplyr::rename(Gentrification = Gent, MaxVal = WEA_MAX) %>%
    dplyr::select(State, City, perc_MIN, perc_MAX, EJ, Gentrification, MaxVal) %>%
    tidyr::pivot_longer(cols = c(perc_MIN, perc_MAX, EJ, Gentrification, MaxVal),
                        names_to = "Var", values_to = "Value") %>%
    #tidyr::separate(City, into = c("City", "State"), sep = ",") %>%
    left_join(df, by = "State") %>%
    dplyr::arrange(State)


  # metadata ----
  attr(wind_port, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/wea-fishing-port-landings.html"
  attr(wind_port, "data_files")   <- list(
    wind_port_csv = wind_port_csv)
  attr(wind_port, "data_steward") <- c(
    "Angela Silva <angela.silva@noaa.gov>")
  attr(wind_port, "plot_script") <- list(
    `hd_MAB` = "human_dimensions_MAB.Rmd-wea-port-rev.R")

  if (save_clean){
    usethis::use_data(wind_port, overwrite = T)
  } else {
    return(wind_port)
  }
}
get_wind_port(save_clean = T)
