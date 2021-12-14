## Sandlance from Moe Nelson

sandlance_csv<- "Nelson_sand_lance_collocation_data_Silva_et_al_2020_Table1 - David Nelson - NOAA Federal.csv"

get_sandlance <- function(save_clean = F){
  sandlance<- read.csv(file.path(raw.dir,sandlance_csv)) %>%
    janitor::row_to_names(1) %>%
    dplyr::rename("Time" = "Cruise",
                  "Sandlance" = "Sand lance",
                  "Humpback" = "Humpback whale",
                  "GreatShearwater" = "Great shearwater") %>%
    dplyr::select(Time, Sandlance, Humpback, GreatShearwater) %>%
    tidyr::pivot_longer(cols = c("Sandlance","Humpback" ,
                                 "GreatShearwater"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = c("NE")) %>%
    tidyr::separate(Time, into = c("Season", "Year")) %>%
    dplyr::mutate(seas.num = dplyr::case_when(Season == "Fall" ~ 0.75,
                                              Season == "Summer" ~ 0.5,
                                              Season == "Spring" ~ 0.25),
                  Year = as.numeric(Year),
                  Value = as.numeric(Value),
                  Time = Year + seas.num) %>%
    dplyr::select(Time, Var, Value, EPU)



  if (save_clean){
    usethis::use_data(sandlance, overwrite = T)
  } else {
    return(sandlance)
  }
}
get_sandlance(save_clean = T)




