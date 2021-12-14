### SAV
library(readxl)
SAV_xlsx<- "SAV_Table_for_SOE_Report - Laurel Smith - NOAA Federal.xlsx"
get_SAV <- function(save_clean = F){

  # Alexandrium
  SAV <- read_excel(file.path(raw.dir,SAV_xlsx)) %>%
    janitor::row_to_names(1) %>%
    tidyr::pivot_longer(cols = (c("Tidal Fresh Total", "Oligohaline Total", "Mesohaline Total",
                                  "Polyhaline Total",  "Baywide Total"  )),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = c("MAB"),
                  Year =  as.numeric(Year),
                  Value = as.numeric(Value)) %>%
    dplyr::rename(Time = Year)


  if (save_clean){
    usethis::use_data(SAV, overwrite = TRUE)
  } else {
    return(SAV)
  }
}
get_SAV(save_clean = T)


