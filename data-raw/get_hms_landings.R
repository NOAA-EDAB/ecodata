## HMS Landings

library(tidyverse)


raw.dir <- here::here("data-raw")
hms_landings_xlsx<-"HMS Landings Data_20242025_Final - Jennifer Cudney - NOAA Federal.xlsx"

get_hms_landings <- function(save_clean = F){

  hms_landings<- readxl::read_excel(file.path(raw.dir, hms_landings_xlsx))

  hms_landings<-hms_landings %>%
    dplyr::mutate(HMS_Groups = recode(HMS_Groups, "BAYS (Bigeye, Albacore, Yellowfin, Skipjack) tunas" = "BAYS")) %>%
    tidyr::unite(Var, HMS_Groups, VAR_wt, sep = "_") %>%
    dplyr::mutate(EPUs = dplyr::recode(EPUs, "New England" = "NE", "Mid-Atlantic Bight" = "MAB")) %>%
    dplyr::rename(Time = YEAR, Value = Total, EPU = EPUs, Units = Units_WT) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    dplyr::select(Time, Var, Value, EPU)


  if (save_clean){
    usethis::use_data(hms_landings, overwrite = T)
  } else {
    return(hms_landings)
  }

  # metadata ----
  attr(hms_landings, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/highly-migratory-species-landings.html"
  attr(hms_landings, "data_files")   <- list(
    hms_landings_xlsx = hms_landings_xlsx)
  attr(hms_landings, "data_steward") <- c(
    "Carrie Solatnoff <carrie.solatnoff@noaa.gov>")
}
get_hms_landings(save_clean = T)
