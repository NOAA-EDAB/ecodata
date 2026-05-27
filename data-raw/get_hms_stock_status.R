## get HMS Stock Status
library(dplyr)
library(tidyr)
library(ggplot2)

data.dir <- here::here('data-raw')
hms_stock_status_xlsx <- "2025_SAFE_B_F - Jennifer Cudney - NOAA Federal.xlsx"

get_hms_stockstatus <- function(save_clean = F){

  xlsx <- readxl::read_xlsx(file.path(data.dir, hms_stock_status_xlsx))

  hms_stock_status <-
    xlsx %>% dplyr::rename("B.Bmsy" = "current_rel_B_level",
                           "F.Fmsy" = "current_rel_F_level",
                           #"Include" = "Included in Kobe Plot? If No, Why?",
                           "Time" = "rel_F_year") %>%
    #dplyr::filter(Include == "Yes") %>%
    dplyr::select( "species_abr", "species", "Time", "F.Fmsy", "B.Bmsy" ) %>%
    tidyr::pivot_longer(cols = -c("species_abr", "species", "Time"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(species_abr, ":", species, ":", Var),
                  EPU = "ALL") %>%
    dplyr::select(-species_abr, -species) %>%
    dplyr::filter(!Time == "NA")

  if (save_clean){
    usethis::use_data(hms_stock_status, overwrite = T)
  } else {
    return(hms_stock_status)
  }
}
get_hms_stockstatus(save_clean = T)
