## Landings and Revenue in wind lease areas

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

## Doug Christel
wind_rev_xlsx<-"Active OSW Projects_Landings-Revenues_SOE 2026_Christel - Douglas Christel - NOAA Federal.xlsx"
get_wea_landings_rev <- function(save_clean = F){
  # import data
   wea_landings_rev<-readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "Cumulative Max Rev & Land") %>%
    # janitor::row_to_names(.,1) %>%
    #  select("Max % Regional Revenue and Landings of fisheries managed by the Atlantic States Marine Fisheries Commission and the New England and Mid-Atlantic Fishery Management Council within Existing Lease Areas and the Draft Primary and Secondary Central Atlantic Call Areas")

    dplyr::select( "NEFMC and MAFMC Managed Species",
                   "Maximum Percent Total Annual Regional Species Landings",
                   "Maximum Percent Total Annual Regional Species Revenue") %>%
    dplyr::rename("NEFMC, MAFMC, and ASMFC Managed Species" = "NEFMC and MAFMC Managed Species",
                  "perc_landings_max" = "Maximum Percent Total Annual Regional Species Landings",
                  "perc_revenue_max" = "Maximum Percent Total Annual Regional Species Revenue") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(perc_landings_max = as.numeric(perc_landings_max)*100,
                  perc_revenue_max = as.numeric(perc_revenue_max)*100,
                  Units = c("Percent")) %>%
    dplyr::mutate(across(where(is.numeric), ~round(., 0))) %>%
    dplyr::mutate(Council = "NEFMC")

   # Add council data to dataset
    mafmc <- list("Atlantic Mackerel", "Black Sea Bass", "Bluefish*", "Blueline Tilefish*", "Butterfish",
                  "Chub Mackerel", "Golden Tilefish", "Illex Squid", "Longfin Squid",
                  "Ocean Quahog", "Scup", "Summer Flounder", "Atlantic Surfclam")

    for (i in 1:length(mafmc)){
      wea_landings_rev <- wea_landings_rev %>%
        dplyr::mutate(Council = replace(Council, wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species` == mafmc[i], "MAFMC"))
    }

    wea_landings_rev <- wea_landings_rev %>%
      dplyr::mutate(Council = replace(Council, wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species` == "Monkfish", "MAFMC/NEFMC")) %>%
      dplyr::mutate(Council = replace(Council, wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species` == "Spiny Dogfish", "MAFMC/NEFMC"))

  if (save_clean){
    usethis::use_data(wea_landings_rev, overwrite = T)
  } else {
    return(wea_landings_rev)
  }
}
get_wea_landings_rev(save_clean = T)
