## Landings and Revenue in wind lease areas

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

## Doug Christel
wind_rev_xlsx<-"Active OSW Projects_Landings-Revenues_SOE 2026_Christel.xlsx"

get_wea_landings_rev <- function(save_clean = F){
  # import data
   wea_landings_rev<-readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "Max and Avg Rev & Land") %>%
    # janitor::row_to_names(.,1) %>%
    #  select("Max % Regional Revenue and Landings of fisheries managed by the Atlantic States Marine Fisheries Commission and the New England and Mid-Atlantic Fishery Management Council within Existing Lease Areas and the Draft Primary and Secondary Central Atlantic Call Areas")

    dplyr::select("NEFMC and MAFMC Managed Species",
                  "Maximum Percent Total Annual Regional Species Landings...27",
                  "Maximum Percent Total Annual Regional Species Revenue...28") %>%
    dplyr::rename("NEFMC, MAFMC, and ASMFC Managed Species" = "NEFMC and MAFMC Managed Species",
                  "perc_landings_max" = "Maximum Percent Total Annual Regional Species Landings...27",
                  "perc_revenue_max" = "Maximum Percent Total Annual Regional Species Revenue...28") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(perc_landings_max = as.numeric(perc_landings_max)*100,
                  perc_revenue_max = as.numeric(perc_revenue_max)*100,
                  Units = c("Percent")) %>%
    dplyr::mutate(across(where(is.numeric), ~round(., 2))) %>%
    dplyr::mutate(Council = "Unknown") %>%
    dplyr::distinct()

   # Add council data to dataset
    mafmc <- list("Chub Mackerel", "Bluefish", "Butterfish", "Scup", "Longfin Squid",
                  "Black Sea Bass", "Atlantic Mackerel", "Ocean Quahog",
                  "Summer Flounder", "Atlantic Surfclam", "Illex Squid",
                  "Golden Tilefish", "Blueline Tilefish")

    nefmc <- list("Little Skate", "Barndoor Skate", "Yellowtail Flounder", "Winter Skate",
                  "Clearnose Skate", "Red Hake", "Smooth Skate", "Winter Flounder",
                  "Windowpane Flounder", "Atlantic Cod", "Atlantic Sea Scallop",
                  "Offshore Hake", "Atlantic Herring", "Atlantic Halibut", "Witch Flounder",
                  "Haddock", "Thorny Skate", "American Plaice", "Redfish", "Pollock", "White Hake")

    both_councils <- list("Monkfish", "Spiny Dogfish")

    for (i in 1:length(mafmc)){
      wea_landings_rev <- wea_landings_rev %>%
        dplyr::mutate(Council = replace(Council, stringr::str_detect(wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species`, paste0(mafmc[i])), "MAFMC"))
    }

    for (i in 1:length(nefmc)){
      wea_landings_rev <- wea_landings_rev %>%
        dplyr::mutate(Council = replace(Council, stringr::str_detect(wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species`, paste0(nefmc[i])), "NEFMC"))
    }

    for (i in 1:length(both_councils)){
      wea_landings_rev <- wea_landings_rev %>%
        dplyr::mutate(Council = replace(Council, stringr::str_detect(wea_landings_rev$`NEFMC, MAFMC, and ASMFC Managed Species`, paste0(both_councils[i])), "MAFMC/NEFMC"))
    }

  if (save_clean){
    usethis::use_data(wea_landings_rev, overwrite = T)
  } else {
    return(wea_landings_rev)
  }
}
get_wea_landings_rev(save_clean = T)
