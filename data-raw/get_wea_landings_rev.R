## Landings and Revenue in wind lease areas

library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data-raw")

## Doug Christel
#wind_xlsx<-"CHRISTEL_2022 State of the Ecosystem Report_Max WEA Species Landings and Revenue.xlsx"
wind_rev_xlsx<-"Fishery Impacts from OSW Development_2023 SOE Report_Christel_UPDATED.xlsx"
get_wea_landings_rev <- function(save_clean = F){
  # import data
   wea_landings_rev<-readxl::read_excel(file.path(raw.dir,wind_rev_xlsx),
                                        sheet = "Cumulative Max Rev and Land") %>%
    # janitor::row_to_names(.,1) %>%
    #  select("Max % Regional Revenue and Landings of fisheries managed by the Atlantic States Marine Fisheries Commission and the New England and Mid-Atlantic Fishery Management Council within Existing Lease Areas and the Draft Primary and Secondary Central Atlantic Call Areas")

    dplyr::select( "NEFMC and MAFMC Managed Species",
                   "Maximum Percent Total Annual Regional Species Landings...9",
                   "Maximum Percent Total Annual Regional Species Revenue...8") %>%
    dplyr::rename("NEFMC, MAFMC, and ASMFC Managed Species" = "NEFMC and MAFMC Managed Species",
                  "perc_landings_max" = "Maximum Percent Total Annual Regional Species Landings...9",
                  "perc_revenue_max" = "Maximum Percent Total Annual Regional Species Revenue...8") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(perc_landings_max = as.numeric(perc_landings_max)*100,
                  perc_revenue_max = as.numeric(perc_revenue_max)*100,
                  Units = c("Percent"))

  # metadata ----
  attr(wea_landings_rev, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/fisheries-revenue-in-wind-development-areas.html"
  attr(wea_landings_rev, "data_files")   <- list(
    wind_rev_xlsx = wind_rev_xlsx)
  attr(wea_landings_rev, "data_steward") <- c(
    "Doug Christel <douglas.christel@noaa.gov>")
  attr(wea_landings_rev, "plot_script") <- list(
    `hd_MAB_wea_rev` = "human_dimensions_MAB.Rmd-wea-landings-rev.R",
    `hd_MAB_wea_spp_rev` = "human_dimensions_MAB.Rmd-wea-spp-rev.R")


  if (save_clean){
    usethis::use_data(wea_landings_rev, overwrite = T)
  } else {
    return(wea_landings_rev)
  }
}
get_wea_landings_rev(save_clean = T)





