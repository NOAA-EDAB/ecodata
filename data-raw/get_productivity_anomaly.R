#processing small fish per large fish biomass indicator
# data processing comes from trawlR package.

library(dplyr)
library(tidyr)
library(ggplot2)

productivity_anomaly_rdata <- "dat_spec_rec_forSOE.Rdata"
productivity_anomaly__epu_rdata <- "dat_spec_rec_epu_forSOE.Rdata"
raw.dir <- here::here("data-raw")

load(file.path(raw.dir,productivity_anomaly_rdata))
load(file.path(raw.dir,productivity_anomaly__epu_rdata))

#Select and rename
epu_rec_anom <- dat_spec_rec_epu_forSOE %>%
  dplyr::select(Time, EPU = Region, Value, Units, -Source,Var) %>%
  dplyr::filter(!Time == "2020")

#Select, rename, and bind
productivity_anomaly <- dat_spec_rec_forSOE %>%
  dplyr::select(-Source) %>%
  dplyr::mutate(EPU = "All",
         Var = paste("NE LME",Var)) %>%
  rbind(.,epu_rec_anom) %>%
  as.data.frame()

# metadata ----
attr(productivity_anomaly, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/fish-productivity-indicator.html"
attr(productivity_anomaly, "data_files")   <- list(
  productivity_anomaly_rdata = productivity_anomaly_rdata,
  productivity_anomaly__epu_rdata  =productivity_anomaly__epu_rdata )
attr(productivity_anomaly, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")
attr(productivity_anomaly, "plot_script") <- list(
  `mf_MAB` = "macrofauna_MAB.Rmd-productivity-anomaly.R",
  `mf_NE_gb` = "macrofauna_NE.Rmd-productivity-anomaly-gb.R",
  `mf_NE_gom` = "macrofauna_NE.Rmd-productivity-anomaly-gom.R")

usethis::use_data(productivity_anomaly, overwrite = TRUE)




