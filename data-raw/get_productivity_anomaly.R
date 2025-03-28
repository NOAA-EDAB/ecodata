#processing small fish per large fish biomass indicator
# data processing comes from trawlR package.

library(dplyr)
library(tidyr)
library(ggplot2)

productivity_anomaly_rdata <- "dat_spec_rec_forSOE.Rdata"
productivity_anomaly__epu_rdata <- "dat_spec_rec_epu_forSOE.Rdata"
prod_anom_assessment_rds<- "AssessFishProdAnomaly - Sarah Gaichas - NOAA Federal.rds"

raw.dir <- here::here("data-raw")

load(file.path(raw.dir,productivity_anomaly_rdata))
load(file.path(raw.dir,productivity_anomaly__epu_rdata))

#Select and rename
epu_rec_anom <- dat_spec_rec_epu_forSOE %>%
  dplyr::select(Time, EPU = Region, Value, Units, -Source,Var) %>%
  dplyr::filter(!Time == "2020")

#Select, rename, and bind
productivity_anomaly1 <- dat_spec_rec_forSOE %>%
  dplyr::select(-Source) %>%
  dplyr::mutate(EPU = "All",
         Var = paste("NE LME",Var)) %>%
  rbind(.,epu_rec_anom) %>%
  as.data.frame()%>%
  tibble::as_tibble() %>%
  dplyr::select(Time, Var, Value, EPU, Units) %>%
  dplyr::mutate(Var = paste0(Var, "_Survey"))


prod_assess<- readRDS(file.path(raw.dir,prod_anom_assessment_rds))
prod_assess1<- prod_assess %>%
  tidyr::separate(StockName, into= c("Stock", "Region"), sep = "-") %>%
  dplyr::mutate(EPU = dplyr::recode(Region, " Gulf of Maine / Georges Bank" = "NE",
                                    " Gulf of Maine / Cape Hatteras" = "ALL",
                                    " Mid" = "MA",
                                    " Atlantic Coast" = "ALL",
                                    " Georges Bank" = "NE",
                                    " Northwestern Atlantic Coast" = "ALL",
                                    " Gulf of Maine" = "NE",
                                    " Southern New England / Mid" = "MA",
                                    " Cape Cod / Gulf of Maine" = "NE")) %>%
  tidyr::pivot_longer(cols = c("spawners_biom_lag0", "spawners_biom_lag0_anom",
                               "recruits_abund_lead1","recruits_abund_lead1_anom",
                               "rs","rs_anom","logr_abund_anom",
                               "logs_biom_anom","logrs_anom"),
                      names_to = "Var", values_to = "Value") %>%
  dplyr::mutate(Var = paste0(Stock, "-", Var, "-Assessment"),
                Time = YEAR,
                Units = c("NA")) %>%
  dplyr::ungroup() %>%
  dplyr::select(Time, Var, Value, EPU, Units)

productivity_anomaly<- rbind(productivity_anomaly1, prod_assess1)


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




