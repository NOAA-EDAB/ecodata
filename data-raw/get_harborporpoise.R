# Process harbor porpoise bycatch estimates

# Time series figure is 5-yr running mean for harbor porpoise bycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
harborporpoise_csv<-"Precoda-historical_harbp_est-20241114 - Kristin Precoda - NOAA Affiliate.csv"


get_harborporpoise<- function(save_clean = F){
  harborporpoise<- read.csv(here::here(file.path(raw.dir, harborporpoise_csv))) %>%
    dplyr::select(year, pbr, totalest1y, totalest5y, total5yLCI, total5yUCI) %>%
    dplyr::rename("Time" = year) %>%
    tidyr::pivot_longer(cols = c(pbr, totalest1y, totalest5y, total5yLCI, total5yUCI ),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = c("All"))

  if (save_clean){
     usethis::use_data(harborporpoise, overwrite = TRUE)
   } else {
     return(harborporpoise)
   }
   # metadata ----
   attr(harborporpoise, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
   attr(harborporpoise, "data_files")   <- list(
    harborporpoise_csv = harborporpoise_csv)
   attr(harborporpoise, "data_steward") <- c(
    "Debra Palka <debra.palka@noaa.gov>")
}
get_harborporpoise(save_clean = T)

