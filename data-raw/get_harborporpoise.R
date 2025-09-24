# Process harbor porpoise bycatch estimates

# Time series figure is 5-yr running mean for harbor porpoise bycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
harborporpoise_csv<-"historical_harbp_est-20250812 - Kristin Precoda - NOAA Affiliate.csv"


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

}
get_harborporpoise(save_clean = T)
