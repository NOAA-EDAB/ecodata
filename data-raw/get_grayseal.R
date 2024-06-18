# Process Gray Seal bycatch estimates

# Time series figure is 5-yr running mean for Gray Sealbycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
grayseal_csv<-"historical_gray_est-20230310.csv"


get_grayseal<- function(save_clean = F){
  grayseal<- read.csv(here::here(file.path(raw.dir, grayseal_csv))) %>%
    dplyr::select(year, pbr, totalest1y, totalest5y, total5yLCI, total5yUCI) %>%
    dplyr::rename("Time" = year) %>%
    tidyr::pivot_longer(cols = c(pbr, totalest1y, totalest5y, total5yLCI, total5yUCI ),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = c("All"))

  if (save_clean){
    usethis::use_data(grayseal, overwrite = TRUE)
  } else {
    return(grayseal)
  }
  # metadata ----
  attr(grayseal, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html"
  attr(grayseal, "data_files")   <- list(
    grayseal_csv = grayseal_csv)
  attr(grayseal, "data_steward") <- c(
    "Debra Palka <debra.palka@noaa.gov>")
}
get_grayseal(save_clean = T)
