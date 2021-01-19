# Process Gray Seal bycatch estimates

# Time series figure is 5-yr running mean for Gray Sealbycatch estimates
# for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
grayseal_csv<-"1996-2019_5yr_gs_est_wPBR.xlsx"
#HP bycatch time series estimates------------------------------------------------------
get_grayseal <- function(save_clean = F){
  d <- readxl::read_excel(file.path(raw.dir,grayseal_csv)) %>%
    dplyr::mutate(Region = "All") %>%
    dplyr::rename(EST5 = `5-YR EST`,
                  EST1 = `NE 1-YR EST`) %>%
    dplyr::select(-...8,
                  -LCI_DIFF,
                  -UCI_DIFF,
                  -...11,
                  -...12) #%>%
    #dplyr::group_by(Year, Region) %>%
    #tidyr::pivot_wider(names_from = Var, values_from = Value)

  #Create confidence intervals
  # var1nnum <- log(1+d$CV^2)
  # c <- exp(1.96 * sqrt(var1nnum))
  # d$up95ci <- d$EST * c
  # d$lo95ci <- d$EST / c


  grayseal <- d %>%
    tidyr::pivot_longer(cols = c("EST5", "EST1","CV", "PBR", "LCI", "UCI"), names_to = "Var", values_to = "Value") %>%
    mutate(Units = "N") #%>%
    #as.data.frame()

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
    "Chris Orphanides <chris.orphanides@noaa.gov>")
}
get_grayseal(save_clean = T)
