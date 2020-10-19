# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-doc/fishery-reliance-and-social-vulnerability.html

library(tidyverse)
library(readxl)


raw.dir <- here::here("data-raw")

engagement_xlsx<-"ComEng Scores 2004-2018 for SOE 2020 Report 021020.xlsx"
get_eng_rel <- function(save_clean = F){

  d <-read_excel(file.path(raw.dir, engagement_xlsx))

  #Process
  engagement <- d %>%
    dplyr::rename(EPU = Region,
                  "%med.high.scores" = "Average Scores for Medium High Communities  (n=49)") %>%
    tidyr::pivot_longer(cols = starts_with("%"), names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = recode(Var, "%med.high.scores" = "med.high.scores")) %>%
    dplyr::select(EPU, Time, Var, Value) %>%
    dplyr::mutate(Units = "unitless")

  if (save_clean){
    usethis::use_data(engagement, overwrite = T)
  } else {
    return(engagement)
  }
  # metadata ----
  attr(engagement, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/community-engagement.html"
  attr(engagement, "data_files")   <- list(
    engagement_xlsx = engagement_xlsx)
  attr(engagement, "data_steward") <- c(
    "Lisa Colburn <lisa.colburn@noaa.gov>")
}
get_eng_rel(save_clean = T)



