## Long Line Survey
## Black-Bellied Rose
library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")

rose_lls_csv<- "BLLS_RAW_2014-2019_tilefish_rosefish.csv"

get_long_line_survey<- function(save_clean = F){

  long_line_survey<-read_csv(file.path(raw.dir,rose_lls_csv)) %>%
    dplyr::mutate(categ = case_when(YEAR>2017 ~ "B",
                                    YEAR<=2017 ~"A"))

  # metadata ----


  if (save_clean){
    usethis::use_data(long_line_survey, overwrite = T)
  } else {
    return(long_line_survey)
  }
}
get_long_line_survey(save_clean = T)









