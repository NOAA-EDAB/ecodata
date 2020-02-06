#### Chesapeake bay salinity

library(tidyverse)
library(readr)

raw.dir <- here::here("data-raw")


get_ch_bay_sal <- function(save_clean = F){

       ch_bay_sal<-read_csv(file.path(raw.dir,"SR_Salinity.csv"), col_names = FALSE) %>%
         dplyr::mutate(Columns = c("UTCTime", "MinDataLim", "MaxDataLim", "AvgMinLim",
                            "AvgMaxLim","Daily19", "Daily18")) %>%
         tidyr::gather(Day, Value, -Columns) %>%
         dplyr::rename(Var = Columns,
                       Time = Day) %>%
         dplyr::mutate(EPU = c("MAB"),
                       Units = c("degree C"))


if (save_clean){
  usethis::use_data(ch_bay_sal, overwrite = T)
} else {
  return(ch_bay_sal)
}
}
get_ch_bay_sal(save_clean = T)
