library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
transdates_csv <- "trans_dates.csv"

get_transition_dates <- function(save_clean = F){

  trans_dates<- read.csv(file.path(raw.dir,transdates_csv)) %>%
    tidyr::pivot_longer( cols = c("sprtrans","falltrans","sumlen"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::filter(!EPU == "NA") %>%
    dplyr::rename(Time = Year) %>%
    dplyr::select(Time, Value, Var, EPU) %>%
    dplyr::mutate(Time = as.numeric(Time),
                  Value = as.numeric(Value)) %>%
    dplyr::group_by(Time, EPU, Var) %>%
    dplyr::summarise(Value = unique(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(EPU = recode(EPU,"GBK" = "GB"))

  if (save_clean){
    usethis::use_data(trans_dates, overwrite = T)
  } else {
    return(trans_dates)
  }
}
get_transition_dates(save_clean = T)

