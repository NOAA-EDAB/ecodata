
#Processing for North Atlantic Right Whale data

#See full documentation for these data at https://noaa-edab.github.io/tech-doc/right-whale-abundance.html

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

raw.dir <- here::here("data-raw")

get_narw <- function(save_clean = F){
  narw <- read_excel(file.path(raw.dir,"narw_abundance.xlsx")) %>%
    dplyr::select(-c(5,7:10)) %>%
    dplyr::rename(Lower95 = Lower95...2,
           Median = Median...3,
           Upper95 = Upper95...4,
           Time = Year) %>%
    tidyr::pivot_longer(-Time, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Units =  "n",
           EPU = "All")

  calves19<-data.frame(Time = c(2019), Var = c("Calves"),
                       Value = c(7), Units = c("n"), EPU = c("All")) #add 7 calves from 2019

  narw <- rbind(narw, calves19) #bind with rest of data

  if (save_clean){
    usethis::use_data(narw, overwrite = T)
  } else {
    return(narw)
  }

}
get_narw(save_clean = T)


