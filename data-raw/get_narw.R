<<<<<<< HEAD
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
    rename(Lower95 = Lower95...2,
           Median = Median...3,
           Upper95 = Upper95...4,
           Time = Year) %>%
    gather(.,"Var","Value",-Time) %>%
    mutate(Units =  "n",
           EPU = "All")

  if (save_clean){
    usethis::use_data(narw, overwrite = T)
  } else {
    return(narw)
  }

}
get_narw(save_clean = T)
=======
#Processing for North Atlantic Right Whale data

#See full documentation for these data at 
# https://noaa-edab.github.io/tech-doc/right-whale-abundance.html

library(dplyr)
library(tidyr)
library(stringr)

raw.dir <- here::here("data-raw")

get_narw <- function(save_clean = F){
  narw <- read.csv(file.path(raw.dir, "narw_numbers.csv")) %>% 
    gather(.,Var,Value,-YEAR) %>% 
    mutate(Var = tolower(paste("right whale abundance",Var)),
           Units =  "n",
           EPU = "All") %>% 
    mutate(Var = ifelse(str_detect(Var, "median"),
                        "right whale abundance median",
                        ifelse(str_detect(Var, "lcl"),
                               "right whale abundance lcl",
                               "right whale abundance ucl"))) %>% 
    dplyr::rename(Time = YEAR)
  
  if (save_clean){
    usethis::use_data(narw, overwrite = T)
  } else {
    return(narw)
  }
  
}
>>>>>>> 22d4e3f50bcec14c6ce42ecadd0f63f02ba6a098
