#Processing for North Atlantic Right Whale data

#See full documentation for these data at https://noaa-edab.github.io/tech-memo/right-whale-abundance.html

library(dplyr)
library(tidyr)

raw.dir <- here::here("inst","extdata")

get_narw <- function(save_clean = F){
  narw <- read.csv(file.path(raw.dir, "narw_numbers.csv")) %>% 
    dplyr::select(-Parm) %>%
    gather(.,Var,Value,-Year) %>% 
    mutate(Var = tolower(paste("right whale abundance",Var)),
           Units =  "n",
           EPU = "All")
  
  if (save_clean){
    usethis::use_data(narw, overwrite = T)
  } else {
    return(narw)
  }
  
}
