# Commercial and recreational engagement and reliance

#More information about these data are available at https://noaa-edab.github.io/tech-doc/fishery-reliance-and-social-vulnerability.html

library(tidyverse)


raw.dir <- here::here("data-raw")

get_eng_rel <- function(save_clean = F){

  ne <- read.csv(file.path(raw.dir,"ComEng_NE.csv")) %>%
    mutate(EPU = c("GOM"))
  mab <- read.csv(file.path(raw.dir,"ComEng_MA.csv")) %>%
    mutate(EPU = c("MAB"))

  #Process
  eng_rel <- rbind(ne,mab) %>%
    gather(key = "Time", value = "Value", X2004:X2018) %>%
    rename(Var = Community) %>%
    mutate(Units = c("unitless"),
           Time = as.integer(gsub('[a-zA-Z]', '', eng_rel$Time))) %>%
    dplyr::select(-c(X1.std))


  if (save_clean){
    usethis::use_data(eng_rel, overwrite = T)
  } else {
    return(eng_rel)
  }

}
get_eng_rel(save_clean = T)



