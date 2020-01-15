library(tidyverse)


raw.dir <- here::here("data-raw")

get_zoo_abun_anom <- function(save_clean = F){

  load(file.path(raw.dir, "zoo_abun_anom.rdata"))

  zoo_abun_anom <- as.tibble(mtest) %>%
    dplyr::rename(EPU = Region, Value = value, Var = variable) %>%
    mutate(Var = plyr::mapvalues(Var, from = c("Pse", "Cty", "Cha", "Tlo", "Cf"),
                                 to = c("Pseuodocalanus spp.", "Centropages typicus", "Centropages hamatus",
                                        "Temora longicornis", "Calanus finmarchicus")))


  if (save_clean){
    usethis::use_data(zoo_abun_anom, overwrite = T)
  } else {
    return(zoo_abun_anom)
  }
}
get_zoo_abun_anom(save_clean = T)
