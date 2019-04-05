# Process harbor porpoise bycatch estimates

# Time series figure is 5-yr running mean for harbor porpoise bycatch estimates for the Northeast US across all fisheries.

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")

#HP bycatch time series estimates------------------------------------------------------
get_harborporpoise <- function(save_clean = F){
  d <- read.csv(file.path(raw.dir,"1994-2017_5yr_hp_est.csv"))
  
  #Create confidence intervals
  var1nnum <- log(1+d$CV^2)  
  c <- exp(1.96 * sqrt(var1nnum))
  d$up95ci <- d$EST * c
  d$lo95ci <- d$EST / c
  
  
  harborporpoise <- d %>% dplyr::rename(Time = YEAR) %>% 
    gather(., Var, Value, -Time) %>% 
    mutate(Units = "N",
           EPU = "All",
           Var, Var = plyr::mapvalues(Var, from = c("EST","CV","PBR","up95ci","lo95ci"),
                                      to = c("harbor porpoise bycatch estimate",
                                             "harbor porpoise bycatch cv",
                                             "harbor porpoise bycatch pbr",
                                             "harbor porpoise bycatch up95ci",
                                             "harbor porpoise bycatch lo95ci"))) %>% 
    as.data.frame()
  
  if (save_clean){
    usethis::use_data(harborporpoise, overwrite = R)
  } else {
    return(harborporpoise)
  }
}
