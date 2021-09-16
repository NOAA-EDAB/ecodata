## Get tac starter script
## One species, one area from Applegate

library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
tac_csv <- "TAC_Applegate.csv"

whitingcsv <- read.csv(file.path(raw.dir,tac_csv), header=TRUE, stringsAsFactors=FALSE)

whiting_tac<- whitingcsv %>%
  dplyr::mutate(prop_ABC_Landings = (Federal.landings..mt../ABC..mt..)*100,
                prop_TAL_Landings = (Federal.landings..mt../TAL..mt..)*100) %>%
  dplyr::rename(Time = Row.Labels,
                OFL = OFL..mt..,
                ABC = ABC..mt..,
                ACL = ACL..mt..,
                TAL = TAL..mt..,
                Fed_Landings = Federal.landings..mt..,
                Est_bycatch = Estimated.discards..mt..) %>%
  tidyr::pivot_longer(!Time, names_to = "Var", values_to = "Value", values_drop_na = TRUE) %>%
  dplyr::mutate(Units = "metric tons",
                EPU = "ALL")

## Managament Stability
whiting_tac %>%
  filter(!Var == c("prop_ABC_Landings" , "prop_TAL_Landings")) %>%
  ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value))+
  #geom_line(aes(y = Time, x = mean(Value)), linetype = "dashed")+
  facet_wrap(.~Var, scales = "free")+
  ylab("metric tons")+
  ggtitle("Southern Whiting")+
  ecodata::theme_ts()


whiting_tac %>%
  filter(Var == c("prop_ABC_Landings" , "prop_TAL_Landings")) %>%
  group_by(Var) %>%
  mutate(hline = mean(Value)) %>%
  ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value))+
  facet_wrap(.~Var, scales = "free")+
  geom_hline(aes(yintercept = hline), linetype = "dashed")+
  ylab("Percent")+
  ggtitle("Southern Whiting")+
  ecodata::theme_ts()
