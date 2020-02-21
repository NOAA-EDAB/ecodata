
d<-ecodata::energy_density

colNames <- c("Species",rep(c("Mean ED (SD)","N"),5),"Mean ED", "Mean ED (SD)")

kable(d, caption = "Forage fish energy content",col.names = colNames) %>%
  kable_styling(bootstrap_options = "striped", font_size = 9) %>%
  add_header_above(c(" " = 1, "Spring" = 2, "Fall" = 2, "Spring" = 2, "Fall" = 2, " " = 4)) %>% 
  add_header_above(c(" " = 1, "2017" = 4, "2018" = 4, "Total" = 2, "Steimle and Terranove (1985)" = 1, "Lawson et al. (1998)" = 1))
