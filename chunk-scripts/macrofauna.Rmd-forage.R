
d<-ecodata::energy_density %>% 
    rename("Mean ED (SD)" = "Mean.ED..SD.", 
           "N" = "n",
           "Mean ED (SD)" = "Mean.ED..SD..1", 
           "N" = "n.1", 
           "Mean ED (SD)" = "Mean.ED..SD..2", 
           "N" = "n.2", 
           "Mean ED (SD)" = "Mean.ED..SD..3", 
           "N" = "n.3",
           "Mean ED (SD)" = "Mean.ED..SD..4", 
           "N" = "X",
           "Mean ED (SD)" = "Mean.ED..SD..5", 
           "Mean ED" = "Mean.ED" )
kable(d, caption = "Forage fish energy content") %>% 
  kable_styling(bootstrap_options = "striped", font_size = 9) %>% 
  add_header_above(c(" " = 1, "Spring" = 2, "Fall" = 2, "Spring" = 2, "Fall" = 2, " " = 4)) %>% 
  add_header_above(c(" " = 1, "2017" = 4, "2018" = 4, "Total" = 2, "Steimle and Terranove (1985)" = 1, "Lawson et al. (1998)" = 1))
