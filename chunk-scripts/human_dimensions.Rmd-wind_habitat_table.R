
wind1 <- ecodata::wind_occupancy

wind1$trend<- ifelse(wind1$Trend == "pos", 
                    '![](arrow_up.png)',
                    ifelse(wind1$Trend == "neg",
                    '![](arrow_down.png)', 
                    '![](arrow_null.png)')) 


wind2<-wind1 %>% dplyr::select(Area, Season, Species, trend)
names<-c("Area", "Season", "Species", "trend")
bnew<-c("Area.1", "Season.1", "Species.1", "trend.1")
cnew<-c("Area.2", "Season.2", "Species.2", "trend.2")
dnew<-c("Area.3", "Season.3", "Species.3", "trend.3")
enew<-c("Area.4", "Season.4", "Species.4", "trend.4")

a<-wind2 %>% filter(Area == "Existing-North") 
b<-wind2 %>% filter(Area == "Proposed-North") %>% 
  dplyr::rename_at(vars(names), ~ bnew)
c<-wind2 %>% filter(Area == "Existing-Mid")%>% 
  dplyr::rename_at(vars(names), ~ cnew)
d<-wind2 %>% filter(Area == "Proposed-Mid")%>% 
  dplyr::rename_at(vars(names), ~ dnew)
e<-wind2 %>% filter(Area == "Existing-South")%>% 
  dplyr::rename_at(vars(names), ~ enew)

all<- a %>% cbind(b,c,d,e) %>% 
  dplyr::select(2:4,7:8,11:12,15:16,19:20) %>% 
  rename(Trend = trend, 
         Species = Species.1, 
         Trend = trend.1, 
         Species = Species.2, 
         Trend = trend.2, 
         Species = Species.3, 
         Trend = trend.3, 
         Species = Species.4, 
         Trend = trend.4 )
  

all %>% kable(format = "html") %>%
  kable_styling("striped", "condensed", font_size = 9) %>%
  collapse_rows(1) %>% 
  add_header_above(c(" " = 1, "Existing - North" = 2, "Proposed - North" = 2, 
                     "Existing - Mid" = 2, "Proposed - Mid" = 2, 
                     "Existing - South" = 2)) 
