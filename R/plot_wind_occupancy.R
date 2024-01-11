#' plot probability of species occupancy in wind area
#'
#' plot wind_occupancy data. This is not region specific?
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return kable object
#'
#'
#' @export
#'

plot_wind_occupancy <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  wind1 <- ecodata::wind_occupancy
  wind1$trend<- ifelse(wind1$Trend == "pos",
                       "$\\nearrow$",
                       ifelse(wind1$Trend == "neg",
                              "$\\searrow$",
                              " "))
  wind2<-wind1 %>% dplyr::select(Area, Season, Species, trend)
  names<-c("Area", "Season", "Species", "trend")
  bnew<-c("Area.1", "Season.1", "Species.1", "trend.1")
  cnew<-c("Area.2", "Season.2", "Species.2", "trend.2")
  dnew<-c("Area.3", "Season.3", "Species.3", "trend.3")
  enew<-c("Area.4", "Season.4", "Species.4", "trend.4")
  a<-wind2 %>% dplyr::filter(Area == "Existing-North")
  b<-wind2 %>% dplyr::filter(Area == "Proposed-North") %>%
    dplyr::rename_at(dplyr::vars(names), ~ bnew)
  c<-wind2 %>% dplyr::filter(Area == "Existing-Mid")%>%
    dplyr::rename_at(dplyr::vars(names), ~ cnew)
  d<-wind2 %>% dplyr::filter(Area == "Proposed-Mid")%>%
    dplyr::rename_at(dplyr::vars(names), ~ dnew)
  e<-wind2 %>% dplyr::filter(Area == "Existing-South")%>%
    dplyr::rename_at(dplyr::vars(names), ~ enew)
  all<- a %>% cbind(b,c,d,e) %>%
    dplyr::select(2:4,7:8,11:12,15:16,19:20)

  tab <- kableExtra::kable(all, escape = FALSE,
                    col.names = c("Season", "Species", "Trend", "Species", "Trend", "Species","Trend", "Species","Trend", "Species", "Trend"),
                    caption = "Species with highest probability of occupancy species each season and area, with observed trends",
                    booktabs = T) %>%
    kableExtra::add_header_above(c(" " = 1, "Existing - North" = 2, "Proposed - North" = 2,
                                   "Existing - Mid" = 2, "Proposed - Mid" = 2,
                                   "Existing - South" = 2)) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

    return(tab)

}
