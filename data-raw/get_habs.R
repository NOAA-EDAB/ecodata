### HABs - New England

habs1_csv<-
  "Figure1_Gulf_of_Maine_Annual_Cyst_Abundance_Alexandrium_catenella - David Nelson - NOAA Federal.csv"
#psp_me_csv<- "Data_for_Figure3_PSP_BlueMussels_ME_2005-2019_cleaned - David Nelson - NOAA Federal.csv"
psp_me_csv<- "Data_for_Figures4and5_annual_mussel_psp_samples_Maine_2005_2019 - David Nelson - NOAA Federal.csv"
psp_nh_csv <- "Data_for_Figure7and8_Annual_Mytilis_PSP_samples_NH_2000_2019 - David Nelson - NOAA Federal.csv"
psp_ma_csv <- "Data_for_Figure10and11_Annual_Mytilis_Samples_MA_1972-2019_Mabrouk - David Nelson - NOAA Federal (1).csv"

get_habs <- function(save_clean = F){

  # Alexandrium
  dat <- read.csv(file.path(raw.dir,habs1_csv), header=TRUE, stringsAsFactors=FALSE)

  habs1 <- dat %>%
    pivot_longer(cols= c( "West_Gulf_of_Maine","East_Gulf_of_Maine",
                          "Bay_of_Fundy","Gulf_of_Maine_All" ),
                 names_to = "Var", values_to = "Value") %>%
    mutate(EPU = c("GOM"),
           Source = c("Alexandrium")) %>%
    dplyr::rename(Time = Year)

  # PSP
  me_psp <- read.csv(file.path(raw.dir,psp_me_csv), header=TRUE, stringsAsFactors=FALSE)

  me_psp2<- me_psp %>%
    dplyr::select(Year, PSP_Exceed_Threshold_.) %>%
    dplyr::rename(Time = Year,
                  Value = PSP_Exceed_Threshold_.) %>%
    dplyr::mutate(Var = c("ME"))

  nh_psp <- read.csv(file.path(raw.dir,psp_nh_csv), header=TRUE, stringsAsFactors=FALSE)
  nh_psp2<- nh_psp %>%
    dplyr::select(Year, PSP.Exceed.Threshold..) %>%
    dplyr::rename(Time = Year,
                  Value = PSP.Exceed.Threshold..) %>%
    dplyr::mutate(Var = c("NH"))

  ma_psp <- read.csv(file.path(raw.dir,psp_ma_csv), header=TRUE, stringsAsFactors=FALSE)
  ma_psp2<- ma_psp %>%
    dplyr::select(Year, PSP.) %>%
    dplyr::rename(Time = Year,
                  Value = PSP.) %>%
    dplyr::mutate(Var = c("MA"))


  psp<- me_psp2 %>% rbind(nh_psp2, ma_psp2) %>%
    dplyr::mutate(EPU = c("NE"),
                  Source = c("PSP"))

  habs<- habs1 %>% rbind(psp)



  # metadata ----
  attr(habs, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc.html"
  attr(habs, "data_files")   <- list(
    habs1_csv = habs1_csv,
    psp_me_csv=psp_me_csv,
    psp_nh_csv=psp_nh_csv,
    psp_ma_csv=psp_ma_csv )
  attr(habs, "data_steward") <- c(
    "Moe Nelson <david.nelson@noaa.gov>")
  attr(habs, "plot_script") <- list()

  if (save_clean){
    usethis::use_data(habs, overwrite = TRUE)
  } else {
    return(habs)
  }
}
get_habs(save_clean = T)


