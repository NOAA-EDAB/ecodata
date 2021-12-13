### HABs - New England

habs1_csv<-
  "Figure1_Gulf_of_Maine_Annual_Cyst_Abundance_Alexandrium_catenella - David Nelson - NOAA Federal.csv"
habs2_csv<- "Data_for_Figure3_PSP_BlueMussels_ME_2005-2019_cleaned - David Nelson - NOAA Federal.csv"
get_habs <- function(save_clean = F){

  # Alexandrium
  dat1 <- read.csv(file.path(raw.dir,habs1_csv), header=TRUE, stringsAsFactors=FALSE)

  habs1 <- dat %>%
    pivot_longer(cols= c( "West_Gulf_of_Maine","East_Gulf_of_Maine",
                          "Bay_of_Fundy","Gulf_of_Maine_All" ),
                 names_to = "Var", values_to = "Value") %>%
    mutate(EPU = c("GOM"),
           Source = c("Alexadrium")) %>%
    dplyr::rename(Time = Year)

  # PSP
  dat2 <- read.csv(file.path(raw.dir,habs2_csv), header=TRUE, stringsAsFactors=FALSE)

  habs2<- dat2 %>% dplyr::filter(Over_44_threshold == "Yes") %>%
    group_by(Year) %>%
    summarise(Value = sum(length(Over_44_threshold))) %>%
    ungroup() %>%
    dplyr::mutate(Var = c("PSP Toxicity in Blue Mussels"),
                  EPU = c("GOM"),
                  Source = c("PSP")) %>%
    dplyr::rename(Time = Year)

  habs<- habs1 %>% rbind(habs2)
  # metadata ----
  attr(habs, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/harbor-porpoise-and-gray-seal-bycatch.html"
  attr(habs, "data_files")   <- list(
    habs_csv = habs_csv)
  attr(habs, "data_steward") <- c(
    "Chris Orphanides <chris.orphanides@noaa.gov>")
  attr(habs, "plot_script") <- list(
    `mf_MAB` = "macrofauna_MAB.Rmd-grayseal.R",
    `mf_MAB_mab` = "macrofauna_MAB.Rmd-grayseal-mab.R")

  if (save_clean){
    usethis::use_data(habs, overwrite = TRUE)
  } else {
    return(habs)
  }
}
get_habs(save_clean = T)


