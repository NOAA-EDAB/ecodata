### SAV
library(readxl)

raw.dir <- here::here("data-raw")
SAV_xlsx<- "Wilcox - SAV Table for SOE Report - David Wilcox.xlsx"
get_SAV <- function(save_clean = F){

  # Alexandrium
  SAV <- read_excel(file.path(raw.dir,SAV_xlsx)) %>%
    janitor::row_to_names(1) %>%
    tidyr::pivot_longer(cols = (c("Tidal Fresh Total", "Oligohaline Total", "Mesohaline Total",
                                  "Polyhaline Total",  "Baywide Total"  )),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(EPU = c("MAB"),
                  Year =  as.numeric(Year),
                  Value = as.numeric(Value)) %>%
    dplyr::rename(Time = Year) %>%
    tidyr::separate(Var, into = c("Var", "Null")) %>%
    dplyr::select(-Null)


  # metadata ----
  attr(SAV, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/submerged-aquatic-vegetation.html"
  attr(SAV, "data_files")   <- list(
    SAV_xlsx  = SAV_xlsx)
  attr(SAV, "data_steward") <- c(
    "David Wilcox <dwilcox@vims.edu>")

  if (save_clean){
    usethis::use_data(SAV, overwrite = TRUE)
  } else {
    return(SAV)
  }
}
get_SAV(save_clean = T)


