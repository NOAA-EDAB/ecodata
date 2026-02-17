# Processing for Gulf Stream Index data

# GSI = degrees latitude above the average Gulf Stream position based
# on ocean temperature at 200m (15 C) depth between 55W to 75W.

library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

raw.dir <- here::here("data-raw")
gsi_xlsx <- "CHEN_T200-based_GSI_EN4_195401-202509_monthly - Zhuomin Chen.csv"
get_gsi <- function(save_clean = F) {
  gsi <- read.csv(file.path(raw.dir, gsi_xlsx)) %>%
    dplyr::rename(
      Time = year.month,
      "gulf stream index" = GSI,
      "western gulf stream index" = WGSI
    ) %>%
    tidyr::pivot_longer(
      c("gulf stream index", "western gulf stream index"),
      names_to = "Var",
      values_to = "Value"
    ) %>%
    dplyr::mutate(EPU = c("All"))

  if (save_clean) {
    usethis::use_data(gsi, overwrite = T)
  } else {
    return(gsi)
  }
}
get_gsi(save_clean = T)
