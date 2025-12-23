library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

raw.dir <- here::here("data-raw")
wbts_zoo_xlsx <- "WBTS_CI - Cameron Thompson.xlsx"
### Zooplankton Diversity
get_wbts_zoo <- function(save_clean = F){

  data <- read_excel(file.path(raw.dir,wbts_zoo_xlsx)) %>%
    dplyr::rename(DATE = 'DATE...4') |>
    dplyr::filter(is.na(Flag) | Flag != 1) %>%
    dplyr::mutate(
      CI = `Calanus Index Sum CIII-CVI`,
      CSI = CSI,
      DAY = as.Date(DATE, origin = "1899-12-30"),
      day_of_year = as.numeric(format(DAY, "%j")),
      year = as.numeric(format(DAY, '%Y')),
      year_period = cut(
        year,
        breaks = c(2003, 2010, 2024, 2025.1),
        labels = c("2004 - 2010", "2011 - 2024", "2025"),
        right = TRUE
      )
    )

  #### CI Model and Plot ####
  ci_model_data <- data %>%
    dplyr::filter(!is.na(CI), CI != -Inf, DAY <= as.Date("2024-03-14")) %>%
    dplyr::mutate(CI = sqrt(CI))

  ci_data <- data %>%
    dplyr::filter(!is.na(CI), CI != -Inf) %>%
    dplyr::mutate(CI = sqrt(CI))

  mod_ci <- mgcv::gam(CI ~ s(day_of_year, bs = "cc") + s(year),
                data = ci_model_data,
                correlation = corAR1(form = ~ 1 | year),
                method = "REML")

  pred_ci <- expand.grid(day_of_year = 1:365, year = 2012)
  preds_ci <- predict(mod_ci, newdata = pred_ci, type = "response", se.fit = TRUE)
  pred_ci <- cbind(pred_ci, pred = preds_ci$fit,
                   pred_low = preds_ci$fit - 1.96 * preds_ci$se.fit,
                   pred_high = preds_ci$fit + 1.96 * preds_ci$se.fit)

  wbts_zoo = list(
    data = ci_data |>
      dplyr::select(year, day_of_year, CI) |>
      tidyr::pivot_longer(cols = c(CI), names_to = "Var", values_to = "Value") |>
      tidyr::unite(Time, year, day_of_year, sep = ".") |>
      dplyr::mutate(Var = 'Calinus finmarchicus: Stage I Abundance',
                    Units = 'Abundance'),
    model = pred_ci |>
      dplyr::select(-year) |>
      tidyr::pivot_longer(cols = c(pred, pred_low, pred_high), names_to = "Var", values_to = "Value")
  )


  if (save_clean){
    usethis::use_data(wbts_zoo, overwrite = T)
  } else {
    return(wbts_zoo)
  }
  # metadata ----
  attr(zoo_diversity, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/wbts_zoo.html"
  attr(zoo_diversity, "data_files")   <- list(
    zoo_diversity_xlsx = zoo_diversity_xlsx)
  attr(zoo_diversity, "data_steward") <- "Cameeron Thompson <Cameron@neracoos.org>"
}
get_wbts_zoo(save_clean = T)



