library(dplyr)
library(tidyr)

raw.dir <- here::here("data-raw")
crew_survey_xlsx <- "2012_2024_Crew Survey SOE Dataset_01052026.xlsx"

get_crew_survey <- function(save_clean = F){

  crew_survey_orig <- readxl::read_xlsx(path = file.path(raw.dir, crew_survey_xlsx))

  used.vars = c('Survey wave',
                'Primary port region',
                'Education Combined',
                'Primary Fishery Combined',
                'Age (Categorical) Combined',
                '[Your actual earnings] Job satisfaction Combined',
                '[Predictability of earnings] Job satisfaction Combined',
                '[Job safety] Job satisfaction Combined',
                '[Time away] Job satisfaction Combined',
                '[Physical fatigue] Job satisfaction Combined',
                '[Healthfulness] Job satisfaction Combined',
                '[Adventure] Job satisfaction Combined',
                '[Challenge] Job satisfaction Combined',
                '[Own boss] Job satisfaction Combined',
                'Years in commercial fishing (0 if less than a year) Combined'
                )

  crew_survey = crew_survey_orig |>
    dplyr::mutate(`Subject number` = as.character(`Subject number`),
                  ID = dplyr::coalesce(`Subject number`, `Case ID...2`,`Case ID...3`)) |>
    dplyr::select(ID,used.vars) |>
    tidyr::separate('Survey wave',c('Time','Wave'),sep = "-", convert = T) |>
    #Unite both 'Case ID' columns
    dplyr::mutate(EPU = dplyr::case_when(
                    `Primary port region` == "New England" ~ "NE",
                    `Primary port region` == "Mid-Atlantic" ~ "MA",
                    `Primary port region` == "Multiple ports" ~ "All",
                    `Primary port region` == "Southeast/GOA" ~ NA,
                    `Primary port region` == "No answer" ~ NA
                  )) |>
    tidyr::pivot_longer(cols = -c(Time,EPU),
                        names_to = "Var",
                        values_to = "Value",values_transform = as.character)


  if (save_clean){
    usethis::use_data(crew_survey, overwrite = T)
  } else {
    return(crew_survey)
  }
}
get_crew_survey(save_clean = T)
