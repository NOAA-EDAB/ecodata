#' table of community factor indicators
#'
#' returns community indicators with highest engagement or port activity
#'
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable sets to plot ("Social","Economic","Gentrification")
#' @param plottype Character string. Which Fishery to plot ("Commercial","Recreational")
#' @param n Numeric scalar. Number of rows to return (top in plottype category)
#'
#' @return dataframe
#'
#'
#' @export
#'

plot_community_factors <- function(
  report = "MidAtlantic",
  varName = "Social",
  plottype = 'Commercial',
  n = 10
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  if (plottype == 'Commercial') {
    filterVar = 'fishing_mean_score'
  } else if (plottype == 'Recreational') {
    filterVar = 'RecEng'
  } else {
    stop('plottype must be either "Commercial" or "Recreational"')
  }

  if (varName == "Social") {
    indgroup <- c(
      "personal_disruption_rank",
      "pop_composition_rank",
      "poverty_rank"
    )
  }
  if (varName == "Economic") {
    indgroup <- c("labor_force_str_rank", "housing_characteristics_rank")
  }
  if (varName == "Gentrification") {
    indgroup <- c(
      "housing_disrupt_rank",
      "retiree_migration_rank",
      "urban_sprawl_index_rank"
    )
  }

  eng <- ecodata::engagement |>
    #dplyr::distinct(Time, Var,  EPU, Units, .keep_all = T) |> #hack, remove later
    tidyr::separate(Var, into = c("Town", "StateVar"), sep = ", ") |> #using two steps because some towns have - in the name
    tidyr::separate(StateVar, into = c("State", "Var"), sep = "-") |> # which also seps the variable
    dplyr::filter(Var %in% c(filterVar, indgroup) & !is.na(Value)) |>
    tidyr::unite("Town", c(Town, State), sep = ",") |>
    dplyr::mutate(Town = dplyr::case_when(
      stringr::str_detect(Town, "OTHER,VA") ~ "OTHER,VA (includes REEDVILLE)",
      stringr::str_detect(Town, "Bronx/City Island") ~ "BRONX,NY",
      stringr::str_detect(Town, "Reedville/District 5") ~ "Reedville,VA",
      stringr::str_detect(Town, "Harpswell/Bailey Island") ~ "Harpswell,ME",
      stringr::str_detect(Town, "South Kingstown/Kingston/Wakefield-Peacedale") ~ "South Kingstown,RI",
      TRUE ~ Town # This keeps everything else the same
    ))|>
    # tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::filter(EPU == filterEPUs) |>
    tidyr::separate(
      Town,
      into = c("city", "state"),
      sep = ",",
      remove = FALSE
    ) |>
    dplyr::mutate(city = stringr::str_to_title(city)) |>
    tidyr::unite("Town", city, state, sep = ", ")

  top.coms <- eng |>
    dplyr::filter(Var == filterVar) |>
    dplyr::filter(Time == max(Time)) |>
    dplyr::arrange(desc(Value)) |>
    head(n = n) |>
    dplyr::mutate(
      Town = dplyr::recode(Town, "Other, VA (includes REEDVILLE)" = "Reedville, VA")
    ) |>
    dplyr::pull(Town)

  data = eng |>
    dplyr::filter(Var %in% indgroup) |>
    dplyr::group_by(Town) |>
    dplyr::mutate(total = sum(Value, na.rm = T)) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::filter(Time == max(Time)) |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::select(Community = Town, dplyr::ends_with('_rank')) |>
    dplyr::mutate(dplyr::across(
      dplyr::ends_with('_rank'),
      ~ dplyr::case_when(
        . == 1 ~ "low",
        . == 2 ~ "med",
        . == 3 ~ "med high",
        . == 4 ~ "high",
        TRUE ~ NA_character_
      )
    )) |>
    dplyr::filter(Community %in% top.coms)

  return(data)
}


attr(plot_community_factors, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_community_factors, "varName") <- c(
  "Social",
  "Economic",
  "Gentrification"
)
attr(plot_community_factors, "plottype") <- c("Commercial", "Recreational")
