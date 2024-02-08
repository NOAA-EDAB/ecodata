#' plot species groupings by SOE year
#'
#' plots species_grouping dataset
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which year of SOE report ("2024", "2020","2018","2017")
#'
#' @return flextable object
#'
#'
#' @export
#'

plot_species_groupings <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "2024") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    message("Same figure for both reports ")
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  varName <- paste0("SOE.",substr(varName,3,4))

  # new table with all species listed by management entity
  fix <- ecodata::species_groupings |>
    dplyr::select(dplyr::contains(varName), COMNAME, Fed.Managed) |>
    dplyr::filter(get(varName) != "Other") |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::across(varName), Fed.Managed) |>
    dplyr::summarize_all(dplyr::funs(paste(na.omit(.), collapse = ", "))) |>
    dplyr::ungroup() |>
    tidyr::spread(Fed.Managed, COMNAME) |>
    dplyr::arrange(factor(get(varName), levels = c("Apex Predator", "Piscivore", "Planktivore", "Benthivore", "Benthos"))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) |>
    dplyr::rename(Guild = !!varName,
                  Joint = JOINT) |>
    dplyr::relocate(Guild,MAFMC,Joint)

  # fix<-fix[c(1,3,2,4,5)] |>
  #   dplyr::mutate_all(tolower)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #


  p <- flextable::flextable(fix) |>
    flextable::set_caption('Feeding guilds and management bodies.') |>
    flextable::fontsize(size=8, part = "all") |>
    flextable::set_header_labels(SOE.24 = "Guild",
                                 MAFMC = "MAFMC",
                                 JOINT = "Joint",
                                 NEFMC = "NEFMC",
                                 "<NA>" = "State or Other") |>
    flextable::width(width = c(1,1,1,1,3))


  if(report == "NewEngland") {
    p <- "Same figure for both regions"
  }

  return(p)


}


attr(plot_species_groupings,"report") <- c("MidAtlantic","NewEngland")
attr(plot_species_groupings,"varName") <- c("2024")
