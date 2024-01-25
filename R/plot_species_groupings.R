#' plot species groupings by SOE year
#'
#' plots species_grouping dataset
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which year of SOE report ("SOE.24", "SOE.20","SOE.18","SOE.17")
#'
#' @return flextable object
#'
#'
#' @export
#'

plot_species_groupings <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "SOE.24") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  varName <- toupper(varName)

  # new table with all species listed by management entity
  fix <- ecodata::species_groupings |>
    dplyr::select(dplyr::contains(varName), COMNAME, Fed.Managed) |>
    dplyr::filter(get(varName) != "Other") |>
    dplyr::distinct() |>
    dplyr::group_by(dplyr::across(varName), Fed.Managed) |>
    dplyr::summarize_all(dplyr::funs(paste(na.omit(.), collapse = ", "))) |>
    tidyr::spread(Fed.Managed, COMNAME) |>
    dplyr::arrange(factor(get(varName), levels = c("Apex Predator", "Piscivore", "Planktivore", "Benthivore", "Benthos")))
  fix<-fix[c(1,3,2,4,5)] |>
    dplyr::mutate_all(tolower)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  p <- flextable::flextable(fix) |>
    flextable::set_caption('Feeding guilds and management bodies.') |>
    flextable::fontsize(size=8, part = "all") |>
    flextable::set_header_labels(SOE.20 = "Guild",
                                 MAFMC = "MAFMC",
                                 JOINT = "Joint",
                                 NEFMC = "NEFMC",
                                 "<NA>" = "State or Other") |>
    flextable::width(width = c(1,1,1,1,3))

    return(p)


}


attr(plot_species_groupings,"report") <- c("MidAtlantic","NewEngland")
attr(plot_species_groupings,"varName") <- c("SOE.24")
