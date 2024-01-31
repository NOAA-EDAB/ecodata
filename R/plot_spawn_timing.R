#' plot spawn_timing
#'
#' Plots time series of maturity stage and associated data for available stocks
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot:
#' maturity stage ("Resting", "Ripe", "Spent", "Developing"),
#' number of mature females ("MF"),
#' mean sampled bottom temperature ("meanTEMP"),
#' or mean sampled day of year ("meanJDAY")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_spawn_timing <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "Resting") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  # dataset has NA EPU field but may be added later, so keep this here
  # NOTE no filtering by EPU happens below, change later!

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix<- ecodata::spawn_timing |>
    tidyr::separate(Var, into = c("Season", "Species", "Stock", "Var"), sep = "_")

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  # SPRING data plots by default for spring spawning species
  # May have to change to add fall option later if we get fall spawners


  filt <- fix |>
    dplyr::filter(Season == "SPRING",
                  Var == varName) |>
    dplyr::mutate(Species = factor(Species, levels = c("Yellowtail", "Haddock")),
                  Stock = factor(Stock, levels = c("GOM", "CC", "GB", "SNE")))

  p <- ggplot2::ggplot(filt, ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ecodata::geom_gls() +
    #ggplot2::geom_bar(stat="identity", position="fill") +
    #ggplot2::scale_fill_manual(values = mat.col) +
    ggplot2::ylab(filt$Units) +
    ggplot2::facet_wrap(Species~Stock,
                        labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
    ecodata::theme_facet() +
    ecodata::theme_title()

  if(varName %in% c("Resting", "Ripe", "Spent", "Developing")){
    p <- p + ggplot2::ggtitle(paste(stringr::str_to_sentence(filt$Season), varName, "Spawning Stage"))
  }

  if(varName %in% c("MF", "meanTEMP", "meanJDAY")){
    varLong <- dplyr::case_when(varName == "MF" ~ "Mature Females",
                                varName == "meanTEMP" ~ "Mean Sampled Temperature",
                                varName == "meanJDAY" ~ "Mean Sampling Date")

    p <-p + ggplot2::ggtitle(paste(stringr::str_to_sentence(filt$Season), varLong))
  }

  return(p)

}
attr(plot_spawn_timing,"report") <- c("MidAtlantic","NewEngland")
attr(plot_spawn_timing, "varName") <- c("Resting", "Ripe", "Spent", "Developing", "MF", "meanTEMP", "meanJDAY")
