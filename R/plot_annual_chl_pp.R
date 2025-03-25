#' plot chl_pp
#'
#' Plot time series of chlorophyll a (chl), primary production (pp), weekly, monthly,
#' or anomalies.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("chl","pp")
#' @param plottype Character string. Which plot ("mean","total")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_annual_chl_pp <- function(shadedRegion = NULL,
                        report="MidAtlantic",
                        varName="chl",
                        plottype="mean",
                        EPU = "MAB",
                        n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  if (plottype == "mean") {
    type <- "MEAN"
  } else {
    type<-  "MTON"
  }
  if (varName == "chl") {
    var <- "CHLOR_A"
  } else {
    var <-  "PPD"
  }

  varN <- paste0(var,"_ANNUAL_",type)

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix <- ecodata::annual_chl_pp |>
    dplyr::mutate(Time = as.integer(gsub("A_","",Time))) |>
    dplyr::filter(EPU == filterEPUs,
                  Var == varN)

  hline <- mean(fix$Value)
  varunits <- unique(fix$Units)
  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

    p <- fix |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
      #ecodata::geom_lm(aes(x = Year, y = Value, group = Month))+
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::ggtitle(paste0(stringr::str_to_title(plottype)," ",var)) +
      ggplot2::ylab(varunits) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty)+
      ecodata::geom_lm(n=n) +
      ecodata::theme_facet() +
      ecodata::theme_title()

  return(p)

}

attr(plot_annual_chl_pp,"report") <- c("MidAtlantic","NewEngland")
attr(plot_annual_chl_pp,"varName") <- c("chl","pp")
attr(plot_annual_chl_pp,"plottype") <- c("mean","total")
attr(plot_annual_chl_pp,"EPU") <- c("MAB","GB","GOM")
