#' plot zoo_diversity
#'
#' zooplanktn diversity index (Shannon index)
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_zoo_diversity <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    to_string <- ggplot2::as_labeller("MAB Zooplankton Diversity")
  } else {
    filterEPUs <- c("GB", "GOM")
    to_string <- ggplot2::as_labeller(c("GB" = "GB Zooplankton Diversity","GOM"="GOM Zooplankton Diversity"))

  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::zoo_diversity |>
     dplyr::filter(EPU %in% filterEPUs,
                   Time > 1991)

   mn <- fix |>
     dplyr::group_by(EPU,Var) |>
     dplyr::summarise(mn = mean(Value,na.rm = T),
                      .groups="drop")

   fix <- fix |>
     dplyr::left_join(mn,by=c("Var","EPU"))
  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value,group=Var))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+

    ggplot2::ylab("Zooplankton Diversity (Shannon)")+
    ggplot2::xlab(ggplot2::element_blank())

  #if(report == "MidAtlantic"){
   # p <-  p + ggplot2::ggtitle("MAB  Zooplankton Diversity")
  #} else {
    p <- p + ggplot2::facet_wrap(~EPU,scales = "free_y")#,labeller = to_string)
  #}


    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = mn),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ecodata::geom_gls() +
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = element_blank())
   #
   #  }

    return(p)
}

attr(plot_zoo_diversity,"report") <- c("MidAtlantic","NewEngland")
