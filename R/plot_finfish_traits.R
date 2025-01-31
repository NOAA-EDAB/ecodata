#' plot finfish traits
#'
#' Plot time series of community level fish traits for spring and fall.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable:Trophic Level, Offspring Size, 
#' Age at Maturity, Length at Maturity, Fecundity, Maximum Theoretical Length, Growth Rate, 
#' Maximum Observed Length, and three Pricipal Components Axes Pace of Life (PC1), PC2, andPC3 
#' ("trophic_level","offspring_size","age_maturity","length_maturity","fecundity","l_inf","k",
#' "max_obs_length","PC1","PC2","PC3")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_finfish_traits <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName="fecundity",
                              n = 0) {

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
  
  varTitle <- c("Trophic Level", "Offspring Size", "Age at Maturity", "Length at Maturity",
                "Fecundity", "Maximum Theoretical Length", "Growth Rate", "Maximum Observed Length",
                "Pace of Life", "PC2", "PC3")
  names(varTitle) <- c("trophic_level","offspring_size","age_maturity","length_maturity",
                       "fecundity","l_inf","k","max_obs_length",
                       "PC1","PC2","PC3")
  
   fix<- ecodata::finfish_traits |>
     tidyr::separate(Var, into = c("Season", "Var"), sep = "-") |>
     dplyr::group_by(Season, Var, EPU) |>
     dplyr::summarise(hline = mean(Value))
   
   varUnit <- ecodata::finfish_traits |>
     tidyr::separate(Var, into = c("Season", "Var"), sep = "-") |>
     dplyr::filter(Var %in% c(varName)) |>
     dplyr::select(Units) |>
     dplyr::distinct()

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
   p <- ecodata::finfish_traits |>
     tidyr::separate(Var, into = c("Season", "Var"), sep = "-") |>
     dplyr::filter(Var %in% c(varName),
                   EPU %in% filterEPUs) |>
     dplyr::left_join(fix) |>
     ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, group = Season))+
     ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                       xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                       ymin = -Inf, ymax = Inf) +
     ggplot2::geom_point(ggplot2::aes(color = Season))+
     ggplot2::geom_line(ggplot2::aes(color = Season))+
     ggplot2::ggtitle(paste(varTitle[varName]))+
     ggplot2::ylab(paste0(varName, " (", unlist(varUnit), ")"))+
     ggplot2::xlab(ggplot2::element_blank())+
     ggplot2::facet_wrap(~EPU)+
     ecodata::geom_gls(ggplot2::aes(x = Time, y = Value,
                                    group = Season),
                       alpha = setup$trend.alpha, size = setup$trend.size)+
     ecodata::geom_lm(ggplot2::aes(x = Time, y = Value,
                                   group = Season),
                      alpha = setup$trend.alpha, size = setup$trend.size, n=n) +
     ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                      color = Season),
                         linewidth = setup$hline.size,
                         alpha = setup$hline.alpha,
                         linetype = setup$hline.lty)+
     ecodata::theme_ts()+
     ecodata::theme_facet()+
     ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)


}

attr(plot_finfish_traits,"report") <- c("MidAtlantic","NewEngland")
attr(plot_finfish_traits,"varName") <- c("trophic_level","offspring_size","age_maturity","length_maturity","fecundity","l_inf","k","max_obs_length","PC1","PC2","PC3")

