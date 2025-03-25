#' plot exp_n
#'
#' Plots time series of expected number of species from NEFSC bottom trawl survey.
#' Plot by season
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Season to plot ("fall", "spring")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_exp_n <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                       varName="fall",
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

  season <- stringr::str_to_upper(varName)

  if (season == "fall"){
    start_year = 1965
  } else {
    start_year = 1968
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  exp<- ecodata::exp_n |>
    tidyr::separate(Var, into = c("Var", "Season"), sep = "-") |>
    dplyr::filter( EPU %in% filterEPUs, Season == season,
                   stringr::str_detect(Var, 'AlbatrossSD|BigelowSD')) |>
    dplyr::rename(VarSD = Var, ValueSD = Value)

  exp2<- ecodata::exp_n |>
    tidyr::separate(Var, into = c("Var", "Season"), sep = "-") |>
    dplyr::filter(EPU %in% filterEPUs, Season == season,
                  Var %in% c("Albatross", "Bigelow"))   |>
    dplyr::left_join(exp) |>
    dplyr::mutate(upper = Value+ValueSD,
                  lower = Value - ValueSD)


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- exp2 |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, fill = Var)) +

    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max ,
                      ymin = -Inf, ymax = Inf) +

    ggplot2::geom_ribbon(data = exp2,
                         ggplot2::aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
                         alpha = 0.5) +
    ggplot2::geom_line(size = setup$lwd-0.5) +
    ggplot2::geom_point(size = setup$pcex-0.5) +
    #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
    #ecodata::geom_lm()+
    # scale_color_manual(values = series.col, aesthetics = "color")+
    #ggplot2::guides(color = FALSE) +
    #ggplot2::geom_hline(aes(yintercept = hline,
    #               group = Var),
    #           size = hline.size,
    #           alpha = hline.alpha,
    #           linetype = hline.lty)+
    ggplot2::facet_wrap(EPU~.,scales = "free_y", ncol = 2) +
    ggplot2::ggtitle(paste("Expected Number of Species -", season))+
    ecodata::geom_gls() +
    ecodata::geom_lm(n=n) +
    #Axis and theme
    ggplot2::scale_x_continuous(breaks = seq(start_year, 2015, by = 10), expand = c(0.01, 0.01)) +
    ggplot2::ylab("n species per 1000 ind") +
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::theme_facet()+
    ggplot2::theme(strip.text= ggplot2::element_text(hjust=0),
                   legend.title = ggplot2::element_blank())+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)

}

attr(plot_exp_n,"varName") <- c("fall","spring")
attr(plot_exp_n,"report") <- c("MidAtlantic","NewEngland")


  # Paste commented original plot code chunk for reference
  # exp<- ecodata::exp_n %>%
  #   tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>%
  #   dplyr::filter( EPU == "MAB", Season == "FALL",
  #                  stringr::str_detect(Var, 'AlbatrossSD|BigelowSD')) %>%
  #   dplyr::rename(VarSD = Var, ValueSD = Value)
  #
  # exp2<- ecodata::exp_n %>%
  #   tidyr::separate(Var, into = c("Var", "Season"), sep = "-") %>%
  #   dplyr::filter(EPU == "MAB", Season == "FALL",
  #                 Var %in% c("Albatross", "Bigelow"))   %>%
  #   dplyr::left_join(exp) %>%
  #   dplyr::mutate(upper = Value+ValueSD,
  #                 lower = Value - ValueSD)
  #
  # exp2 %>%
  #   ggplot2::ggplot(aes(x = Time, y = Value, fill = Var)) +
  #
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max ,
  #                     ymin = -Inf, ymax = Inf) +
  #
  #   ggplot2::geom_ribbon(data = exp2, aes(ymax = pmax(upper, 0), ymin = lower, x = Time),
  #                        alpha = 0.5) +
  #   ggplot2::geom_line(size = lwd-0.5) +
  #   ggplot2::geom_point(size = pcex-0.5) +
  #   #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #   #ecodata::geom_lm()+
  #   # scale_color_manual(values = series.col, aesthetics = "color")+
  #   #ggplot2::guides(color = FALSE) +
  #   #ggplot2::geom_hline(aes(yintercept = hline,
  #   #               group = Var),
  #   #           size = hline.size,
  #   #           alpha = hline.alpha,
  #   #           linetype = hline.lty)+
  #   ggplot2::facet_wrap(EPU~.,scales = "free_y", ncol = 2) +
  #   ggplot2::ggtitle("Expected Number of Species - Fall")+
  #   #ecodata::geom_gls() +
  #   #Axis and theme
  #   ggplot2::scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  #   ggplot2::ylab("n species per 1000 ind") +
  #   ggplot2::xlab(element_blank())+
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  legend.title = element_blank())+
  #   ecodata::theme_title()
  #
  #
