#' plot cold_pool
#'
#' Create 3 panel plot of cold pool temperature, extent, and persistence time series.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" only, default)
#' @param varName Character string. Which plot (NULL, "cold_pool", "persistence","extent")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_cold_pool <- function(shadedRegion = NULL,
                           report="MidAtlantic",
                           varName = NULL,
                           n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   filterEPUs <- c("GB", "GOM")
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  # if no PSYyear given by user, default to the last 2 years in dataset
  #PSYyear <- ifelse(is.null(PSYyear), max(ecodata::cold_pool$Time)-1, PSYyear)

  # new dataset has both GLORYS and PSY for comparison
  # preferentially use GLORYS where duplicates exist

  cpdup <- ecodata::cold_pool |>
    dplyr::group_by(Time, Var, EPU) |>
    dplyr::mutate(duplicated = dplyr::n()>1) |>
    dplyr::ungroup()

  cpts <- cpdup |>
    dplyr::filter(!duplicated) |>
    dplyr::bind_rows(cpdup |>
                       dplyr::filter(duplicated & source=="GLORYS")) |>
    dplyr::select(-duplicated)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  cpi<- cpts |>
    dplyr::filter(stringr::str_detect(Var, pattern = "cold_pool")) |>
    dplyr::mutate(Value = Value*-1) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)  |>
    dplyr::mutate(Upper = cold_pool_index + se_cold_pool_index,
                  Lower = cold_pool_index - se_cold_pool_index) |>
    dplyr::select(!se_cold_pool_index)|>
    dplyr::rename(Value = cold_pool_index)|>
    dplyr::mutate(Var = c("cold_pool_index")) |>
    ggplot2::ggplot() +
    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf)+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value), size = setup$lwd) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, shape = source), size = setup$pcex) +
    ggplot2::scale_shape_manual(values = c(16, 1))+
    ggplot2::theme(legend.position = "none")+
    # ggplot2::geom_ribbon(aes(x = Time, ymin = Lower, ymax = Upper), fill = "gray")+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0))+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value))+
    ecodata::geom_lm(n=n, ggplot2::aes(x = Time, y = Value))+
    #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
    ggplot2::ylab("Cold Pool Index (x(-1))") +
    #ggplot2::scale_y_reverse()+
    ggplot2::xlab("")+
    ecodata::theme_ts()+
    ecodata::theme_title()+
    ggplot2::annotate("text", x = 1990, y = 2.2, label = "Colder", size = 4,colour = "blue")+
    ggplot2::annotate("text", x = 1990, y = -2.5, label = "Warmer",size = 4, colour = "red")




  ei<- cpts |>
    dplyr::filter(stringr::str_detect(Var, pattern = "extent")) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)  |>
    dplyr::mutate(Upper = extent_index + se_extent_index,
                  Lower = extent_index - se_extent_index) |>
    dplyr::select(!se_extent_index)|>
    dplyr::rename(Value = extent_index)|>
    dplyr::mutate(Var = c("extent_index")) |>
    ggplot2::ggplot() +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf)+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value), size = setup$lwd) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, shape = source), size = setup$pcex) +
    ggplot2::scale_shape_manual(values = c(16, 1))+
    ggplot2::theme(legend.position = "none")+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0))+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value))+
    ecodata::geom_lm(n=n, ggplot2::aes(x = Time, y = Value))+
    #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
    ggplot2::ylab("Spatial Extent Index") +
    ggplot2::xlab("")+
    ecodata::theme_ts()+
    ecodata::theme_title()+
    # ggplot2::annotate("segment", x = 2025, xend = 2025, y = 0.05, yend = 50,
    #          colour = "blue", size = 0.70, arrow = arrow())+
    # ggplot2::annotate("segment", x = 2025, xend = 2025, y = -0.05, yend = -250,
    #          colour = "red", size = 0.70, arrow = arrow())+
    ggplot2::annotate("text", x = 1990, y = 125, label = "Larger",size = 4, colour = "blue")+
    ggplot2::annotate("text", x = 1990, y = -350, label = "Smaller",size = 4, colour = "red")


  pi<- cpts |>
    dplyr::filter(stringr::str_detect(Var, pattern = "persistence")) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value)  |>
    dplyr::mutate(Upper = persistence_index + se_persistence_index,
                  Lower = persistence_index - se_persistence_index) |>
    dplyr::select(!se_persistence_index)|>
    dplyr::rename(Value = persistence_index)|>
    dplyr::mutate(Var = c("persistence_index"))|>
    ggplot2::ggplot() +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf)+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value), size = setup$lwd) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, shape = source), size = setup$pcex) +
    ggplot2::scale_shape_manual(values = c(16, 1))+
    ggplot2::theme(legend.position = "none")+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0))+
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value))+
    ecodata::geom_lm(n=n, ggplot2::aes(x = Time, y = Value))+
    #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
    ggplot2::ylab("Persistence Index") +
    ggplot2::xlab("")+
    ecodata::theme_ts()+
    ecodata::theme_title()+
    # ggplot2::annotate("segment", x = 2025, xend = 2025, y = 0.01, yend = 0.6,
    #          colour = "blue", size = 0.70, arrow = arrow())+
    # ggplot2::annotate("segment", x = 2025, xend = 2025, y = -0.05, yend = -1.6,
    #          colour = "red", size = 0.70, arrow = arrow())+
    ggplot2::annotate("text", x = 1990, y = 1.5, label = "Longer", size = 4,colour = "blue")+
    ggplot2::annotate("text", x = 1990, y = -1.5, label = "Shorter", size = 4, colour = "red")

  #cowplot::plot_grid(cpi, pi, ei, labels = c('a', 'b', 'c'), align = "h")

  if (!is.null(varName)) {
    if (varName == "cold_pool") {
      p <- cpi
    } else if (varName == "persistence") {
      p <- pi
    } else if (varName == "extent") {
      p <- ei
    }
  } else {
    p <- gridExtra::grid.arrange(cpi, pi,ei, ncol=3)
  }

  return(p)
}

attr(plot_cold_pool,"report") <- c("MidAtlantic","NewEngland")
attr(plot_cold_pool,"varName") <- c("cold_pool", "persistence","extent")



  # Paste commented original plot code chunk for reference
  # cp1<- ecodata::cold_pool %>%
  #   dplyr::filter(Time >= 2021) %>%
  #   dplyr::mutate(Source = c("PSY"))
  # cpts<- ecodata::cold_pool %>%
  #   dplyr::filter(Time <= 2020) %>%
  #   dplyr::mutate(Source = c("Glorys")) %>%
  #   rbind(cp1)
  #
  #
  #
  # cpi<- cpts %>%
  #   dplyr::filter(stringr::str_detect(Var, pattern = "cold_pool")) %>%
  #   dplyr::mutate(Value = Value*-1) %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value)  %>%
  #   dplyr::mutate(Upper = cold_pool_index + se_cold_pool_index,
  #                 Lower = cold_pool_index - se_cold_pool_index) %>%
  #   dplyr::select(!se_cold_pool_index)%>%
  #   dplyr::rename(Value = cold_pool_index)%>%
  #   dplyr::mutate(Var = c("cold_pool_index")) %>%
  #   ggplot2::ggplot() +
  #   #Highlight last ten years
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf)+
  #   ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  #   ggplot2::geom_point(aes(x = Time, y = Value, shape = Source), size = pcex) +
  #   ggplot2::scale_shape_manual(values = c(16, 1))+
  #   ggplot2::theme(legend.position = "none")+
  #   # ggplot2::geom_ribbon(aes(x = Time, ymin = Lower, ymax = Upper), fill = "gray")+
  #   ggplot2::geom_hline(aes(yintercept = 0))+
  #   ecodata::geom_gls(aes(x = Time, y = Value))+
  #   #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #   ggplot2::ylab("Cold Pool Index (x(-1))") +
  #   #ggplot2::scale_y_reverse()+
  #   ggplot2::xlab("")+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()+
  #   ggplot2::annotate("text", x = 1990, y = 2.2, label = "Colder", size = 4,colour = "blue")+
  #   ggplot2::annotate("text", x = 1990, y = -2.5, label = "Warmer",size = 4, colour = "red")
  #
  #
  #
  #
  # ei<- cpts %>%
  #   dplyr::filter(stringr::str_detect(Var, pattern = "extent")) %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value)  %>%
  #   dplyr::mutate(Upper = extent_index + se_extent_index,
  #                 Lower = extent_index - se_extent_index) %>%
  #   dplyr::select(!se_extent_index)%>%
  #   dplyr::rename(Value = extent_index)%>%
  #   dplyr::mutate(Var = c("extent_index")) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf)+
  #   ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  #   ggplot2::geom_point(aes(x = Time, y = Value, shape = Source), size = pcex) +
  #   ggplot2::scale_shape_manual(values = c(16, 1))+
  #   ggplot2::theme(legend.position = "none")+
  #   ggplot2::geom_hline(aes(yintercept = 0))+
  #   ecodata::geom_gls(aes(x = Time, y = Value))+
  #   #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #   ggplot2::ylab("Spatial Extent Index") +
  #   ggplot2::xlab("")+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()+
  #   # ggplot2::annotate("segment", x = 2025, xend = 2025, y = 0.05, yend = 50,
  #   #          colour = "blue", size = 0.70, arrow = arrow())+
  #   # ggplot2::annotate("segment", x = 2025, xend = 2025, y = -0.05, yend = -250,
  #   #          colour = "red", size = 0.70, arrow = arrow())+
  #   ggplot2::annotate("text", x = 1990, y = 125, label = "Larger",size = 4, colour = "blue")+
  #   ggplot2::annotate("text", x = 1990, y = -400, label = "Smaller",size = 4, colour = "red")
  #
  #
  # pi<- cpts %>%
  #   dplyr::filter(stringr::str_detect(Var, pattern = "persistence")) %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value)  %>%
  #   dplyr::mutate(Upper = persistence_index + se_persistence_index,
  #                 Lower = persistence_index - se_persistence_index) %>%
  #   dplyr::select(!se_persistence_index)%>%
  #   dplyr::rename(Value = persistence_index)%>%
  #   dplyr::mutate(Var = c("persistence_index"))%>%
  #   ggplot2::ggplot() +
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf)+
  #   ggplot2::geom_line(aes(x = Time, y = Value), size = lwd) +
  #   ggplot2::geom_point(aes(x = Time, y = Value, shape = Source), size = pcex) +
  #   ggplot2::scale_shape_manual(values = c(16, 1))+
  #   ggplot2::theme(legend.position = "none")+
  #   ggplot2::geom_hline(aes(yintercept = 0))+
  #   ecodata::geom_gls(aes(x = Time, y = Value))+
  #   #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #   ggplot2::ylab("Persistence Index") +
  #   ggplot2::xlab("")+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()+
  #   # ggplot2::annotate("segment", x = 2025, xend = 2025, y = 0.01, yend = 0.6,
  #   #          colour = "blue", size = 0.70, arrow = arrow())+
  #   # ggplot2::annotate("segment", x = 2025, xend = 2025, y = -0.05, yend = -1.6,
  #   #          colour = "red", size = 0.70, arrow = arrow())+
  #   ggplot2::annotate("text", x = 1990, y = 0.8, label = "Longer", size = 4,colour = "blue")+
  #   ggplot2::annotate("text", x = 1990, y = -1.8, label = "Shorter", size = 4, colour = "red")
  #
  # #cowplot::plot_grid(cpi, pi, ei, labels = c('a', 'b', 'c'), align = "h")
  #
  # gridExtra::grid.arrange(cpi, pi,ei, ncol=3)
  #
  #
