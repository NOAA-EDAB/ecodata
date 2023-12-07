#' plot comdat
#'
#' Plot time series of commercial landings or revenue for various aggregations.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("landings", "revenue")
#' @param plottype Character string. Which plot ("total", "guild")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_comdat <- function(shadedRegion = shadedRegion,
                        report="MidAtlantic",
                        varName="landings",
                        plottype="total") {

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
  managed_landings <- ecodata::comdat  |>
    dplyr::filter(Var %in% c("Planktivore MAFMC managed species - Seafood Landings",
                             "Piscivore MAFMC managed species - Seafood Landings",
                             "Benthivore MAFMC managed species - Seafood Landings",
                             "Apex Predator MAFMC managed species - Seafood Landings",
                             "Benthos MAFMC managed species - Seafood Landings",
                             "Planktivore NEFMC managed species - Seafood Landings",
                             "Piscivore NEFMC managed species - Seafood Landings",
                             "Benthivore NEFMC managed species - Seafood Landings",
                             "Apex Predator NEFMC managed species - Seafood Landings",
                             "Benthos NEFMC managed species - Seafood Landings",
                             "Planktivore JOINT managed species - Seafood Landings",
                             "Piscivore JOINT managed species - Seafood Landings",
                             "Benthivore JOINT managed species - Seafood Landings",
                             "Apex Predator JOINT managed species - Seafood Landings",
                             "Benthos JOINT managed species - Seafood Landings"),
                  Time >= 1986) |>
    dplyr::filter(stringr::str_detect(Var, paste0("JOINT|", setup$council_abbr)))


  US_landings <- ecodata::comdat  |>
    dplyr::filter(Var == "Seafood Landings",
                  Time >= 1986)
  # #Total landings
  total_landings <- ecodata::comdat  |>
    dplyr::filter(Var == "Landings",
                  Time >= 1986)

  total_landings_agg <- total_landings |>
    dplyr::group_by(EPU,Time) |>
    dplyr::summarise(Value = sum(Value)/1000) |>
    dplyr::mutate(Var = "Total",hline = mean(Value))

  us_total_landings_agg <- US_landings |>
    dplyr::group_by(EPU,Time) |>
    dplyr::summarise(Value = sum(Value)/1000) |>
    dplyr::mutate(Var = "USTotal",hline = mean(Value))

  managed_landings_agg <- managed_landings |>
    dplyr::group_by(EPU,Time) |>
    dplyr::summarise(Value = sum(Value)/1000) |>
    dplyr::mutate(Var = "Managed",hline = mean(Value))

  landings_agg <- rbind(total_landings_agg, managed_landings_agg, us_total_landings_agg)# %>%
  #  dplyr::mutate(Value = Value/1000)
  series.col2 <- c("indianred",  "black", "steelblue4")

  landings_aggx<- landings_agg |>
    dplyr::filter(EPU %in% filterEPUs)


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- landings_aggx |>
    ggplot2::ggplot()+

    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value,
                                   group = Var),
                      alpha = setup$trend.alpha, size = setup$trend.size) +
    #ecodata::geom_lm(aes(x = Time, y = Value))+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value, color = Var), size = setup$lwd) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value, color = Var), size = setup$pcex) +
    #ggplot2::ylim(15,600)+
    # ggplot2::geom_line(data =landings_agg_post2018, aes(x = Time, y = Value, color = Var), size = lwd) +
    # ggplot2::geom_point(data =landings_agg_post2018,aes(x = Time, y = Value, color = Var), size = pcex, shape = 1)+
    # ggplot2::geom_line(data =landings_agg_pre2018, aes(x = Time, y = Value, color = Var), size = lwd) +
    # ggplot2::geom_point(data =landings_agg_pre2018,aes(x = Time, y = Value, color = Var), size = pcex, shape = 16)+
    #  ggplot2::scale_y_continuous(labels = function(l){trans = l / 1000})+
    ggplot2::scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0.01, 0.01)) +
    ggplot2::scale_color_manual(values = series.col2, aesthetics = "color")+
    ggplot2::facet_wrap(~EPU, scales = "free")+
    ggplot2::guides(color = "none") +
    ggplot2::ylab(expression("Landings (10"^3*"mt)")) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::theme(legend.position = "left")+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     color = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ecodata::theme_ts() +
    ggplot2::ggtitle(setup$region)+
    ecodata::theme_title() +
    ecodata::theme_facet()

  # optional code for New England specific (2 panel) formatting
  if (report == "NewEngland") {
    p <- p +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())

  }

  return(p)

  # Paste commented original plot code chunk for reference
  # ecodata::dataset |>
  #   dplyr::filter(Var %in% c("..."),
  #                 EPU == "...") |>
  #   ... more dataset wrangling as necessary |>
  #   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   ggplot2::ggtitle("Title")+
  #   ggplot2::ylab(expression("Y label"))+
  #   ggplot2::xlab(element_blank())+
  #   ecodata::geom_gls()+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

}
