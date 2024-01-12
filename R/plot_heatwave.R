#' plot heatwave
#'
#' Time series plots of detrended temperature extreme event maximum intensity
#' and total days for either Surface or Bottom temperature.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("Surface", "Bottom")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_heatwave <- function(shadedRegion = NULL,
                          report="MidAtlantic",
                          varName="Surface") {

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
  durdvar <- paste0("duration-",varName,"Detrended")

  durd <- ecodata::heatwave |>
    dplyr::filter(Var == durdvar) |>
    dplyr::group_by(Time, EPU, Var, Units) |>
    dplyr::summarise(Value = sum(Value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Var = dplyr::recode(Var, .default = "Total Days Detrended (N days)"))

  maxindvar <- paste0("maximum intensity-",varName,"Detrended")

  maxind <- ecodata::heatwave |>
    dplyr::filter(Var == maxindvar) |>
    dplyr::group_by(Time, EPU, Var, Units) |>
    dplyr::summarise(Value = max(Value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Var = dplyr::recode(Var, .default = "Maximum Intensity Detrended (degree C)"))

  hw<- durd |>
    rbind( maxind) |>
    dplyr::group_by(Var, EPU) |>
    dplyr::mutate(hline = mean(Value)) |>
    dplyr::filter(EPU %in% filterEPUs)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- hw |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::geom_point(ggplot2::aes(x = Time, y = Value)) +
    ecodata::geom_gls(ggplot2::aes(x = Time, y = Value, group = Var)) +
    #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
    ggplot2::ylab("") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle(paste(setup$region, varName, "Marine Heatwave Intesity")) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::facet_wrap(EPU~Var,
                        #labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
                        scales = "free")+
    ecodata::theme_facet()+
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0,
                                                    face = "italic"))+
    ecodata::theme_title()

  # optional code for New England specific (2 panel) formatting
  if (report == "NewEngland") {
    p <- p +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())

  }

  return(p)

}
attr(plot_heatwave,"varName") <- c("Surface","Bottom")
attr(plot_heatwave,"report") <- c("MidAtlantic","NewEngland")



  # Paste commented original plot code chunk for reference
  # durd <- ecodata::heatwave %>%
  #   dplyr::filter(Var == "duration-SurfaceDetrended") %>%
  #   dplyr::group_by(Time, EPU, Var, Units) %>%
  #   dplyr::summarise(Value = sum(Value)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(Var = dplyr::recode(Var, "duration-SurfaceDetrended" = "Total Days Detrended (N days)"))
  #
  # maxind <- ecodata::heatwave %>%
  #   dplyr::filter(Var == "maximum intensity-SurfaceDetrended") %>%
  #   dplyr::group_by(Time, EPU, Var, Units) %>%
  #   dplyr::summarise(Value = max(Value)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(Var = dplyr::recode(Var, "maximum intensity-SurfaceDetrended" = "Maximum Intensity Detrended (degree C)"))
  #
  #
  # hw<- durd %>%
  #   rbind( maxind) %>%
  #   dplyr::group_by(Var, EPU) %>%
  #   dplyr::mutate(hline = mean(Value))
  #
  # mab.hw<- hw %>% dplyr::filter(EPU == epu_abbr)
  # mab.hw %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(aes(x = Time, y = Value)) +
  #   ggplot2::geom_point(aes(x = Time, y = Value)) +
  #   ecodata::geom_gls(aes(x = Time, y = Value, group = Var)) +
  #   #ecodata::geom_lm(aes(x = Time, y = Value, group = Var))+
  #   ggplot2::ylab("") +
  #   ggplot2::xlab(element_blank())+
  #   ggplot2::ggtitle("Mid-Atlantic Marine Heatwave Intesity") +
  #   ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  #   ggplot2::geom_hline(aes(yintercept = hline),
  #                       size = hline.size,
  #                       alpha = hline.alpha,
  #                       linetype = hline.lty)+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::facet_wrap(~Var, scales = "free")+
  #   ecodata::theme_facet()+
  #   ggplot2::theme(strip.text=element_text(hjust=0,
  #                                          face = "italic"))+
  #   ecodata::theme_title()
  #
  #
