#' plot hms_landings
#'
#' Plot time series of Highly Migratory Species (HMS) commercial landings or revenue.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland").
#' @param varName Character string. Which Variable to plot ("Landings", "Revenue")
#' HMS landings/revenue are by Mid Atlantic or New England, not EPU
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_hms_landings <- function(shadedRegion = shadedRegion,
                              report="MidAtlantic",
                              varName="Landings") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  apex<-ecodata::hms_landings |>
    dplyr::filter(stringr::str_detect(Var, varName)) |>
    tidyr::separate(Var, c("Var", "trash"), sep = "_") |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(Value = ifelse(stringr::str_detect(Var, varName),
                                 Value/1000000,
                                 Value),
                  hline = mean(Value))

  ylabdat <- ifelse(varName == "Revenue", expression("Revenue (10"^6*"USD)"),
                    expression("Landings (metric tons)"))


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- apex |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Var)) +
    ggplot2::geom_line(size = setup$lwd) +
    ggplot2::geom_point(size = setup$pcex) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                            color = Var,
                            size = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ecodata::theme_facet() +
    ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.title = ggplot2::element_blank())+
    ggplot2::ylab(ylabdat) +
    ggplot2::xlab("Time")+
    ggplot2::ggtitle(paste("HMS", setup$region, "Commercial", varName))+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(p)

  # Paste commented original plot code chunk for reference
  #Get data for plotting
  # ## Apex pred
  # apex<-ecodata::hms_landings %>%
  #   dplyr::filter(stringr::str_detect(Var, "Landings")) %>%
  #   #dplyr::mutate(Value = as.numeric(Value)) %>%
  #   separate(Var, c("Var", "trash"), sep = "_") #%>%
  # #dplyr::filter(!Var == "Smoothhound Sharks")
  #
  # ##Plot
  # p1<-apex %>%
  #   dplyr::filter(EPU == "NE") %>%
  #   dplyr::group_by(Var) %>%
  #   dplyr::mutate(hline = mean(Value)) %>%
  #
  #   ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_line(size = lwd) +
  #   ggplot2::geom_point(size = pcex) +
  #   # ggplot2::geom_hline(aes(yintercept = hline,
  #   #                color = Var,
  #   #                size = Var),
  #   #            size = hline.size,
  #   #            alpha = hline.alpha,
  #   #            linetype = hline.lty)+
  #   ecodata::theme_facet() +
  #   ggplot2::theme(strip.text=element_text(hjust=0),
  #                  legend.position = "bottom",
  #                  legend.direction = "horizontal",
  #                  legend.title = element_blank())+
  #   ggplot2::ylab("Landings (metric tons)")+
  #   ggplot2::ggtitle("HMS Commercial Landings")
  # ggplot2::xlab(element_blank())+
  #   ecodata::theme_title()
  #
  # p1
  #

}
