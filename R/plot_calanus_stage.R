#' plot calanus_stage
#'
#' Create bar plots of Calanus zooplankton stages over time
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_calanus_stage <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

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
  cal <- ecodata::calanus_stage |>
    dplyr::filter(EPU %in% filterEPUs) |>
    tidyr::separate(Var, into = c("Var", "season"), sep = "-") |>
    dplyr::filter(Var %in% c("c3", "c4", "c5", "adt")) |>
    dplyr::mutate(Var = dplyr::recode(Var, "c5" = "Stage 5",
                               "adt" = "Adult" ))

  cal$Var <- factor(cal$Var, levels = c("c3", "c4", "Stage 5", "Adult"))
  cal$season <- factor(cal$season, levels = c("Spring", "Summer", "Fall"))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- cal |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Var, fill = Var)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf)+
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::facet_wrap(EPU~season)+
    ggplot2::ylab("Calanus Stage (N/100m^3)") +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::ggtitle("Calanus Stage Abundance") +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())+
    ecodata::theme_facet()+
    ggplot2::scale_fill_manual(values = c("steelblue1","coral1","steelblue3", "coral3"))+
    ggplot2::scale_color_manual(values = c("steelblue1","coral1","steelblue3","coral3"))+
    ecodata::theme_title()


   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)

}


attr(plot_calanus_stage,"report") <- c("MidAtlantic","NewEngland")

  # Paste commented original plot code chunk for reference
  # MAB
  # cal <- ecodata::calanus_stage %>%
  #   dplyr::filter(EPU == "MAB") %>%
  #   tidyr::separate(Var, into = c("Var", "season"), sep = "-") %>%
  #   filter(Var %in% c("c3", "c4", "c5", "adt"))
  #
  # cal$Var <- factor(cal$Var, levels = c("c3", "c4", "c5", "adt"))
  # cal$season <- factor(cal$season, levels = c("Spring", "Summer", "Fall"))
  #
  # cal %>%
  #   ggplot2::ggplot(aes(x = Time, y = Value, color = Var, fill = Var)) +
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf)+
  #   ggplot2::geom_bar(stat = "identity")+
  #   ggplot2::facet_wrap(~season)+
  #   ggplot2::ylab("Calanus Stage (N/100m^3)") +
  #   ggplot2::xlab(element_blank())+
  #   ggplot2::ggtitle("MAB Calanus Stage Abundance") +
  #   ggplot2::theme(legend.position = "bottom",
  #                  legend.title = element_blank())+
  #   ecodata::theme_facet()+
  #   scale_fill_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))+
  #   scale_color_manual(values = c("steelblue1","steelblue3", "coral1", "coral3"))+
  #   ecodata::theme_title()
  #
  #

