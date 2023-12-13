#' plot hms_cpue
#'
#' Plot time series of NEUS Highly Migratory Species (HMS) groups: sharks or tunas.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" default, same plot for both)
#' @param varName Character string. Which Variable to plot ("shark", "tuna"). Sharks
#' are categorized as large coastal, pelagic, prohibited, and small coastal, while
#' tuna are by species.
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_hms_cpue <- function(shadedRegion = shadedRegion,
                          report="MidAtlantic",
                          varName = "shark") {

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
  varabbr <- stringr::str_to_upper(varName)

  sp_cat<- ecodata::hms_category

  hms <- ecodata::hms_cpue |>
    dplyr::filter(stringr::str_detect(Var, varabbr)) #|>

  if(varName=="shark"){
    hms <-  hms |>
    dplyr::rename(COMMON_POP = Var) |>
    dplyr::left_join(sp_cat) |>
    dplyr::group_by(Time, SP_CATEGORY) |>
    dplyr::summarise(Value = sum(Value)) |>
    dplyr::rename("Var" = "SP_CATEGORY") |>
    dplyr::filter(!Var == "NA")
  }

  #if(varName=="all") hms <- ecodata::hms_cpue

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- hms |>
    ggplot2::ggplot()+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point(ggplot2::aes(x=Time, y = Value, color = Var))+
    ggplot2::geom_line(ggplot2::aes(x=Time, y = Value, color = Var))+
    ggplot2::scale_color_discrete(name = "Category")+
    #{if(varName=="all") ggplot2::facet_wrap(~Var, scales = "free")}+
    ggplot2::ggtitle(paste("HMS POP", varabbr, "CPUE"))+
    ggplot2::ylab("Number per Haul")+
    ecodata::theme_ts()+
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
  # one code chunk: macrofauna_MAB.Rmd-hms-cpue-sharks.R
  # sp_cat<- ecodata::hms_category
  # ecodata::hms_cpue %>%
  #   filter(stringr::str_detect(Var, "SHARK")) %>%
  #   rename(COMMON_POP = Var) %>%
  #   left_join(sp_cat) %>%
  #   group_by(Time, SP_CATEGORY) %>%
  #   summarise(Value = sum(Value)) %>%
  #   rename("Var" = "SP_CATEGORY") %>%
  #   filter(!Var == "NA") %>%
  #   ggplot()+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_point(aes(x=Time, y = Value, color = Var))+
  #   ggplot2::geom_line(aes(x=Time, y = Value, color = Var))+
  #   ggplot2::scale_color_discrete(name = "Category")+
  #   #ggplot2::facet_wrap(~Var, scales = "free")+
  #   ggplot2::ggtitle("HMS POP SHARK CPUE")+
  #   ggplot2::ylab("Number per Haul")+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

}
