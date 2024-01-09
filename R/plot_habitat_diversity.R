#' plot habitat diversity
#'
#' Plots ecodata::habitat_diversity (Shanon index)
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("Diversity","Richness")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_habitat_diversity <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "Diversity") {

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
  if (varName == "Diversity") {
    varNames <- "Shannon"
    fix<- ecodata::habitat_diversity |>
      dplyr::filter(Var == varNames,
                    EPU %in% filterEPUs) |>
      dplyr::group_by(EPU) |>
      dplyr::mutate(mean = as.numeric(Value),
                    Time = as.numeric(Time),
                    hline = mean(Value))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- ecodata::habitat_diversity |>
    dplyr::filter(grepl(varNames,Var)) |>
    dplyr::group_by(EPU) |>
    dplyr::mutate(mean = as.numeric(Value),
                  Time = as.numeric(Time),
                  hline = mean(Value)) |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value,color = EPU))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    #ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper), alpha = 0.5)+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::geom_point(data = fix, aes(x = Time, y = mean), size = setup$line.size)+
    ggplot2::geom_line(data = fix, aes(x = Time, y = mean), linewidth = setup$line.size)+
    ggplot2::ggtitle(paste(report,"Species Shannon Diversity from Habitat Assessment"))+
    ggplot2::ylab("Shannon Diversity")+
    ggplot2::xlab(ggplot2::element_blank())+

    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

  } else { # Richness
    varNames <- "Richness"
    fix<- ecodata::habitat_diversity |>
      dplyr::filter(grepl(varNames,Var),
                    EPU %in% filterEPUs) |>
      tidyr::separate(Var, into = c("Var", "Richness"),sep = "-") |>
      tidyr::pivot_wider(values_from = "Value", names_from = "Var") |>
      dplyr::group_by(EPU) |>
      dplyr::mutate(mean = as.numeric(mean),
                    lower = as.numeric(lower),
                    upper = as.numeric(upper),
                    Time = as.numeric(Time),
                    hline = mean(mean))

    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #
    p <- ecodata::habitat_diversity |>
      dplyr::filter(grepl(varNames,Var)) |>
      tidyr::separate(Var, into = c("Var", "Richness"),sep = "-") |>
      tidyr::pivot_wider(values_from = "Value", names_from = "Var") |>
      dplyr::group_by(EPU) |>
      dplyr::mutate(mean = as.numeric(mean),
                    lower = as.numeric(lower),
                    upper = as.numeric(upper),
                    Time = as.numeric(Time),
                    hline = mean(mean)) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = mean, fill = EPU))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymax = upper, ymin = lower,color=EPU), alpha=setup$shade.alpha)     +
      ggplot2::geom_point(ggplot2::aes(color = EPU))+
      ggplot2::geom_line(ggplot2::aes(color = EPU))+
      ggplot2::geom_point(data = fix, ggplot2::aes(x = Time, y = mean,color=EPU))+
      ggplot2::geom_line(data = fix, ggplot2::aes(x = Time, y = mean,color=EPU), size = setup$line.size)+
      ggplot2::ggtitle(paste(report,"Species Richness from NEFSC Bottom Trawl Survey"))+
      ggplot2::ylab("Richness")+
      ggplot2::xlab(ggplot2::element_blank())+

      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

  }

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = element_blank())
    #
    # }

    return(p)


}
