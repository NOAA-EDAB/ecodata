#' plot north atlantic right whale abundance
#'
#' plots narw data set. calf and adult abundance. This is a shelf wide product
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("adult","calf")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_narw <- function(shadedRegion = NULL,
                      report="MidAtlantic",
                      varName = "adult") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  if (varName == "calf") {
    vtitle <- "NARW calf abundance"
    vylab <- "Number of individuals"
  } else {
    vtitle <- "NARW abundance"
    vylab <- "Number of individuals"
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  if (varName == "calf") {
    fix<- ecodata::narw |>
      dplyr::filter(Var == "Calves") |>
      dplyr::mutate(hline = mean(Value, na.rm = TRUE))
  } else {
    fix<- ecodata::narw |>
       dplyr::filter(Var != "Calves") |>
       tidyr::pivot_wider(  id_cols = c(Time,EPU,Units),
                              names_from = Var,
                              values_from = Value) |>
       dplyr::rename(Value = Median) |>
       dplyr::mutate(hline = mean(Value, na.rm = TRUE))
  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle(vtitle)+
    ggplot2::ylab(vylab)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::theme_ts()+
    ecodata::theme_title()

  if(varName == "adult"){
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower95, ymax = Upper95, x = Time), alpha = setup$shade.alpha)
  }

    if (report == "NewEngland") {
      p <- NULL
    }


    return(p)

}

attr(plot_narw,"varName") <- c("adult","calf")
attr(plot_narw,"report") <- c("MidAtlantic","NewEngland")
