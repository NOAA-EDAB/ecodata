#' plot shelf break advection index
#'
#' plots integrated mass transport through transects along the shelf break
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot (top 15 m: "<15",15m-40m: "15_40",>40m: ">40")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_advection <- function(shadedRegion = NULL,
                     report="MidAtlantic",
                     varName = "<15",
                     n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE")
  }

  if (varName == "<15") {
    var <- "T15m"
  } else if(varName == "15_40"){
    var <- "T15mto40m"
  } else if(varName == ">40"){
    var <- "TBlw40m"
  } else{
    stop("varName must be one of '<15','15_40', or '>40'")
  }
  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix <- ecodata::advection  |>
    dplyr::filter(Var == var & EPU %in% filterEPUs) |>
    tidyr::separate(Time, c('Year','Var'),sep = '\\.') |>
    dplyr::mutate(Year = as.numeric(Year),
                  Var = as.numeric(Var),
                  Month.Name = factor(month.name[Var], levels = month.name))

  #check for count of obs in each year x var
  obs_check <- fix |>
    dplyr::group_by(Year, Month.Name) |>
    dplyr::tally()

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Year, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle(paste0(setup$region,": ",stringr::str_to_title(var)))+
    ggplot2::ylab(paste0("Net Advection (",fix$Units[1],")"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::annotate('text',x = -Inf, y = Inf, label = 'On Shelf', hjust = 0, vjust = 2, size = setup$label.size*0.75)+
    ggplot2::annotate('text',x = -Inf, y = -Inf, label = 'Off Shelf', hjust = 0, vjust = -2, size = setup$label.size*0.75)+
    ggplot2::facet_wrap(~Month.Name)+
    ecodata::geom_gls()+
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ggplot2::geom_hline(yintercept = 0,
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ecodata::theme_title()

  return(p)

}


attr(plot_advection,"report") <- c("MidAtlantic","NewEngland")
attr(plot_advection,"varName") <- c("<15", "15_40",">40")
