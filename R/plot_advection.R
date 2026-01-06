#' plot shelf break advection index
#'
#' plots integrated mass transport through transects along the shelf break
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable (months) to plot (3 = 'March', 4 = 'April', 5 = 'May', 6 = 'June'). Can be a vector
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_advection <- function(shadedRegion = NULL,
                     report="MidAtlantic",
                     varName = 3,
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

  if(!all(varName %in% c(3,4,5,6))){
    stop('varName must be one of 3,4,5,6 (March:June) at this time')
  }else{
    var = varName
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  fix <- ecodata::advection  |>
    tidyr::separate(Time, c('Year','Month'),sep = '\\.') |>
    dplyr::filter(Month %in% var & EPU %in% filterEPUs) |>
    dplyr::mutate(Year = as.numeric(Year),
                  Month = as.numeric(Month),
                  Month.Name = factor(month.name[Month], levels = month.name),
                  VarLong = factor(
                    dplyr::case_match(Var,
                    "T15m" ~ "<15 m",
                    "T15mto40m" ~ "15-40 m",
                    "TBlw40m" ~ ">40 m"
                    ),
                    levels = c("<15 m", "15-40 m", ">40 m")
                    )
    )

  #set plot limits based on data
  fix.mu = mean(ecodata::advection$Value,na.rm=T)
  fix.sd = sd(ecodata::advection$Value,na.rm=T)
  plot.lower = fix.mu - 1.5*fix.sd
  plot.upper = fix.mu + 1.5*fix.sd

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, color = VarLong))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::labs(color = 'Depth')+
    ggplot2::ggtitle(paste0(setup$region,": ",stringr::str_to_title(paste0(sort(unique(fix$Month.Name)), collapse =', '))))+
    ggplot2::ylab(paste0("Net Advection (",fix$Units[1],")"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::annotate('text',x = -Inf, y = Inf, label = 'On Shelf', hjust = -0.5, vjust = 2, size = setup$label.size*0.75)+
    ggplot2::annotate('text',x = -Inf, y = -Inf, label = 'Off Shelf', hjust = -0.5, vjust = -2, size = setup$label.size*0.75)+
    ggplot2::facet_wrap(~Month.Name,scale = 'free_y')+
    ecodata::geom_gls()+
    ecodata::geom_lm(n=n)+
    ecodata::theme_ts()+
    ggplot2::coord_cartesian(ylim = c(plot.lower,plot.upper))+
    ggplot2::geom_hline(yintercept = 0,
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ecodata::theme_title()

  return(p)

}


attr(plot_advection,"report") <- c("MidAtlantic","NewEngland")
attr(plot_advection,"varName") <- c(3,5,6)
