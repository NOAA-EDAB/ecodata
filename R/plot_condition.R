#' plots Fish Condition
#'
#' plots the condition data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU in the report ("GB", "GOM", "MAB")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_condition <- function(shadedRegion = NULL,
                           report="MidAtlantic",
                           EPU="MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB", "GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  numberOfConditions <- 5


  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

   fix<- ecodata::condition |>
     dplyr::filter(EPU == filterEPUs) |>
     dplyr::group_by(Var) |>
     dplyr::mutate(scaleCond = scale(Value,scale =T,center=T))

   # finds quantiles
   xs <- quantile(fix$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)
   # labels quantiles
   fix <- fix |>
     dplyr::mutate(category = cut(scaleCond,
                                  breaks = xs,
                                  labels = c( "Poor Condition",
                                              "Below Average",
                                              "Neutral",
                                              "Above Average",
                                              "Good Condition"),
                                  include.lowest = TRUE))

   sortNames <- fix  |>
     dplyr::filter(Time <= 2014) |>
     dplyr::group_by(Var) |>
     dplyr::summarize(total = sum(scaleCond)) |>
     dplyr::arrange(total) |>
     dplyr::mutate(Species = factor(Var, levels = unique(Var))) |>
     dplyr::pull(Species)

   fix$Var <-   factor(fix$Var, levels = sortNames)


   #See 5 scale colors for viridis:
   vir <- viridis::viridis_pal()(numberOfConditions)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = forcats::fct_rev(Var), fill = category)) +
    ggplot2::labs(fill="Quantiles of Condition") +
    ggplot2::geom_tile() +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values=vir) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(breaks=round(seq(min(fix$Time), max(fix$Time), by = numberOfConditions))) +
    ggplot2::theme(
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 8),
                   plot.title = ggplot2::element_text(size = 12),
                   legend.position = "bottom",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::ggtitle(paste0("Relative condition for species sampled in ",EPU)) +
    ggplot2::ylab(ggplot2::element_blank())+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::theme_ts()+
    ecodata::theme_title()

  if (report == "NewEngland") {
    p <- p +
      ggplot2::theme(
          legend.text = ggplot2::element_text(size = 8),
          legend.title = ggplot2::element_text(size = 8),
          axis.text.x = ggplot2::element_text(size = 8),
          axis.text.y = ggplot2::element_text(size = 6),
          plot.title = ggplot2::element_text(size = 12)
      )
  }
    else {
      p <- p +
        ggplot2::guides(fill= ggplot2::guide_legend(nrow=2,byrow=TRUE))
    }

    return(p)

}
