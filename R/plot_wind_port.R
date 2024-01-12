#' plot revenue by port from wind energy
#'
#' plot wind_port data
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_wind_port <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

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
   fix <- ecodata::wind_port |>
     dplyr::filter(EPU %in% filterEPUs)
   fix <- tidyr::pivot_wider(fix,names_from = Var, values_from = Value) |>
     dplyr::mutate(ordering = WEA_MAX,
                   City = paste0(City, State),
                   perc_dif =  c(perc_MAX - perc_MIN),
                   TOT_MAX = c(100 - perc_dif - perc_MIN))
   fix <- tidyr::pivot_longer(fix,cols = c(perc_MIN,  perc_dif, TOT_MAX), names_to="Var", values_to = "Value") |>
     dplyr::arrange(ordering) |>
     dplyr::mutate(City = factor(City, levels = unique(City))) |>
     dplyr::filter(!Var %in% c("EJ","Gent","WEA_MAX")) |>
     dplyr::mutate(Var = dplyr::recode(Var,"perc_MIN"= "WEA Revenue" ,
                                "perc_dif" ="WEA Revenue Range",
                                "TOT_MAX" = "Non-WEA Revenue"),
                   Var = factor(Var, levels = c("Non-WEA Revenue",
                                                "WEA Revenue Range",
                                                "WEA Revenue")))

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_bar(ggplot2::aes(x = Value,y = City, fill=Var),stat="identity")+
    ggplot2::scale_fill_brewer()+
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.box="vertical", legend.margin=ggplot2::margin())+
    #ggplot2::geom_point(data = df.symbol, ggplot2::aes(x = symbol,y = City, shape = Variable)) +
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::ggtitle(paste0(report,": Port Revenue from Wind Energy Area"))+
    ggplot2::xlab(expression("Port Revenue (%)"))+
    ggplot2::ylab(ggplot2::element_blank())+
    ecodata::theme_ts()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)

}

attr(plot_wind_port,"report") <- c("MidAtlantic","NewEngland")
