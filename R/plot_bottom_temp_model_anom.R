#' plot bottom anomaly temperature time series
#'
#' plots bottom_temp_model_anom data set. Use GLORYS and PSY to supplement
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable ("seasonal", "annual")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#' @param plottype character vector. Which source data should be plotted (e.g. 'GLORYS','MOM6')
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_model_anom <- function(shadedRegion=NULL,
                                  report="MidAtlantic",
                                  varName="seasonal",
                                  EPU="MAB",
                                  plottype = 'GLORYS',
                                  n = 0) {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }


  fix <- ecodata::bottom_temp_model_anom |>
    dplyr::filter(Source %in% plottype) |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::mutate(Time = as.numeric(Time),
                  Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall|Annual"))) |>
    dplyr::filter(!Var == "NA") |>
    dplyr::mutate(Source = as.factor(Source)) |>
    dplyr::arrange(Source,Time,EPU,Var)

  fix$Var <- factor(fix$Var, levels= c("Winter","Spring","Summer","Fall", "Annual"))

  # psy <- ecodata::bottom_temp_model_anom |>
  #   dplyr::filter(Source == "PSY") |>
  #   dplyr::filter(EPU %in% filterEPUs) |>
  #   dplyr::mutate(Time = as.numeric(Time),
  #                 Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall|Annual"))) |>
  #   dplyr::filter(!Var == "NA")
  #
  # psy$Var <- factor(psy$Var, levels= c("Winter","Spring","Summer","Fall", "Annual"))
  #
  # #find last year of GLORYS then add PSY if needed
  # for (avar in unique(psy$Var)) {
  #   maxYearGlorys <- fix |>
  #     dplyr::filter(Var == avar) |>
  #     dplyr::pull(Time) |>
  #     max()
  #   addpsy <- psy |>
  #     dplyr::filter(Var == avar,Time>maxYearGlorys)
  #
  #   fix <- rbind(fix,addpsy)
  # }


    # ne_anom <- ne_anom |>
  #   dplyr::filter(Var %in% season)

  if(varName == "seasonal"){
    fix <- fix |>
      dplyr::filter(Var != "Annual")

    ylabvar <-expression("Bottom Temperature Anomaly (C)")
  } else if(varName == "annual"){
    fix <- fix |>
      dplyr::filter(Var == "Annual")

    ylabvar <-expression("Bottom Temperature (C)")
  }

  p <-  fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value,   color = Source)) + #color = EPU,
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::ylab(ylabvar) +
    ggplot2::xlab(ggplot2::element_blank())+
    {if(varName == "seasonal")ggplot2::geom_hline(yintercept=0,linetype=setup$hline.lty)}+
    ggplot2::ggtitle(paste0(EPU,": Bottom Temperature")) +
    ggplot2::scale_color_manual(values = c("black","indianred",'steelblue4'))+
    ggplot2::facet_wrap(Var~., scales="free_y") +
    ecodata::theme_ts() +
    ecodata::theme_facet() +
    ecodata::geom_gls(inherit.aes =T) +
    # ecodata::geom_lm(n=10)+
    # ecodata::geom_lm(ggplot2::aes(x = Time, y = Value))+
    # ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
    #                plot.title = ggplot2::element_text(size = 12))+
    ecodata::theme_title()


  return(p)
}


attr(plot_bottom_temp_model_anom,"EPU") <- c("MAB","GB","GOM")
attr(plot_bottom_temp_model_anom,"report") <- c("MidAtlantic","NewEngland")
attr(plot_bottom_temp_model_anom, "varName") <- c("seasonal", "annual")
attr(plot_bottom_temp_model_anom, "plottype") <- c("GLORYS", "MOM6")
