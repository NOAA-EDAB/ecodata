#' plot bottom anomaly temperature time series
#'
#' plots bottom_temp_comp data set. Use GLORYS and PSY to supplement
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_comp <- function(shadedRegion=NULL,
                                  report="MidAtlantic",
                                  EPU="MAB") {

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


  fix <- ecodata::bottom_temp_comp |>
    dplyr::filter((Source == "GLORYS" & Time >= 1993) | (Source == "ROMS") ) |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::mutate(Time = as.numeric(Time),
                  Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall"))) |>
    dplyr::filter(!Var == "NA")

  fix$Var <- factor(fix$Var, levels= c("Winter","Spring","Summer","Fall"))

  psy <- ecodata::bottom_temp_comp |>
    dplyr::filter(Source == "PSY") |>
    dplyr::filter(EPU %in% filterEPUs) |>
    dplyr::mutate(Time = as.numeric(Time),
                  Var = stringr::str_to_title(stringr::str_extract(Var,"Winter|Spring|Summer|Fall"))) |>
    dplyr::filter(!Var == "NA")

  psy$Var <- factor(psy$Var, levels= c("Winter","Spring","Summer","Fall"))

  #find last year of GLORYS then add PSY if needed
  for (avar in unique(psy$Var)) {
    maxYearGlorys <- fix |>
      dplyr::filter(Var == avar) |>
      dplyr::pull(Time) |>
      max()
    addpsy <- psy |>
      dplyr::filter(Var == avar,Time>maxYearGlorys)

    fix <- rbind(fix,addpsy)
  }


    # ne_anom <- ne_anom |>
  #   dplyr::filter(Var %in% season)

  p <- ggplot2::ggplot(data = fix,
                       ggplot2::aes(x = Time, y = Value, color = EPU, group = EPU)) +
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line()+
    ggplot2::geom_point(ggplot2::aes(shape = Source))+
    ggplot2::scale_shape_manual(values = c(16,1,15)) +
    ggplot2::ylim(-2,3)+
    ggplot2::ylab(expression("BT Anomaly (C)")) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::geom_hline(yintercept=0,linetype=setup$hline.lty)+
    ggplot2::ggtitle(paste0(report,": ROMS/GLORYS/PSY bottom temperature Anomaly")) +
    ggplot2::scale_color_manual(values = c("black","indianred"))+
  #  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    # ggplot2::geom_hline(aes(yintercept = hline),
    #                     size = hline.size,
    #                     alpha = hline.alpha,
    #                     linetype = hline.lty) +
    #ggplot2::facet_wrap(Var ~., ncol = 2, scales = "free_y")+
    ggplot2::facet_wrap(~Var, scales="free_y") +
    ecodata::theme_ts() +
    ecodata::theme_facet() +
    ecodata::geom_gls() +
    #ecodata::geom_lm(aes(x = Time, y = Value))+
    # ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
    #                plot.title = ggplot2::element_text(size = 12))+
    ecodata::theme_title()


  return(p)
}


attr(plot_bottom_temp_comp,"EPU") <- c("MAB","GB","GOM")
attr(plot_bottom_temp_comp,"report") <- c("MidAtlantic","NewEngland")
