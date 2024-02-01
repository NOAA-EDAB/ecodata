#' plot stock_Status
#'
#' Kobe plots of regional stock status
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return list of 2 items
#'
#' \item{p}{ggplot object}
#' \item{unknown}{data frame listing stocks with unklnown status}
#'
#'
#' @export
#'

plot_stock_status <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    councils <- c("MAFMC","Both")
  } else {
    councils <- c("NEFMC","Both")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
   fix<- ecodata::stock_status |>
     dplyr::mutate(Code = dplyr::recode(Code, "Dogfish" = "Sp. Dogfish" ),
                   Code = dplyr::recode(Code, "Mackerel" = "At. Mackerel"))
   fix <- tidyr::pivot_wider(fix,names_from=Var,values_from=Value) |>
     dplyr::filter(Council %in% councils) |>
     (\(.) { . ->> unfiltered})() |>
     dplyr::group_by(Stock) |>
     dplyr::mutate(score = dplyr::case_when(
       (F.Fmsy < 1 & B.Bmsy > 1.0) ~ "a",
       (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
       (F.Fmsy > 1 | B.Bmsy < 0.5) ~ "c",)) |>
     dplyr::mutate(Council = dplyr::recode(Council, "Both" = "NEFMC/MAFMC")) |>
     dplyr::filter(!(is.na(F.Fmsy) | is.na(B.Bmsy)))

   unknown <- unfiltered |>
     dplyr::filter(is.na(F.Fmsy) | is.na(B.Bmsy)) |>
     dplyr::select(Stock,F.Fmsy,B.Bmsy)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot()+
    ggplot2::geom_vline(xintercept = 1, linetype = "dotted")+
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed")+
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(x = B.Bmsy,
                            y = F.Fmsy,
                            shape = Council,
                            color = score)) +
    ggrepel::geom_text_repel(ggplot2::aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                                 y = F.Fmsy,
                                 label = Code,
                                 color = score),
                             show.legend = FALSE, nudge_y = -0.01, nudge_x = 0.05) +
    ggplot2::scale_color_manual(values = c("c"="#D95F02",
                                           "b"="#7570B3",
                                           "a" = "#1B9E77")) +
    # ggplot2::ylim(0,y.max) +
    # ggplot2::xlim(0,x.max) +
    # ggplot2::geom_text(data = unknown, ggplot2::aes(x = x-0.5, y = y+0.2, label = text), #Custom legend for unknown stock status
    #                    size = c(4.75,rep(4,6))) +
    ggplot2::xlab(expression(~B/B[msy])) +
    ggplot2::ylab(expression(~F/F[msy])) +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle(paste0(report,": stock status"))+
    ecodata::theme_ts()+
    ecodata::theme_title()

   # # optional code for New England specific (2 panel) formatting
   #  if (report == "NewEngland") {
   #    p <- p +
   #      ggplot2::theme(legend.position = "bottom",
   #                     legend.title = ggplot2::element_blank())
   #
   #  }

    return(list(p=p,unknown=unknown))
}


attr(plot_stock_status,"report") <- c("MidAtlantic","NewEngland")
