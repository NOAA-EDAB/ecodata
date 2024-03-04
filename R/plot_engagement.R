#' plot engagement
#'
#' Plot reliance vs engagement by community, recreational or commercial fishery.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Fishery to plot ("Commercial","Recreational")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_engagement <- function(shadedRegion = NULL,
                            report="MidAtlantic",
                            varName="Commercial") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- setup$region_abbr
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  eng<-ecodata::engagement |>
    dplyr::filter(EPU == filterEPUs,
                  Fishery == varName) |>
    dplyr::rename("EJRating" = "EJ Rating") |>
    dplyr::mutate(EJRating = as.factor(EJRating))
  eng$EJRating <- factor(eng$EJRating, c("MedHigh to High","Medium","All Other Communities"))

  # select 3 colors from palette
  #colorpalette <- RColorBrewer::brewer.pal(4, "Dark2")[1:3]

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  eng2<-eng |>
    ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x = Eng, y = Rel, color = EJRating), size = 2)+
    #ggplot2::scale_color_manual(values = c("MedHigh to High"="red","Medium"="black","All Other Communities"="pink"))+
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",color = "black", linewidth = 0.5)+
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    ggrepel::geom_text_repel(ggplot2::aes(x = Eng, #geom_text_repel auto-jitters text around points
                                 y = Rel,
                                 label = Community,
                                 color = EJRating),
                             show.legend = FALSE, direction = "both", box.padding = 0.2, size = 3)+
    ggplot2::scale_color_manual(values = c("MedHigh to High"="#D95F02",
                                           "Medium"="#7570B3",
                                           "All Other Communities" = "#1B9E77"),
                                labels = c('MedHigh to High' = 'Medium-High to High','Medium' = 'Medium','All Other Communities'='All Other Communities')) +
    # ggplot2::scale_color_brewer(palette = c("#F8766D","#00BA38","#619CFF"),#"Dark2", #Change legend labels for clarity
    #                             breaks = eng$EJRating) +

    #ggplot2::xlim(-2,12.7)+
    #ggplot2::ylim(-1,3.0)+
    ggplot2::theme(legend.position="top",
          #legend.title = element_blank(),
         legend.background = ggplot2::element_blank()) +#,
         #legend.box.background = ggplot2::element_rect(colour = "black"))+
    ggplot2::labs(color = "EJ Vulnerability")+
    ggplot2::xlab(paste(varName, "Engagement Index")) +
    ggplot2::ylab(paste(varName, "Reliance Index")) +
    ggplot2::ggtitle(paste(setup$region, "Environmental Justice in Top", varName, "Fishing Communities"))+
    #ggplot2::guides(color = FALSE) +
    #theme_bw()
    ecodata::theme_ts()+
    ecodata::theme_title()
  #ecodata::theme_facet()


  p <- gridExtra::grid.arrange(eng2, bottom = grid::textGrob("Low <---------------------------------------------------------------------------------------------------------------------------> High",
                                                  x = 0.5, y = 1, gp = grid::gpar(fontsize = 7)),
                          left = grid::textGrob("Low <--------------------------------------------------------------------------------------> High", rot = 90,
                                          x = 1, y = 0.5, gp = grid::gpar(fontsize = 7)))

    return(p)


}

attr(plot_engagement,"report") <- c("MidAtlantic","NewEngland")
attr(plot_engagement,"varName") <- c("Commercial","Recreational")

