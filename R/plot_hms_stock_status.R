#' plot hms_stock_status
#'
#' Kobe plot of F/Fmsy vs B/Bmsy for Highly Migratory Species (HMS) stocks.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic" default, same for both)
#'
#' @return list of 2 items
#'
#' \item{p}{ggplot object}
#' \item{unknown}{data frame listing stocks with unklnown status}
#'
#' @export
#'

plot_hms_stock_status <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

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
  y.max <- 5
  x.max <- 2
  #Get data, spread for plotting, and filter
  # for now include legend because it has 2 species not in table??
  unknownp <- data.frame(text = c("Unknown Status", "ATL SBH", "SPF", "WA BFT"),
                        x = rep(0.9*x.max,4), y = seq(0.88*y.max,3.5,-0.3))




  stock_status<-ecodata::hms_stock_status |>
    #tidyr::spread(.,Var,Value) |>
    tidyr::separate(Var, c("species_abr", "spp", "Var"), ":") |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    dplyr::group_by(spp) |>
    dplyr::filter(!species_abr == "------",
                  !species_abr == "-----") |>
    dplyr::mutate(score = dplyr::case_when(
      #(B.Bmsy <0.5) ~"a",
      #(B.Bmsy == 0.5) ~"a",
      #(B.Bmsy <1) ~"a",
      #(B.Bmsy == 1) ~"a",
      #(F.Fmsy == 1) ~ "a",
      #(F.Fmsy >1) ~ "a",
      #(F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
      (F.Fmsy > 1 | B.Bmsy < 1) ~ "b",
      (F.Fmsy < 1 & B.Bmsy > 1) ~ "a"))

  unknown <- stock_status |>
    dplyr::filter(is.na(F.Fmsy) | is.na(B.Bmsy)) |>
    dplyr::select(spp,F.Fmsy,B.Bmsy)


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- stock_status |>
    ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dotted", color = "grey60")+
    #ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60")+
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
    ggplot2::geom_point(ggplot2::aes(x = B.Bmsy,
                            y = F.Fmsy,
                            color = stock_status$score)) +
    ggrepel::geom_text_repel(ggplot2::aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                                 y = F.Fmsy,
                                 label = species_abr,
                                 color = stock_status$score), show.legend = FALSE,
                             nudge_y = -0.01, nudge_x = 0.05) +
    ggplot2::ylim(0,y.max) +
    ggplot2::xlim(0,x.max*1.1) +
    ggplot2::geom_text(data = unknownp, ggplot2::aes(x = x, y = y, label = text), #Custom legend for unknown stock status
                       size = c(4.75,rep(4,3))) +
    ggplot2::annotate(geom="text", x=0.43, y=5, label="ATL SBN (F/Fmsy = 22.5)",
                      color="#D95F02")+
    ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
                                breaks = stock_status$score) +
    ggplot2::xlab(expression(~B/B[msy])) +
    ggplot2::ylab(expression(~F/F[msy])) +
    ggplot2::guides(color = FALSE) +
    ecodata::theme_ts()

  # Temporarily block plots as HMS reviews indicator
  p <- "This indicator is under review."
  return(p)

  #return(list(p=p,unknown=unknown))
}

attr(plot_hms_stock_status,"report") <- c("MidAtlantic","NewEngland")

  # Paste commented original plot code chunk for reference
  # y.max <- 5
  # x.max <- 2
  # #Get data, spread for plotting, and filter
  # unknown <- data.frame(text = c("Unknown Status", "ATL SBH", "SPF", "WA BFT"),
  #                       x = rep(0.9*x.max,4), y = seq(0.88*y.max,3.5,-0.3))
  #
  #
  #
  # stock_status<-ecodata::hms_stock_status %>%
  #   #tidyr::spread(.,Var,Value) %>%
  #   tidyr::separate(Var, c("species_abr", "spp", "Var"), ":") %>%
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
  #   dplyr::group_by(spp) %>%
  #   dplyr::filter(!species_abr == "------",
  #                 !species_abr == "-----") %>%
  #   dplyr::mutate(score = case_when(
  #     (B.Bmsy <0.5) ~"a",
  #     (B.Bmsy == 0.5) ~"a",
  #     (F.Fmsy == 1) ~ "a",
  #     (F.Fmsy >1) ~ "a",
  #     (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
  #     (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"))
  # #Plot constants
  #
  #
  # #Plotting code
  # ggplot2::ggplot(data = stock_status) +
  #   ggplot2::geom_vline(xintercept = 1, linetype = "dotted", color = "grey60")+
  #   ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60")+
  #   ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  #   ggplot2::geom_point(aes(x = B.Bmsy,
  #                           y = F.Fmsy,
  #                           color = stock_status$score)) +
  #   ggrepel::geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
  #                                y = F.Fmsy,
  #                                label = species_abr,
  #                                color = stock_status$score), show.legend = FALSE,
  #                            nudge_y = -0.01, nudge_x = 0.05) +
  #   ggplot2::ylim(0,y.max) +
  #   ggplot2::xlim(0,x.max*1.1) +
  #   ggplot2::geom_text(data = unknown, aes(x = x, y = y, label = text), #Custom legend for unknown stock status
  #                      size = c(4.75,rep(4,3))) +
  #   ggplot2::annotate(geom="text", x=0.43, y=5, label="ATL SBN (F/Fmsy = 22.5)",
  #                     color="#1B9E77")+
  #   ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
  #                               breaks = stock_status$score) +
  #   ggplot2::xlab(expression(~B/B[msy])) +
  #   ggplot2::ylab(expression(~F/F[msy])) +
  #   ggplot2::guides(color = FALSE) +
  #   ecodata::theme_ts()
  #

