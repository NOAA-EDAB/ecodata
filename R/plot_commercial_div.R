#' plot commercial_div
#'
#' Plot time series of commercial fleet diversity (fleet count, fleet revenue,
#' or species permit revenue). Mid Atlantic or New England, no EPUs.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("Fleet count",
#' "Fleet diversity in revenue", "Permit revenue species diversity")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_commercial_div <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName="Fleet count",
                              n = 0) {




  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # # which report? this may be bypassed for some figures
  # if (report == "MidAtlantic") {
  #   filterEPUs <- c("MAB")
  # } else {
  #   filterEPUs <- c("GB", "GOM")
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  comm_div <- ecodata::commercial_div |>
    dplyr::filter(EPU == setup$region_abbr) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(hline = mean(Value))

  ylabdat <- ifelse(varName=="Fleet count", expression("Count (n)"),
                    expression("Effective Shannon"))

  if(varName=="Fleet count"){
    ylim_fc <- c(min(comm_div[comm_div$Var == "Fleet count" & comm_div$EPU == setup$region_abbr ,]$Value, na.rm = TRUE) *0.95,
                 max(comm_div[comm_div$Var == "Fleet count"& comm_div$EPU == setup$region_abbr,]$Value, na.rm = TRUE) *1.05 )
  }else if(varName=="Fleet diversity in revenue"){
    ylim_fc <- c(0, max(comm_div[comm_div$Var == "Fleet diversity in revenue" & comm_div$EPU == setup$region_abbr,]$Value) *1.1 )
  }else{
    ylim_fc <- c(NA, NA)
  }

  ylim_fc <- as.numeric(ylim_fc)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- comm_div |>
    dplyr::filter(Var == varName) |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, color = Var)) +
    # ecodata::geom_lm(n=n, ggplot2::aes(x = Time, y = Value, group = Var),
    #                  alpha = setup$trend.alpha, size = setup$trend.size)+
    # geom_gls(aes(x = Time, y = Value,
    #              group = Var),
    #            alpha = trend.alpha, size = trend.size) +
    ggplot2::geom_line(linewidth = setup$lwd) +
    ggplot2::geom_point(size = setup$pcex) +
    ecodata::geom_lm(n=n) +
    #Highlight last ten years
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                      xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                      ymin = -Inf, ymax = Inf) +
    # ecodata::geom_lm(aes(x = Time, y = Value,
    #              group = Var))+
    ggplot2::ylim(ylim_fc) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_color_manual(values = "black", aesthetics = "color")+
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle(paste(setup$region, varName))+
    ggplot2::ylab(ylabdat) +
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                            color = Var),
                        size = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty) +
    ecodata::theme_ts()+
    ecodata::theme_title()
   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }

    return(p)

  # Paste commented original plot code chunk for reference
  # ecodata::dataset |>
  #   dplyr::filter(Var %in% c("..."),
  #                 EPU == "...") |>
  #   ... more dataset wrangling as necessary |>
  #   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   ggplot2::ggtitle("Title")+
  #   ggplot2::ylab(expression("Y label"))+
  #   ggplot2::xlab(element_blank())+
  #   ecodata::geom_gls()+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #

}


attr(plot_commercial_div,"varName") <- c("Fleet count","Fleet diversity in revenue", "Permit revenue species diversity")
attr(plot_commercial_div,"report") <- c("MidAtlantic","NewEngland")
