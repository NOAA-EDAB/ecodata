#' plot SST transition dates and timing
#'
#' uses ecodata::trans_dates
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("timing","length")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#' @export

plot_trans_dates <- function(shadedRegion = NULL,
                                      report="MidAtlantic",
                                      varName = "timing",
                             n = 0) {


  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  if (varName == "timing") {

    fix <- ecodata::trans_dates |>
      dplyr::filter(EPU %in% filterEPUs,
                    Var %in% c("falltrans", "sprtrans","maxday"),
                    !Value == "NA",
                    !Var == "NA",
                    !Time == "NA") |>
      dplyr::mutate(Var = dplyr::recode(Var, "falltrans"="Fall",
                                 "sprtrans" = "Spring",
                                 "maxday" = "Max"))

    mn <- fix |>
      dplyr::group_by(EPU,Var) |>
      dplyr::summarise(mn = mean(Value,na.rm = T),
                       .groups="drop")

    fix <- fix |>
      dplyr::left_join(mn,by=c("Var","EPU"))

    p <-  fix |>

      ggplot2::ggplot(ggplot2::aes(x= Time, y = Value, color = Var)) +
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ecodata::geom_gls() +
      ecodata::geom_lm(n=n)+
      ggplot2::geom_hline(ggplot2::aes(yintercept = mn),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty)+
      # ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
      #                plot.title = ggplot2::element_text(size = 12)) +
      ecodata::theme_title("") +
      ggplot2::ylab("Day of Year") +
      ggplot2::facet_wrap(.~EPU) +
      ecodata::theme_facet() +
      ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::ggtitle(paste0(report,": SST transition dates"))

  } else if (varName == "length") {
    fix <- ecodata::trans_dates |>
      dplyr::filter(EPU %in% filterEPUs,
                    Var == "sumlen",
                    !Value == "NA")
    mn <- fix |>
      dplyr::group_by(EPU,Var) |>
      dplyr::summarise(mn = mean(Value,na.rm = T),
                       .groups="drop")

    fix <- fix |>
      dplyr::left_join(mn,by=c("Var","EPU"))

    p <- fix |>
      #dplyr::filter(Var %in% season) %>%
      ggplot2::ggplot(ggplot2::aes(x= Time, y = Value))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ecodata::geom_gls() +
      ecodata::geom_lm(n=n)+
      ggplot2::geom_hline(ggplot2::aes(yintercept = mn),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty)+
      # ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
      #                plot.title = ggplot2::element_text(size = 12))+
      ecodata::theme_title()+
      ggplot2::ylab("Number of Days")+
      ggplot2::ggtitle(paste(
        "Time between spring and fall transition in",
        report
      )) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()) +
      ecodata::theme_ts()+
      ggplot2::facet_wrap(.~EPU)+
      ecodata::theme_facet()

    if (report == "NewEngland") {
      p <- p + ggplot2::facet_wrap(~EPU, nrow = 2)
    } else {
      p <- p
    }


  } else {
    stop("Transition date `timing` or `length` allowed")
  }

  return(p)
}

attr(plot_trans_dates,"varName") <- c("timing","length")
attr(plot_trans_dates,"report") <- c("MidAtlantic","NewEngland")
