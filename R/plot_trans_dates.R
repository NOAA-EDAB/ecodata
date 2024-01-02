#' plot SST transition dates and timing
#'
#' uses ecodata::trans_dates
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("timing","length")
#'
#' @return ggplot object
#'
#' @export

plot_trans_dates <- function(shadedRegion = shadedRegion,
                                      report="MidAtlantic",
                                      varName = "timing") {


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

    p <-  fix |>

      ggplot2::ggplot(ggplot2::aes(x= Time, y = Value, color = Var)) +
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ecodata::geom_gls() +
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
    p <- fix |>
      #dplyr::filter(Var %in% season) %>%
      ggplot2::ggplot(ggplot2::aes(x= Time, y = Value))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ecodata::geom_gls() +
      # ggplot2::theme(strip.text=ggplot2::element_text(hjust=0),
      #                plot.title = ggplot2::element_text(size = 12))+
      ecodata::theme_title()+
      ggplot2::ylab("Number of Days")+
      ggplot2::ggtitle(paste0(report,": Number of days between spring and fall transition dates")) +
      #ggplot2::xlab(ggplot2::element_blank())+
      ecodata::theme_ts()+
      ggplot2::facet_wrap(.~EPU)+
      ecodata::theme_facet()





  } else {
    stop("Transition date `timing` or `length` allowed")
  }

  return(p)
}
