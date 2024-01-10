#' plot productivity_anomaly
#'
#' plots the productivity anomaly dataset
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which variable to plot ("anomaly","assessment")
#' @param EPU Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_productivity_anomaly <- function(shadedRegion = NULL,
                                      report="MidAtlantic",
                                      varName = "anomaly",
                                      EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    if (varName == "anomaly") {
      filterEPUs <- "MAB"
    } else {
      filterEPUs <- c("MA")
    }
  } else {
    if (varName == "anomaly") {
      if (!(EPU %in% c("GB","GOM"))) {
        stop("For NewEngland the epu must be either 'GB' or 'GOM'")
      }
      filterEPUs <- EPU
    } else {
      filterEPUs <- c("NE")
    }
  }


  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  if (varName == "assessment") {
    fix<- ecodata::productivity_anomaly |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  |>
      dplyr::filter(EPU == filterEPUs,
                    Var == "rs_anom") |>
      dplyr::group_by(Time) |>
      dplyr::summarise(Total = sum(Value, na.rm = T),
                       Count = dplyr::n()) |> # SG add a count of species
      dplyr::mutate(Totalold = ifelse(Total == 0, NA, Total),
                    Total = ifelse(Count < max(Count), NA, Total)) |>
      dplyr::filter(!is.na(Total))

    prod<- ecodata::productivity_anomaly |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  |>
      dplyr::filter(EPU == filterEPUs,
                    Var == "rs_anom")

    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #
    p <-
      ggplot2::ggplot(prod, ggplot2::aes(x = Time)) +
      ggplot2::geom_bar(data = prod |> dplyr::filter(Value > 0),
                        ggplot2::aes(y = Value, fill = Stock),
                        stat = "identity") +
      ggplot2::geom_bar(data = prod |> dplyr::filter(Value < 0),
                        ggplot2::aes(y = Value, fill = Stock),
                        stat = "identity") +
      ggplot2::geom_line(data = fix, ggplot2::aes(x = Time, y = Total),
                         linewidth = 1) +
      ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
      ggplot2::xlab("") +
      ggplot2::ylab("Recruitment Anomaly") +
      ggplot2::ggtitle(paste0(filterEPUs," Recruitment Anomaly from Stock Assessments")) +
      #ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
      ecodata::theme_ts()+
      ggplot2::theme(axis.title   = ggplot2::element_text(size = 10),
                     axis.text    = ggplot2::element_text(size = 10),
                     plot.title   = ggplot2::element_text(size = 12),
                     #legend.text  = element_text(size = leg_font_size),
                     legend.title = ggplot2::element_blank())

  }

  if (varName == "anomaly") {
    bar_dat <- ecodata::productivity_anomaly |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(Var, into = c("Var", "Survey"), sep = "_")

    adjustAxes <-
      ggplot2::theme(axis.title   = ggplot2::element_text(size = 10),
                     axis.text    = ggplot2::element_text(size = 10),
                     plot.title   = ggplot2::element_text(size = 15))

    p <- plot_stackbarcpts_single(YEAR = bar_dat$Time,
                                    var2bar = bar_dat$Var,
                                    x = bar_dat$Value,
                                    titl = paste0(EPU, "from survey data"),
                                    xlab = "",
                                    ylab = "Small fish per large fish biomass (anomaly)",
                                    height = 5.5,
                                    width = 9,
                                    filt = FALSE,
                                    label = "",
                                    y.text = 10,
                                    aggregate = TRUE)



  }


  return(p)

}


#' anomaly stacked barchart. needs to be reworked
#' @noRd
plot_stackbarcpts_single <- function(YEAR, var2bar,
                                     x, xlab, ylab,
                                     titl,
                                     file_suffix,
                                     leg_font_size = 6,
                                     remove_leg = FALSE,
                                     leg_ncol = 1,
                                     wcpts = TRUE,
                                     wdashed = TRUE,
                                     height = 5.5,
                                     width = 8,
                                     filt = TRUE,
                                     label = label,
                                     y.text = y.text,
                                     aggregate = FALSE) {

  dat2bar <- data.frame(YEAR, var2bar,x)

  if (filt == TRUE){mab_species <-  list("SUMMER FLOUNDER","SCUP","BLACK SEA BASS","BLUEFISH",
                                         "NORTHERN SHORTFIN SQUID", "LONGFIN SQUID", "ATLANTIC MACKEREL",
                                         "BUTTERFISH","ATLANTIC SURFCLAM", "OCEAN QUAHOG", "TILEFISH",
                                         "BLUELINE TILEFISH","SPINY DOGFISH", "GOOSEFISH")
  dat2plot <- dat2bar |>
    tidyr::gather(variable, value, -YEAR, -var2bar) |>
    dplyr::mutate(var2bar = gsub(pattern      = "_",
                                 replacement  = " ",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl.",
                                 replacement  = "ATLANTIC",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl",
                                 replacement  = "ATLANTIC",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "NS and combined",
                                 replacement  = "",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "YT",
                                 replacement  = "Yellowtail",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " GoM",
                                 replacement  = " GOM",
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " by EPU",
                                 replacement  = "",
                                 x            = var2bar)) |>
    dplyr::filter(var2bar %in% mab_species)
  } else if (filt == FALSE){
    dat2plot <-
      dat2bar |>
      tidyr::gather(variable, value, -YEAR, -var2bar) |>
      dplyr::mutate(var2bar = gsub(pattern      = "_",
                                   replacement  = " ",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = "Atl.",
                                   replacement  = "ATLANTIC",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = "Atl",
                                   replacement  = "ATLANTIC",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = "NS and combined",
                                   replacement  = "",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = "YT",
                                   replacement  = "Yellowtail",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = " GoM",
                                   replacement  = " GOM",
                                   x            = var2bar),
                    var2bar = gsub(pattern      = " by EPU",
                                   replacement  = "",
                                   x            = var2bar))
  }
  if (aggregate){
    agg <- dat2plot |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(Total = sum(value, na.rm = T)) |>
      dplyr::mutate(Total = ifelse(Total == 0, NA, Total)) |>
      dplyr::filter(!is.na(Total))
  }

  p <-
    ggplot2::ggplot(dat2plot,
                    ggplot2::aes(x = YEAR)) +
    ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value > 0),
                      ggplot2::aes(y = value, fill = var2bar),
                      stat = "identity") +
    ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value < 0),
                      ggplot2::aes(y = value, fill = var2bar),
                      stat = "identity") +
    {if(aggregate) ggplot2::geom_line(data = agg,ggplot2::aes(x = YEAR, y = Total),
                             linewidth = 1)} +
    ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(titl) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts()+
    ggplot2::theme(axis.title   = ggplot2::element_text(size = 12),
                   axis.text    = ggplot2::element_text(size = 12),
                   plot.title   = ggplot2::element_text(size = 15),
                   legend.text  = ggplot2::element_text(size = leg_font_size),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::annotate("text", label = label, x = 1980, y = y.text,size = 8, colour = "black")



  if(remove_leg) p <- p + theme(legend.position = "none")

  return(p)
}
