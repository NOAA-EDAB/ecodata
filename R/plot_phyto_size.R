#' plot phytoplankton by size class
#'
#' plots phyto_size data set.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param EPU Character string. Specify the EPU to plot within the report (Only affects NewEngland)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_phyto_size <- function(shadedRegion = NULL,
                      report="MidAtlantic",
                      EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    if (!(EPU %in% c("GB","GOM"))) {
      stop("For NewEngland the epu must be either 'GB' or 'GOM'")
    }
    filterEPUs <- EPU
  }

  # if (varName == "size") {
  #   vtitle <- ""
  #   vylab <- ""
  # } else {
  #   vtitle <- ""
  #   vylab <- ""
  # }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  month <- seq(as.Date("2021-01-01"),
               as.Date("2021-12-01"),
               by = "1 month")
  month_numeric <- lubridate::yday(month) / 365 * 52 + 1
  month_label <- lubridate::month(month, label = TRUE)

  phyto_year_nano<- ecodata::phyto_size |>
    dplyr::filter(EPU %in% filterEPUs,
                  Var %in% c("WEEKLY_PSC_FNANO_MEDIAN",
                             "WEEKLY_PSC_FMICRO_MEDIAN")) |>
    tidyr::pivot_wider(names_from = "Var", values_from = "Value") |>
    dplyr::mutate(nano = as.numeric(WEEKLY_PSC_FNANO_MEDIAN) +
                    as.numeric(WEEKLY_PSC_FMICRO_MEDIAN)) |>
    tidyr::separate(Time, into = c("Cat", "WEEK"), sep = "_") |>
    dplyr::mutate(year = stringr::str_sub(WEEK, 1,4),
                  wk = stringr::str_sub(WEEK, 5,6)) |>
    tidyr::pivot_longer(cols = c("nano"),
                        names_to = "Var", values_to = "Value") |>
    dplyr::filter((year == max(year)),
                  !Value == "NA") |>
    dplyr::mutate(Value = Value*100)


  phyto_year_micro<- ecodata::phyto_size |>
    dplyr::filter(EPU %in% filterEPUs,
                  Var == c("WEEKLY_PSC_FMICRO_MEDIAN")) |>
    dplyr::mutate(Value = as.numeric(Value)) |>
    tidyr::separate(Time, into = c("Cat", "WEEK"), sep = "_") |>
    dplyr::mutate(year = stringr::str_sub(WEEK, 1,4),
                  wk = stringr::str_sub(WEEK, 5,6)) |>
    dplyr::filter((year == max(year)),
                  !Value == "NA") |>
    dplyr::mutate(Value = Value*100)

  out_phyto<-  ecodata::phyto_size |>
    dplyr::filter(EPU %in% filterEPUs,
                  stringr::str_detect(Var, ("CLIMATOLOGICAL_WEEK"))) |> #,
    tidyr::separate(Time, into = c("Cat", "WEEK", "Year1", "Year2"), sep = "_") |>
    dplyr::filter(!Value == "NA",
                  !Var == "CLIMATOLOGICAL_WEEK_CHLOR_A_MEDIAN")  |>
    dplyr::mutate(Value = Value*100)

  # Normalize size class data so it adds to 1

  # Climatology median data from Kim does not add up to 1 do to variability in Kim's methods
  # We have decided to normalize these data such that they sum to 1 but remain in the same
  # relative proportions to one another. All data are normalized below to maintain consistent
  # scale across variables. ecodata will be left exactly as sent in by Kim

  # Normalize climatology medians
  climatology_norm<-out_phyto |>
    dplyr::group_by(Var) |>
    tidyr::pivot_wider(names_from = Var,
                       values_from = Value) |>
    dplyr::mutate(Total = CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN +
                          CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN +
                          CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN) |>
    dplyr::mutate(CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN = CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN/Total * 100) |>
    dplyr::mutate(CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN = CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN/Total * 100) |>
    dplyr::mutate(CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN = CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN/Total * 100) |>
    dplyr::mutate(Total = CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN +
                    CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN +
                    CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN) |>
    dplyr::select(!Total) |>
    tidyr::pivot_longer(cols = c("CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN",
                                 "CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN",
                                 "CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN"),
                        names_to = "Var",
                        values_to = "Value") |>
    dplyr::arrange(Var, by_group = T)

  # Reassign processed data to 'out_phyto' for combatibility with code below
  out_phyto<-climatology_norm

  # Normalize current year phytoplankton size classes
  fpico<-ecodata::phyto_size |>
    dplyr::filter(EPU %in% filterEPUs,
                  Var == c("WEEKLY_PSC_FPICO_MEDIAN")) |>
    dplyr::mutate(Value = as.numeric(Value)) |>
    tidyr::separate(Time, into = c("Cat", "WEEK"), sep = "_") |>
    dplyr::mutate(year = stringr::str_sub(WEEK, 1,4),
                  wk = stringr::str_sub(WEEK, 5,6)) |>
    dplyr::filter((year == max(year)),
                  !Value == "NA") |>
    tidyr::pivot_wider(names_from = Var,
                       values_from = Value) |>
    dplyr::select(WEEK, WEEKLY_PSC_FPICO_MEDIAN)

  phyto_norm<-phyto_year_nano |>
    dplyr::left_join(fpico, by = "WEEK") |>
    dplyr::mutate(Total = WEEKLY_PSC_FNANO_MEDIAN +
                          WEEKLY_PSC_FMICRO_MEDIAN +
                          WEEKLY_PSC_FPICO_MEDIAN) |>
    dplyr::mutate(WEEKLY_PSC_FNANO_MEDIAN = WEEKLY_PSC_FNANO_MEDIAN/Total) |>
    dplyr::mutate(WEEKLY_PSC_FMICRO_MEDIAN = WEEKLY_PSC_FMICRO_MEDIAN/Total) |>
    dplyr::mutate(WEEKLY_PSC_FPICO_MEDIAN = WEEKLY_PSC_FPICO_MEDIAN/Total) |>
    dplyr::mutate(Total = WEEKLY_PSC_FNANO_MEDIAN +
                    WEEKLY_PSC_FMICRO_MEDIAN +
                    WEEKLY_PSC_FPICO_MEDIAN) |>
    dplyr::select(!WEEKLY_PSC_FPICO_MEDIAN) |>
    dplyr::select(!Total) |>
    dplyr::mutate(Value = (as.numeric(WEEKLY_PSC_FNANO_MEDIAN) +
                    as.numeric(WEEKLY_PSC_FMICRO_MEDIAN)) * 100)

  fmicro<-phyto_norm |>
    dplyr::select(Cat, WEEK, WEEKLY_PSC_FMICRO_MEDIAN, EPU, Units, year, wk) |>
    tidyr::pivot_longer(cols = "WEEKLY_PSC_FMICRO_MEDIAN",
                        names_to = "Var",
                        values_to = "Value") |>
    dplyr::mutate(Value = Value*100)

  # Reassign processed data to 'phyto_year_nano' and "phyto_year_micro' for compatibility with code below
  phyto_year_micro<-fmicro
  phyto_year_nano<-phyto_norm

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- ggplot2::ggplot()+
    ggplot2::geom_area(ggplot2::aes(x=as.numeric(out_phyto$WEEK), y=out_phyto$Value,
                                    fill = factor(out_phyto$Var, c("CLIMATOLOGICAL_WEEK_PSC_FPICO_MEDIAN",
                                                                   "CLIMATOLOGICAL_WEEK_PSC_FNANO_MEDIAN",
                                                                   "CLIMATOLOGICAL_WEEK_PSC_FMICRO_MEDIAN"))), alpha=0.6)+
    ggplot2::geom_line( ggplot2::aes(x = as.numeric(phyto_year_nano$wk),
                                     y = phyto_year_nano$Value), color = "#FC8D62", linewidth = 1.5)+
    ggplot2::geom_line( ggplot2::aes(x = as.numeric(phyto_year_micro$wk),
                                     y = phyto_year_micro$Value), color = "#66C2A5", linewidth= 1.5)+

    ggplot2::ggtitle(paste0(EPU," Phytoplankton Size Class"))+
    ggplot2::ylab("Percent")+
    ecodata::theme_facet() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1),
                   panel.spacing = ggplot2::unit(.5, "lines"),
                   plot.margin = ggplot2::unit(c(0.1, 0, 0, 0), "cm"),
                   legend.position= "top")+
    ggplot2::scale_fill_manual(values=c("#8DA0CB", "#FC8D62","#66C2A5"), name = "",
                               labels = c("Picoplankton",
                                          "Nanoplankton", "Microplankton"))+
    #scale_y_continuous( name = "Phytoplankton Size Fraction", sec.axis = sec_axis(~.*2, name="Chlorophyll a (mg m^-3)"))+
    ggplot2::scale_x_continuous(breaks = month_numeric,
                                labels = month_label)+
    ggplot2::xlab(ggplot2::element_blank()) +
    ecodata::theme_title()

  # if(varName == "adult"){
  #   p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower95, ymax = Upper95, x = Time), alpha = setup$shade.alpha)
  # }



    return(p)

}
