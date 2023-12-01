#' plot abc.acl
#'
#' Plots stacked bar of quotas by fishery
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_abc.acl <- function(shadedRegion = shadedRegion,
                         report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE") #unique to abc.acl dataset
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # EPUs are Councils here, MAB = MAFMC and NE = NEFMC
  # Councils have different data structures
  # MAB has Fishery and Catch or Quota
  # NE has FMP_Species - Region_Catch ABC ACL or TAL
  # splitting code by report for now

  if (report == "MidAtlantic") {

    ABCs <- ecodata::abc.acl |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") |>
      dplyr::filter(Var == "Quota") |>
      dplyr::mutate(Fishery = gsub("Commercial", "C", Fishery),
                    Fishery = gsub("Recreational", "R", Fishery)) |>
      dplyr::group_by(Fishery, Time) |>
      dplyr::summarise(Value = sum(Value))

  }

  if (report == "NewEngland") {

    ABCs <- ecodata::abc.acl |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(col = Var, into = c("FMP", "Fishery", "Var"), sep = "_") |>
      dplyr::filter(Var == "ABC") |>
      dplyr::group_by(Fishery, Time) |>
      dplyr::summarise(Value = sum(Value))

  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <-  ABCs |>
    ggplot2::ggplot()+
    ggplot2::geom_bar(ggplot2::aes( y = Value, x = Time, fill = Fishery), stat="identity", position = "stack" )+
    ggplot2::ggtitle("ABC or ACL for Managed Species")+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),
                   legend.key.height = ggplot2::unit(2, "mm"))+
    ggplot2::ylab("ABC or ACL")+
    ggplot2::xlab(ggplot2::element_blank())+
    ecodata::theme_ts()+
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=1))+
    ecodata::theme_title()


  return(p)

  # Paste commented original plot code chunk for reference
  #
  # mean<- ecodata::abc.acl %>%
  # ecodata::abc.acl |>
  #   dplyr::filter(EPU == "MAB") |>
  #   tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") |>
  #   dplyr::filter(Var == "Quota") |>
  #   dplyr::mutate(Fishery = gsub("Commercial", "C", Fishery),
  #                 Fishery = gsub("Recreational", "R", Fishery)) |>
  #   dplyr::group_by(Fishery, Time) |>
  #   dplyr::summarise(Value = sum(Value)) |>
  #   ggplot2::ggplot()+
  #   ggplot2::geom_bar(ggplot2::aes( y = Value, x = Time, fill = Fishery), stat="identity", position = "stack" )+
  #   ggplot2::ggtitle("ABC or ACL for MAFMC Managed Species")+
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 8),
  #                  legend.key.height = ggplot2::unit(2, "mm"))+
  #   ggplot2::ylab("ABC or ACL")+
  #   ggplot2::xlab(ggplot2::element_blank())+
  #   ecodata::theme_ts()+
  #   ggplot2::guides(fill=ggplot2::guide_legend(ncol=2))+
  #   ecodata::theme_title()
  #

}
