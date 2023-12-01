#' plot abc.acl
#'
#' Plots realized catch versus quota by fishery
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
  mean<- ecodata::abc.acl |>
    dplyr::filter(EPU == filterEPUs) |>
    tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") |>
    dplyr::select(Time, Fishery, Var, Value) |>
    dplyr::group_by(Time, Fishery) |>
    tidyr::pivot_wider(names_from = Var, values_from = Value) |>
    # dplyr::rename("Quota" = " Quota",
    #               "Catch" = " Catch") |>
    dplyr::mutate(Value = Catch/Quota,
                  Time = as.character(Time)) |>
    filter(!Value == "NA",
           !Fishery == "Chub Mackerel") |>
    dplyr::group_by(Time) |>
    dplyr::summarise(val = mean(Value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Time = as.numeric(Time))



  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- ecodata::dataset |>
    dplyr::filter(Var %in% c("..."),
                                  EPU == "...") |>
    #... more dataset wrangling as necessary |>
    dplyr::left_join(fix) |>
    dplyr::mutate(#
      Value = Value,
      #Mean = as.numeric(Mean),
      #max = as.numeric(Value),
      #Mean = Mean/max,
      #SE = SE/max,
      Upper = Mean + SE,
      Lower = Mean - SE) |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper), alpha = 0.5)+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::ggtitle("")+
    ggplot2::ylab(expression("Indicator (units)"))+
    ggplot2::xlab(element_blank())+
    ggplot2::facet_wrap(.~EPU)+
    ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = element_blank())

    }

    return(p)

  # Paste commented original plot code chunk for reference
  #
  mean<- ecodata::abc.acl %>%
    #group_by(Time) %>%
    tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
    pivot_wider(names_from = Var, values_from = Value) %>%
    dplyr::rename("abc.acl" = " ABC/ACL",
                  "Catch" = " Catch") %>%
    dplyr::mutate(Value = Catch/abc.acl,
                  Time = as.character(Time)) %>%
    filter(!Value == "NA",
           !Fishery == "Chub mackerel ") %>%
    dplyr::group_by(Time) %>%
    dplyr::summarise(val = mean(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Time = as.numeric(Time))

  ecodata::abc.acl %>%
    #group_by(Time) %>%
    tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "-") %>%
    pivot_wider(names_from = Var, values_from = Value) %>%
    dplyr::rename("abc.acl" = " ABC/ACL",
                  "Catch" = " Catch") %>%
    dplyr::mutate(Value = Catch/abc.acl,
                  Time = as.numeric(Time)) %>%
    filter(!Value == "NA",
           !Fishery == "Chub mackerel ") %>%
    ggplot2::ggplot()+
    #geom_boxplot()+
    geom_point(aes(x = Time, y = Value))+
    geom_point(data = mean, aes(x = Time, y = val), color = "red")+
    geom_line(data = mean, aes(x = Time, y = val), color = "red")+
    geom_hline(yintercept = 1, linetype='dashed', col = 'gray')+
    ggplot2::ggtitle("MAFMC Catch per ABC/ACL")+
    ggplot2::ylab(expression("Ratio of Catch to ABC/ACL"))+
    ggplot2::theme(legend.title = element_blank())+
    ggplot2::xlab(element_blank())+
    ecodata::theme_ts()+
    ecodata::theme_title()
  #
  #

}
