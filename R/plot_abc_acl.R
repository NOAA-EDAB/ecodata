#' plot abc_acl
#'
#' Plots either stacked bar of quotas/ABCs by fishery ("Stacked" plottype option)
#' or point comparison of catch with quota/ABC by fishery ("Catch" plottype option)
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param plottype Character string. Which plot ("Stacked", "Catch")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_abc_acl <- function(
    shadedRegion = NULL,
    report = "MidAtlantic",
    plottype = "Stacked"
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("NE") #unique to abc_acl dataset
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below

  # EPUs are Councils here, MAB = MAFMC and NE = NEFMC
  # Councils have different Var data structures
  # MAB has Fishery and Catch or Quota
  # NE has FMP_Species - Region_Catch ABC ACL or TAL
  # splitting code by report for now

  if (report == "MidAtlantic") {
    ABCs <- ecodata::abc_acl |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") |>
      dplyr::filter(Var == "Quota") |>
      dplyr::mutate(
        Fishery = gsub("Commercial", "C", Fishery),
        Fishery = gsub("Recreational", "R", Fishery)
      ) |>
      dplyr::group_by(Fishery, Time) |>
      dplyr::summarise(Value = sum(Value), .groups = "drop")

    CatchABC <- ecodata::abc_acl |>
      unique() |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(col = Var, into = c("Fishery", "Var"), sep = "_") |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      #tidyr::separate(Catch, into = c("Catch", "X"), sep = ",") %>%
      dplyr::mutate(
        Catch = as.numeric(stringr::str_extract(Catch, pattern = "\\d+")),
        Quota = as.numeric(stringr::str_extract(Quota, pattern = "\\d+")),
        Value = Catch / Quota #,
        #Time = as.character(Time)
      ) |>
      dplyr::filter(!is.na(Value))

    meanCatchABC <- CatchABC |>
      dplyr::group_by(Time) |>
      dplyr::summarise(val = mean(Value), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::mutate(Time = as.numeric(Time))
  }

  if (report == "NewEngland") {
    ABCs <- ecodata::abc_acl |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(
        col = Var,
        into = c("FMP", "Fishery", "Var"),
        sep = "_"
      ) |>
      dplyr::filter(Var == "ABC") |>
      dplyr::group_by(Fishery, Time) |>
      dplyr::summarise(Value = sum(Value), .groups = "drop")

    CatchABC <- ecodata::abc_acl |>
      unique() |>
      dplyr::filter(EPU == filterEPUs) |>
      tidyr::separate(
        col = Var,
        into = c("FMP", "Fishery", "Var"),
        sep = "_"
      ) |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      #tidyr::separate(Catch, into = c("Catch", "X"), sep = ",") %>%
      dplyr::mutate(
        Catch = as.numeric(stringr::str_extract(Catch, pattern = "\\d+")),
        Quota = as.numeric(stringr::str_extract(ABC, pattern = "\\d+")),
        Value = Catch / ABC #,
        #Time = as.character(Time)
      ) |>
      dplyr::filter(!is.na(Value))

    meanCatchABC <- CatchABC |>
      dplyr::group_by(Time) |>
      dplyr::summarise(val = mean(Value), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::mutate(Time = as.numeric(Time))
  }

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  if (plottype == "Stacked") {
    # p <- ABCs |>
    #   ggplot2::ggplot() +
    #   ggplot2::geom_bar(
    #     ggplot2::aes(y = Value, x = Time, fill = Fishery),
    #     stat = "identity",
    #     position = "stack",
    #     color = "black"
    #   ) +
    #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    #   ggplot2::ggtitle("ABC or ACL for Managed Species") +
    #   ggplot2::theme(
    #     legend.text = ggplot2::element_text(size = 8),
    #     legend.key.height = ggplot2::unit(2, "mm")
    #   ) +
    #   ggplot2::ylab("ABC or ACL, metric tons") +
    #   ggplot2::xlab(ggplot2::element_blank()) +
    #   ecodata::theme_ts() +
    #   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    #   ecodata::theme_title()

    species <- ABCs |>
      dplyr::mutate(Fishery = stringr::str_wrap(Fishery, width = 10)) |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(y = Value, x = Time, fill = Fishery),
        stat = "identity",
        position = "stack"
      ) +
      ggplot2::facet_wrap(~Fishery, ncol = 3, strip.position = "right") +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::ggtitle("ABC or ACL for Managed Species") +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = 8),
        legend.key.height = ggplot2::unit(2, "mm"),
        legend.position = "none"
      ) +
      ggplot2::ylab("ABC or ACL, metric tons") +
      ggplot2::xlab(ggplot2::element_blank()) +
      ecodata::theme_ts() +
      # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
      ecodata::theme_title()

    total <- ABCs |>
      dplyr::group_by(Time) |>
      dplyr::summarise(Total = sum(Value)) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Total)) +
      ggplot2::geom_bar(
        stat = "identity",
        fill = "lightblue"
      ) +
      ggplot2::ggtitle("Combined ABC or ACL for Managed Species") +
      ggplot2::ylab("ABC or ACL, metric tons") +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ecodata::theme_ts() +
      ecodata::theme_title()

    p <- ggpubr::ggarrange(
      total,
      species,
      ncol = 1,
      nrow = 2,
      heights = c(1, 3)
    )

    return(p)
  }

  if (plottype == "Catch") {
    p <- CatchABC |>
      ggplot2::ggplot() +
      #geom_boxplot()+
      ggplot2::geom_point(ggplot2::aes(x = Time, y = Value)) +
      ggplot2::geom_point(
        data = meanCatchABC,
        ggplot2::aes(x = Time, y = val),
        color = "red"
      ) +
      ggplot2::geom_line(
        data = meanCatchABC,
        ggplot2::aes(x = Time, y = val),
        color = "red"
      ) +
      ggplot2::geom_hline(yintercept = 1, linetype = 'dashed', col = 'gray') +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::ggtitle("Catch per ABC or ACL") +
      ggplot2::ylab(expression("Catch / ABC or ACL")) +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ecodata::theme_ts() +
      ecodata::theme_title()

    return(p)
  }
}

attr(plot_abc_acl, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_abc_acl, "plottype") <- c("Stacked", "Catch")
