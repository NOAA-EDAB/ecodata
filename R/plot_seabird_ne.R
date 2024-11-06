#' plot New England sea bird population
#'
#' Plots seabird_ne data set.
#' Diet diversity, prey composition,
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Variable to plot ("diversity","productivity","prey")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_seabird_ne <- function(shadedRegion = NULL,
                              report="NewEngland",
                              varName = "diversity",
                            n = 0) {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    message("For Mid Atlantic seabirds see `plot_seabird_mab`")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  if (varName == "productivity") {
    fix <- ecodata::seabird_ne  |>
      dplyr::filter(stringr::str_detect(Var, "Productivity"))
    fix <- tidyr::separate(fix,Var,c("Island", "COTE", "Spp"), sep = " ") |>
      dplyr::mutate(Island = plyr::mapvalues(Island, from = c("EER","JI","MR","OGI","PINWR","SINWR","STI"),
                                             to = c("Eastern Egg Rock", "Jenny Island", "Matinicus Rock",
                                                    "Outer Green Island", "Pond Island", "Seal Island",
                                                    "Stratton Island"))) |>
      dplyr::group_by(Time) |>
      dplyr::summarise(Mean = mean(Value, na.rm = T),
                       SE = sd(Value, na.rm = T)/sqrt(dplyr::n()),
                       SD = sd(Value, na.rm = T),
                       n = dplyr::n()) |>
      dplyr::mutate(Mean = ifelse(is.na(SE),NA,Mean),
                    se.low = Mean - SE,
                    se.high = Mean + SE,
                    hline = mean(Mean, na.rm = T))

    p <- fix |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Mean))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_errorbar(ggplot2::aes(x = Time,
                                          ymin = se.low,
                                          ymax = se.high),
                             width = setup$errorbar.width) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                          color = "black",
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty) +
      ggplot2::ggtitle("Common tern productivity")+
      ggplot2::ylab(expression("Fledged chicks per nest"))+
      ggplot2::xlab(ggplot2::element_blank())+
      ecodata::geom_gls()+
      ecodata::geom_lm(n=n)+
      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

  } else if(varName == "diversity") {

    fix <- ecodata::seabird_ne  |>
      dplyr::filter(!stringr::str_detect(Var, "Productivity"),
                    !stringr::str_detect(Var, "Sum"))
    fix <- tidyr::separate(fix, Var,c("Island", "Spp"), sep = " COTE ") |>
      dplyr::group_by(Island, Time) |>
      dplyr::summarise(shannon = vegan::diversity(Value),
                       simpson = vegan::diversity(Value, index = "simpson"),
                       .groups = "drop")

    fix <- tidyr::pivot_longer(fix, cols = c(shannon,simpson),values_to = "Value", names_to = "Index") |>
      dplyr::group_by(Index) |>
      dplyr::mutate(hline = mean(Value, na.rm = T)) |>
      dplyr::filter(Index == "shannon")


    p <- fix |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~Island)+
      #ecodata::geom_lm() +
      ggplot2::ggtitle("Common tern diet diversity")+
      ggplot2::ylab(expression("Shannon Diversity")) +
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::geom_hline(ggplot2::aes(yintercept = hline),
                          linewidth = setup$hline.size,
                          alpha = setup$hline.alpha,
                          linetype = setup$hline.lty) +
      ecodata::theme_ts() +
      ecodata::theme_title()


  } else if (varName == "prey") {

    fix <- ecodata::seabird_ne |>
      dplyr::filter(!stringr::str_detect(Var, "Productivity"),
                    !stringr::str_detect(Var, "Sum"))
    fix <- tidyr::separate(fix, Var,c("Island", "Spp"), sep = "COTE") |>
      dplyr::mutate(Diet = trimws(Spp)) |>
      dplyr::group_by(Diet, Time) |>
      dplyr::summarise(Value = sum(Value, na.rm = T),
                       .groups = "drop") |>
      dplyr::group_by(Time) |>
      dplyr::mutate(Freq = Value/sum(Value, na.rm = T)) |>
      dplyr::ungroup()

    prey_freq1 <- fix |>
      dplyr::filter(Freq > 0.05) |>
      dplyr::mutate(Diet = gsub("Other Invertebrate", "Unknown Invertebrate", Diet))

    prey_freq2<- fix |>
      dplyr::filter(Freq < 0.05) |>
      dplyr::mutate(Diet = c("<5% Occurence"))

    prey_freq3<-prey_freq1 |>
      rbind(prey_freq2)

    colors<- c("grey", "#a6cee3", "#1f78b4", "#b2df8a",
               "#33a02c", "#fb9a99", "#fdbf6f",
               "#ff7f00", "#cab2d6", "#6a3d9a",
               "yellow")

    p <- ggplot2::ggplot() +
      ggplot2::geom_bar(data = prey_freq3,
                        ggplot2::aes(x = Time, y = Freq, fill = Diet),
                        stat = "identity") +
      #ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::ggtitle("Prey composition") +
      ggplot2::ylab("Proportion of prey items") +
      ggplot2::xlab(ggplot2::element_blank())+
      ecodata::theme_ts()+
      ecodata::theme_title()


  }


  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #


   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }


  if(report == "MidAtlantic"){
    p <- NULL
  }

    return(p)


}

attr(plot_seabird_ne,"report") <- c("MidAtlantic","NewEngland")
attr(plot_seabird_ne,"varName") <- c("diversity","productivity","prey")

