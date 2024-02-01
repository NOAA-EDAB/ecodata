#' plot forage index
#'
#' Description should be here. This needs to be reworked to uncouple GB and GOM
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Forage biomass index by region, or coastwide center of gravity ("index", "cog")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_forage_index <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "index") {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  if (varName == "index") {

    if (report == "MidAtlantic") {
      filterEPUs <- c("MAB")
    } else {
      filterEPUs <- c("GB", "GOM")
    }

    fix<- ecodata::forage_index |>
      dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate",
                               "Spring Forage Fish Biomass Estimate"),
                    EPU %in% filterEPUs) |>
      dplyr::group_by(EPU) |>
      dplyr::summarise(max = max(Value))

    p <- ecodata::forage_index |>
      dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate",
                               "Fall Forage Fish Biomass Estimate SE",
                               "Spring Forage Fish Biomass Estimate",
                               "Spring Forage Fish Biomass Estimate SE"),
                    EPU %in% filterEPUs) |>
      dplyr::group_by(EPU) |>
      tidyr::separate(Var, into = c("Season", "A", "B", "C", "D", "Var")) |>
      dplyr::mutate(Var = tidyr::replace_na(Var, "Mean")) |> #,
      #max = as.numeric(Value)) |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::left_join(fix) |>
      dplyr::mutate(#Value = Value/resca,
        Mean = as.numeric(Mean),
        #max = as.numeric(Value),
        Mean = Mean/max,
        SE = SE/max,
        Upper = Mean + SE,
        Lower = Mean - SE) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Mean, group = Season))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::ggtitle("")+
      ggplot2::ylab(expression("Relative forage biomass"))+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::facet_wrap(.~EPU)+
      ecodata::geom_gls()+
      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }
  }

  if (varName == "cog"){

    p <- ecodata::forage_index |>
      dplyr::filter(Var %in% c("Fall Eastward Forage Fish Center of Gravity",
                               "Fall Eastward Forage Fish Center of Gravity SE",
                               "Fall Northward Forage Fish Center of Gravity",
                               "Fall Northward Forage Fish Center of Gravity SE",
                               "Spring Eastward Forage Fish Center of Gravity",
                               "Spring Eastward Forage Fish Center of Gravity SE",
                               "Spring Northward Forage Fish Center of Gravity",
                               "Spring Northward Forage Fish Center of Gravity SE")) |>
      tidyr::separate(Var, into = c("Season", "Direction", "A", "B", "C", "D", "E", "Var")) |>
      dplyr::mutate(Var = tidyr::replace_na(Var, "Mean")) |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::mutate(Mean = as.numeric(Mean),
                    Upper = Mean + SE,
                    Lower = Mean - SE) |>
      ggplot2::ggplot(ggplot2::aes(x = Time, y = Mean, group = Season))+
      ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                        ymin = -Inf, ymax = Inf) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.3)+ #
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::ggtitle("")+
      ggplot2::ylab(expression("Forage Center of Gravity, km"))+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::facet_wrap(~Direction, scales = "free_y")+ #Season
      ecodata::geom_gls()+
      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

  }

  return(p)

}

attr(plot_forage_index,"report") <- c("MidAtlantic","NewEngland")
attr(plot_forage_index, "varName") <- c("index", "cog")
  # ecodata::forage_index |>
  #   dplyr::filter(Var %in% c("Fall Forage Fish Biomass Estimate",
  #                            "Fall Forage Fish Biomass Estimate SE",
  #                            "Spring Forage Fish Biomass Estimate",
  #                            "Spring Forage Fish Biomass Estimate SE"),
  #                 EPU == "MAB") |>
  #   tidyr::separate(Var, into = c("Season", "A", "B", "C", "D", "Var")) |>
  #   dplyr::mutate(Var = replace_na(Var, "Mean"),
  #                 max = as.numeric(resca)) |>
  #   tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  #   dplyr::mutate(#Value = Value/resca,
  #     Mean = as.numeric(Mean),
  #     Mean = Mean/max,
  #     SE = SE/max,
  #     Upper = Mean + SE,
  #     Lower = Mean - SE) |>
  #   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
  #   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
  #                     xmin = x.shade.min , xmax = x.shade.max,
  #                     ymin = -Inf, ymax = Inf) +
  #   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
  #   ggplot2::geom_point()+
  #   ggplot2::geom_line()+
  #   ggplot2::ggtitle("Forage Biomass Index")+
  #   ggplot2::ylab(expression("Relative forage biomass"))+
  #   ggplot2::xlab(element_blank())+
  #   ecodata::geom_gls()+
  #   ecodata::theme_ts()+
  #   ecodata::theme_title()
  #
  #
