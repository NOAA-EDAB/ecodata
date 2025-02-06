#' plot benthos index
#'
#' Description should be here. This needs to be reworked to uncouple GB and GOM
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Benthos group name ("Macrobenthos", "Megabenthos")
#' @param plottype Character string. Benthos biomass index by region, or coastwide center of gravity ("index", "cog")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_benthos_index <- function(shadedRegion = NULL,
                              report="MidAtlantic",
                              varName = "Macrobenthos",
                              plottype = "index",
                              n = 0) {

  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # convert to leading capital
  varName <- stringr::str_to_sentence(varName)
  if (!(varName %in% c("Megabenthos","Macrobenthos"))){
    stop("varName should be either Megabenthos or Macrobenthos")
  }


  if (plottype == "index") {

    if (report == "MidAtlantic") {
      filterEPUs <- c("MAB")
    } else {
      filterEPUs <- c("GB", "GOM")
    }

    fix<- ecodata::benthos_index |>
      dplyr::filter(Var %in% c(paste("Fall",varName,"Biomass Index Estimate"),
                               paste("Spring",varName,"Megabenthos Biomass Index Estimate")),
                    EPU %in% filterEPUs) |>
      dplyr::group_by(EPU) |>
      dplyr::summarise(max = max(Value))

    p <- ecodata::benthos_index |>
      dplyr::filter(Var %in% c(paste("Fall",varName,"Biomass Index Estimate"),
                               paste("Fall",varName,"Biomass Index Estimate SE"),
                               paste("Spring",varName,"Biomass Index Estimate"),
                               paste("Spring",varName,"Biomass Index Estimate SE")),
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
      ggplot2::ylab(paste("Relative",varName,"Biomass"))+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::facet_wrap(.~EPU)+
      ecodata::geom_gls()+
      ecodata::geom_lm(n=n)+
      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

    if (report == "NewEngland") {
      p <- p +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())

    }
  }

  if (plottype == "cog"){

    p <- ecodata::benthos_index |>
      dplyr::filter(Var %in% c(paste("Fall",varName,"Eastward Center of Gravity"),
                               paste("Fall",varName,"Eastward Center of Gravity SE"),
                               paste("Fall",varName,"Northward Center of Gravity"),
                               paste("Fall",varName,"Northward Center of Gravity SE"),
                               paste("Spring",varName,"Eastward Center of Gravity"),
                               paste("Spring",varName,"Eastward Center of Gravity SE"),
                               paste("Spring",varName,"Northward Center of Gravity"),
                               paste("Spring",varName,"Northward Center of Gravity SE"))) |>
      tidyr::separate(Var, into = c("Season", "A", "Direction", "B", "C", "D", "Var")) |>
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
      ggplot2::ylab(paste(varName," Center of Gravity, km"))+
      ggplot2::xlab(ggplot2::element_blank())+
      ggplot2::facet_wrap(~Direction, scales = "free_y")+ #Season
      ecodata::geom_gls()+
      ecodata::geom_lm(n=n)+
      ecodata::theme_ts()+
      ecodata::theme_facet()+
      ecodata::theme_title()

  }

  return(p)

}

attr(plot_benthos_index,"report") <- c("MidAtlantic","NewEngland")
attr(plot_benthos_index, "varName") <- c("Megabenthos", "Macrobenthos")
attr(plot_benthos_index, "plottype") <- c("index", "cog")
