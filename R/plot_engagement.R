#' plot fishery engagement and community social vulnerability
#'
#' Plot population relative engagement vs engagement by community, recreational or commercial fishery.
#' Provide table of social vulnerability indicators for highly engaged communities.
#' Indicator definitions https://www.fisheries.noaa.gov/national/socioeconomics/social-indicators-supporting-information
#'
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Fishery to plot ("Commercial","Recreational")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_engagement <- function(shadedRegion = NULL,
                            report="MidAtlantic",
                            varName="Commercial") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # select social indicators by indicator group for table or shading
  indgroup <- c("personal_disruption_rank", "pop_composition_rank", "poverty_rank")

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- setup$region_abbr
  }

  # filter to fishery type for main plot
  if (varName == "Commercial"){
    eng<-ecodata::engagement |>
      #dplyr::distinct(Time, Var,  EPU, Units, .keep_all = T) |> #hack, remove later
      tidyr::separate(Var, into = c("Town", "StateVar"), sep = ", ") |> #using two steps because some towns have - in the name
      tidyr::separate(StateVar, into = c("State", "Var"), sep = "-") |> # which also seps the variable
      tidyr::unite("Town", c(Town, State), sep = ",") |>
      dplyr::mutate(Town = dplyr::recode(Town, "OTHER,VA" = "OTHER,VA (includes REEDVILLE)")) |>
      # tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::filter(EPU == filterEPUs)

    eng.ts = eng |>
      dplyr::filter(Var == 'fishing_mean_score') |>
      dplyr::mutate(label = dplyr::if_else(Time == max(Time), as.character(Town), NA_character_))

    topEng <- eng |>
      dplyr::filter(Time == max(Time)) |>
      dplyr::arrange(desc(Value)) |>
      head(n=10) |>
      dplyr::pull(Town)

    eng.ts = eng.ts |>
      dplyr::filter(Town %in% topEng)


    p = ggplot2::ggplot(data = eng.ts, ggplot2::aes(x=Time, y=Value, color=Town)) +
      ggplot2::geom_point(size=3, alpha = 0.9)+
      ggplot2::geom_path(linewidth=0.2)+
      ggplot2::ylab("Port Commercial Fishing Activity Indicator score")+
      ggplot2::xlab("Year")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_line(color = "#8ccde3",
                                            size = 0.2,
                                            linetype = 2))+
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::scale_x_continuous(breaks = c(2007, 2010, 2015, 2020,2024))+
      ggplot2::scale_alpha_discrete(range = c(0.15, 0.9)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_text(face = "bold"),
        legend.box = "horizontal"
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 4)) +
      ggplot2::ggtitle(paste(setup$region, "Port Activity in Top", varName, "Fishing Communities"))

  }

  if(varName == "Recreational"){
    eng<-ecodata::engagement |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::select(-Units) |>
      #dplyr::distinct(Time, Var,  EPU, Units, .keep_all = T) |> #hack, remove later
      tidyr::separate(Var, into = c("Town", "StateVar"), sep = ",") |>
      tidyr::separate(StateVar, into = c("State", "Var"), sep = "-") |>
      tidyr::unite("Town", c(Town, State), sep = ",") |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::filter(EPU == filterEPUs) |>
      dplyr::select(-dplyr::starts_with("Com"))


    eng.plot <- eng |>
      dplyr::filter(is.finite(RecEng)) |>
      dplyr::filter(Time == max(Time)) |>
      dplyr::select(Time, EPU, Town,
                    Eng = dplyr::ends_with("Eng"),
                    Eng_ct = dplyr::ends_with("Eng_ct"),
                    Rel = dplyr::ends_with("Rel"),
                    Rel_ct = dplyr::ends_with("Rel_ct"),
                    dplyr::all_of(indgroup))

    # find the top ~20 communities for highlighting and plotting, using top 12 Eng and Rel
    topEng <- eng.plot |>
      dplyr::select(Town, Eng) |>
      dplyr::arrange(desc(Eng)) |>
      head(n=12) |>
      dplyr::select(Town)

    topRel <- eng.plot |>
      dplyr::select(Town, Rel) |>
      dplyr::arrange(desc(Rel)) |>
      head(n=12) |>
      dplyr::select(Town)

    topTowns <- dplyr::bind_rows(topEng, topRel) |>
      dplyr::distinct() |>
      dplyr::pull(Town)

    # raw scores not currently used but here for later
    # Social <- c("personal_disruption", "pop_composition", "poverty")
    # Economic <- c("labor_force_str", "housing_characteristics")
    # Gentrification <- c("housing_disrupt", "retiree_migration", "urban_sprawl_index")

    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #

    p<-eng.plot |>
      ggplot2::ggplot(ggplot2::aes(x=Eng, y=Rel)) +
      ggplot2::geom_point(shape=1,
                          size=1.4, stroke=0.9, alpha=0.6,
                          ggplot2::aes(color=factor(Eng_ct)))+
      ggplot2::scale_color_manual(values = c("gray","skyblue","slateblue3","navy"),
                                  labels = c("low", "med", "med high", "high")) + #1,2,3,4 low-->high
      ggplot2::theme_bw()+
      #ggplot2::ylim(-5,20)+
      ggplot2::theme(text = ggplot2::element_text(size=10),
                     plot.title = ggplot2::element_text(size=9, vjust=-7.5, hjust=0.01),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "bottom")+
      ggplot2::ylab(expression(paste("Population Relative Engagement Index", italic(" (factor score)"))))+
      ggplot2::xlab(expression(paste("Engagement Index", italic(" (factor score)"))))+
      ggplot2::guides(color=ggplot2::guide_legend(title="Engagement Score Rank"))+
      ggrepel::geom_label_repel(ggplot2::aes(label = ifelse(Town %in% c(topTowns),as.character(Town),'')),
                                size=2,
                                force=2.5,
                                box.padding   = 0.5,
                                point.padding = 0,
                                segment.color = 'grey50',
                                min.segment.length = 0.01,
                                max.overlaps=Inf,
                                label.size = NA,
                                fill = ggplot2::alpha(c("white"),0.1))+
      ggplot2::ggtitle(paste(setup$region, "Engagement in Top", varName, "Fishing Communities: ",max(eng$Time)))+
      #ecodata::theme_ts()+
      ecodata::theme_title()

  }

      return(p)
}

attr(plot_engagement,"report") <- c("MidAtlantic","NewEngland")
attr(plot_engagement,"varName") <- c("Commercial","Recreational")
