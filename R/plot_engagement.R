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
#' @param plottype Character string. Which Social indicator group to tabulate ("plot","Social", "Economic", "Gentrification")
#'
#' @return list of 2 items
#'
#' \item{p}{ggplot object}
#' \item{t}{data frame listing selected social indicators for highly engaged communities}
#'
#'
#' @export
#'

plot_engagement <- function(shadedRegion = NULL,
                            report="MidAtlantic",
                            varName="Commercial",
                            plottype="plot") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

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
      tidyr::separate(Var, into = c("Town", "StateVar"), sep = ",") |> #using two steps because some towns have - in the name
      tidyr::separate(StateVar, into = c("State", "Var"), sep = "-") |> # which also seps the variable
      tidyr::unite("Town", c(Town, State), sep = ",") |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::filter(EPU == filterEPUs,
                    !is.na(ComEng)) |>
      dplyr::select(-dplyr::starts_with("Rec"))

  }

  if(varName == "Recreational"){
    eng<-ecodata::engagement |>
      #dplyr::distinct(Time, Var,  EPU, Units, .keep_all = T) |> #hack, remove later
      tidyr::separate(Var, into = c("Town", "StateVar"), sep = ",") |>
      tidyr::separate(StateVar, into = c("State", "Var"), sep = "-") |>
      tidyr::unite("Town", c(Town, State), sep = ",") |>
      tidyr::pivot_wider(names_from = Var, values_from = Value) |>
      dplyr::filter(EPU == filterEPUs,
                    !is.na(RecEng)) |>
      dplyr::select(-dplyr::starts_with("Com"))

  }

  # select social indicators by indicator group for table or shading
  if(plottype == "Social" | plottype == "plot") indgroup <- c("personal_disruption_rank", "pop_composition_rank", "poverty_rank")
  if(plottype == "Economic") indgroup <- c("labor_force_str_rank", "housing_characteristics_rank")
  if(plottype == "Gentrification") indgroup <- c("housing_disrupt_rank", "retiree_migration_rank", "urban_sprawl_index_rank")

  eng <- eng |>
    dplyr::select(Time, EPU, Town,
                  Eng = dplyr::ends_with("Eng"),
                  Eng_ct = dplyr::ends_with("Eng_ct"),
                  Rel = dplyr::ends_with("Rel"),
                  Rel_ct = dplyr::ends_with("Rel_ct"),
                  dplyr::all_of(indgroup))

  # find the top ~20 communities for highlighting and plotting, using top 12 Eng and Rel
  topEng <- eng |>
    dplyr::select(Town, Eng) |>
    dplyr::arrange(desc(Eng)) |>
    head(n=12) |>
    dplyr::select(Town)

  topRel <- eng |>
    dplyr::select(Town, Rel) |>
    dplyr::arrange(desc(Rel)) |>
    head(n=12) |>
    dplyr::select(Town)

  topTowns <- dplyr::bind_rows(topEng, topRel) |>
    dplyr::distinct()

  # raw scores not currently used but here for later
  # Social <- c("personal_disruption", "pop_composition", "poverty")
  # Economic <- c("labor_force_str", "housing_characteristics")
  # Gentrification <- c("housing_disrupt", "retiree_migration", "urban_sprawl_index")

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  p<-eng |>
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
    ggrepel::geom_label_repel(ggplot2::aes(label = ifelse(Town %in% c(unlist(topTowns)),as.character(Town),'')),
                              size=2,
                              force=2.5,
                              box.padding   = 0.5,
                              point.padding = 0,
                              segment.color = 'grey50',
                              min.segment.length = 0.01,
                              max.overlaps=Inf,
                              label.size = NA,
                              fill = ggplot2::alpha(c("white"),0.1))+
    ggplot2::ggtitle(paste(setup$region, "Engagement in Top", varName, "Fishing Communities"))+
    #ecodata::theme_ts()+
    ecodata::theme_title()

  # code for generating table of community social vulnerabilities
  t <- eng |>
    dplyr::filter(Town %in% c(unlist(topTowns))) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with('_rank'), ~
                           dplyr::case_when(
                             . ==  1~ "low",
                             . == 2 ~ "med",
                             . == 3 ~ "med high",
                             . == 4 ~ "high",
                             TRUE ~ NA_character_
                           ))) |>
    dplyr::select(Community = Town,
                  dplyr::ends_with('_rank'))

  t <- flextable::flextable(t)


    if (plottype == "plot") {
      return(p)
    } else {
      return(t)
    }

}

attr(plot_engagement,"report") <- c("MidAtlantic","NewEngland")
attr(plot_engagement,"varName") <- c("Commercial","Recreational")
attr(plot_engagement,"plottype") <- c("plot","Social","Economic","Gentrification")
