#' plot mid atlantic inshore survey
#'
#' Plot mab_inshore_survey.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_mab_inshore_survey <- function(shadedRegion = NULL,
                              report="MidAtlantic") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    stop("Indicator for 'MidAtlantic' report only")
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::mab_inshore_survey |>
    tidyr::separate(Var, into = c("Var",  "Val"), sep = "-") |>
    tidyr::pivot_wider(names_from = Val, values_from = Value) |>
    dplyr::mutate(Value = as.numeric(Value),
                  CV = as.numeric(CV)) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(hline = mean(Value),
                  SD = Value * CV, #calculate SD from CV
                  upper = Value + (2*SD),
                  lower = Value - (2*SD))

  fix$Var <- factor(fix$Var,levels = c("Piscivore Spring","Piscivore Fall",
                                       "Benthivore Spring", "Benthivore Fall",
                                       "Planktivore Spring", "Planktivore Fall",
                                       "Benthos Spring", "Benthos Fall"))


    # code before adding standard deviation calculation
   #fix <- ecodata::mab_inshore_survey |>
    # dplyr::filter(EPU %in% filterEPUs,
     #              grepl("Value",Var)) |>
     #tidyr::separate(Var,into = c("Var","Trash"),sep ="-") |>
     #dplyr::select(-Trash) |>
     #dplyr::mutate(Var = as.factor(Var)) |>
     #dplyr::group_by(Var) |>
     #dplyr::mutate(hline = mean(Value))

   #fix$Var <- factor(fix$Var,levels =  c("Piscivore Spring","Piscivore Fall",
                                        #"Benthivore Spring","Benthivore Fall",
                                         #"Planktivore Spring","Planktivore Fall",
                                         #"Benthos Spring","Benthos Fall"))


   ymax <- fix |>
     tidyr::separate(Var, into = c("vars","Trash"),sep=" ") |>
     dplyr::group_by(vars) |>
     dplyr::summarise(max = max(Value,na.rm=T))

  usevars <- fix |>
    dplyr::distinct(Var) |>
    dplyr::pull()

  df2 <- data.frame(Time = 2015,Var = usevars) |>
    dplyr::mutate(vars = stringr::word(Var)) |>
    dplyr::left_join(ymax,by="vars") |>
    dplyr::select(-vars)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value))+
    ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
        xmin = setup$x.shade.min , xmax = setup$x.shade.max,
        ymin = -Inf, ymax = Inf) +
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::geom_point(data = df2,ggplot2::aes(x=Time,y=max),alpha = 0)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                     group = Var),
                        linewidth = setup$hline.size,
                        alpha = setup$hline.alpha,
                        linetype = setup$hline.lty)+
    ggplot2::geom_ribbon(data = fix,
                         ggplot2::aes(x = Time, ymin = pmax(lower,0), ymax = upper),
                         alpha = 0.5,
                         fill = "gray")+
    ggplot2::ggtitle("NEAMAP inshore BTS")+
    ggplot2::ylab(expression("Biomass (kg tow"^-1*")"))+
    ggplot2::xlab(ggplot2::element_blank())+
    ggplot2::facet_wrap(~Var,ncol=2,scales = "free_y")+
    #ecodata::geom_gls()+
    ecodata::theme_ts()+
    ecodata::theme_facet()+
    ecodata::theme_title()

   # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }

    return(p)

}

attr(plot_mab_inshore_survey,"report") <- c("MidAtlantic","NewEngland")
