#' plot chl_pp
#'
#' Plot time series of chlorophyll a (chl), primary production (pp), weekly, monthly,
#' or anomalies.
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Variable to plot ("chl","pp","size")
#' @param plottype Character string. Which plot ("weekly", "monthly", "anomaly")
#' Weekly and monthly plots are for both variables, annual anomaly plot for PP only.
#' @param year Numeric value. Optional. Year for weekly plot, defaults to max year in data.
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_chl_pp <- function(shadedRegion = NULL,
                        report="MidAtlantic",
                        varName="chl",
                        plottype="weekly",
                        year = NULL,
                        EPU = "MAB") {

  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # plot chl and pp
  if (varName != "size") {

    # optional code to wrangle ecodata object prior to plotting
    # e.g., calculate mean, max or other needed values to join below

    varabbr <- stringr::str_to_upper(varName)
    varunits <- ifelse(varName=="chl", expression("CHL (mg m"^-3*")"),
                       expression("PP (gC m"^-2*" d"^-1*")"))

    if(plottype == "monthly") {

      varfilter <- ifelse(varName=="chl", "MONTHLY_CHLOR_A_MEDIAN",
                          "MONTHLY_PPD_MEDIAN")

      out <- ecodata::chl_pp |>
        dplyr::filter(EPU %in% filterEPUs,
                      stringr::str_detect(Var, varfilter)) |>
        tidyr::separate(Time, into = c("Cat", "Time2"), sep = "_") |>
        tidyr::separate(Time2, into = c("Year", "Month"), sep = 4) |>
        dplyr::mutate(Month = plyr::mapvalues(Month, from = c("01","02","03","04","05","06",
                                                              "07","08","09","10","11","12"),
                                              to = c(month.abb))) |>
        dplyr::filter(!Value == "NA") |>
        dplyr::group_by(EPU, Month) |>
        dplyr::mutate(hline = mean(Value))

      out$Month <- factor(out$Month, levels = month.abb)

    }

    if(plottype == "anomaly") {
      pp_anom <- ecodata::chl_pp |>
        dplyr::filter(stringr::str_detect(Var, "ANNUAL_PPD_RATIO_ANOMALY"),
               EPU %in% filterEPUs) |>
        tidyr::separate(Time, into = c("Cat", "Time"), sep = "_") |>
        dplyr::mutate(hline = 1,
               Time = as.numeric(Time),
               Var = "ANNUAL_PPD_RATIO_ANOMALY")
      pp_anom$EPU <- factor(pp_anom$EPU, levels = filterEPUs)
    }

    if(plottype == "weekly") {

      varfilter <- ifelse(varName=="chl", "WEEKLY_CHLOR_A_MEDIAN",
                          "WEEKLY_PPD_MEDIAN")

      out <- ecodata::chl_pp |>
        dplyr::filter(EPU %in% filterEPUs,
                      stringr::str_detect(Var, varfilter)) |>
        tidyr::separate(Time, into = c("Cat", "Time2"), sep = "_") |>
        tidyr::separate(Time2, into = c("Year", "Month"), sep = 4) |>
        dplyr::filter(Year == ifelse(is.null(year), max(Year), year)) |>
        dplyr::group_by(EPU) |>
        dplyr::mutate(Time = 1:length(Year))

      ltm_out <- ecodata::chl_pp |>
        dplyr::filter(stringr::str_detect(Var, varfilter),
                      EPU %in% filterEPUs) |>
        tidyr::separate(Time, into = c("Cat", "Time2"), sep = "_") |>
        tidyr::separate(Time2, into = c("Year", "Week"), sep = 4) |>
        dplyr::group_by(Week) |>
        dplyr::summarise(LTM = mean(Value, na.rm = T),
                         SD = sd(Value, na.rm = T)) |>
        dplyr::mutate(Time = 1:length(Week),
                      sd.low = LTM - SD,
                      sd.high = LTM + SD) |>
        dplyr::left_join(out, by = c("Time")) |>
        dplyr::mutate(status = ifelse(Value < sd.high & Value > sd.low, "near_mean",
                                      ifelse(Value > sd.high, "high",
                                             ifelse(Value < sd.low,"low",NA))),
                      group = "PLOT") |>
        dplyr::filter(!Value == "NA")

    }


    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #
    if(plottype == "monthly") {
      p <- out |>
        ggplot2::ggplot() +
        #ecodata::geom_lm(aes(x = Year, y = Value, group = Month))+
        ggplot2::geom_point(ggplot2::aes(x = Year, y = Value, group = Month)) +
        ggplot2::geom_line(ggplot2::aes(x = Year, y = Value, group = Month)) +
        ggplot2::scale_x_discrete(name = "", breaks = seq(min(out$Year),max(out$Year),10)) +
        ggplot2::facet_wrap(EPU~Month~., ncol = 12) +
        ggplot2::ggtitle(paste0("Monthly median ",varabbr)) +
        ggplot2::ylab(varunits) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                                         group = Month),
                            linewidth = setup$hline.size,
                            alpha = setup$hline.alpha,
                            linetype = setup$hline.lty)+
        ecodata::theme_facet() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1),
                       panel.spacing = ggplot2::unit(1, "lines"),
                       plot.margin = ggplot2::unit(c(0.1, 0, 0, 0), "cm"))+
        ecodata::theme_title()

    }

    if(plottype == "anomaly") {
      p <- pp_anom |>
        ggplot2::ggplot(ggplot2::aes(x = Time, y = Value, group = Var)) +
        ggplot2::annotate("rect", fill = setup$shade.fill, alpha = setup$shade.alpha,
                 xmin = setup$x.shade.min , xmax = setup$x.shade.max,
                 ymin = -Inf, ymax = Inf) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::guides(color = "none") +
        ggplot2::ylab("Anomaly ratio") +
        ggplot2::xlab(ggplot2::element_blank())+
        ggplot2::facet_wrap(EPU~.,ncol = 2)+
        ggplot2::ggtitle("Primary production anomaly ratio") +
        #ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = c(1998, 2018))+
        #ggplot2::scale_y_continuous(limits = c(0.65,1.35)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = hline,
                       group = Var),
                   linewidth = setup$hline.size,
                   alpha = setup$hline.alpha,
                   linetype = setup$hline.lty)+
        ecodata::theme_facet() +
        ggplot2::theme(strip.text=ggplot2::element_text(hjust=0))

      p <- "This data set is currently under review"

    }

    if(plottype == "weekly") {

      titleyear <- unique(ltm_out$Year)

      p <- ltm_out |>
        ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = Time, y = LTM)) +
        ggplot2::geom_ribbon(ggplot2::aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high),
                             alpha = 0.1,
                             fill = "grey1") +
        ggplot2::geom_line(ggplot2::aes(x = Time, y = Value),
                           linewidth = 1,color = "#33a02c") +
        # ggplot2::geom_line(data = ne_pp_late, aes(x = Time, y = LTM)) +
        # ggplot2::geom_ribbon(data = ne_pp_late,aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high),
        #             alpha = 0.1,
        #             fill = "grey1") +
        # ggplot2::geom_line(data = ne_pp_late,aes(x = Time, y = Value),
        #           size = 1,color = "#33a02c", linetype = "dashed") +
        ggplot2::ggtitle(paste(varabbr, titleyear)) +
        ggplot2::guides(color = "none") +
        ggplot2::facet_wrap(EPU~., ncol = 2)+
        ggplot2::xlab("")+
        ggplot2::ylab(varunits) +
        ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                                    labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                                    expand = c(0.01,0.01)) +
        ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
        ecodata::theme_ts()+
        ecodata::theme_title()+
        ecodata::theme_facet()+
        ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines"))
    }

    # optional code for New England specific (2 panel) formatting
    # if (report == "NewEngland") {
    #   p <- p +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = ggplot2::element_blank())
    #
    # }
  } else {
    # plot phyto_size
    # which report? this may be bypassed for some figures
    if(plottype == "weekly") {
      p <- plot_phyto_size(report = report,
                           EPU = EPU)
    } else {
      p <- ""
    }
  }
  return(p)

}

attr(plot_chl_pp,"report") <- c("MidAtlantic","NewEngland")
attr(plot_chl_pp,"varName") <- c("chl","pp","size")
attr(plot_chl_pp,"plottype") <- c("weekly","monthly","anomaly")
attr(plot_chl_pp,"year") <- NULL
attr(plot_chl_pp,"EPU") <- c("MAB","GB","GOM")

