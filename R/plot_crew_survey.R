#' plot gulf stream index
#'
#' plots gsi dataset. This is a shelfwide indicator
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param varName Character string. Which Var.Group to plot (port, fishery, demographic, personal_life, history, employment_history, employment_conditions, compensation, expenses, satisfaction, perceptions)
#' @param plottype Character string. Which type of plot metric to use ('ts','bar')
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#' @export
#'

#plottype based on Var.Group
#Separate plots into barplots for proportions and timeseries for means
# unique(ecodata::crew_survey$Var.Group)

plot_crew_survey <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "port",
  plottype = "bar",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MA")
  } else {
    filterEPUs <- c("NE")
  }

  possible.types <- unique(ecodata::crew_survey$Var.Group)

  if (!(varName %in% possible.types)) {
    stop(
      "Unexpected varName. Needs to be one of: ",
      paste(na.omit(possible.types), collapse = ', ')
    )
  }

  #Isolate Var.Group and define type of plot to use
  data <- ecodata::crew_survey |>
    dplyr::filter(Var.Group == varName & EPU %in% filterEPUs) |>
    dplyr::mutate(
      plot.type = dplyr::case_when(
        grepl('proportion', Units) ~ 'bar',
        grepl('mean', Units) ~ 'ts',
        TRUE ~ NA
      )
    ) |>
    dplyr::filter(!is.na(plot.type))

  #over ride plottype argument if not a suitable combination in the data
  plottype.key <- data |>
    dplyr::distinct(Var.Group, plot.type)

  override.plottype <- ifelse(plottype %in% plottype.key$plot.type, F, T)

  if (override.plottype) {
    plottype.final <- plottype.key$plot.type[1]
    warning(
      "No variables in ",
      varName,
      " available for plot type: '",
      plottype,
      "'. Using: '",
      plottype.final,
      "' instead"
    )
  } else {
    plottype.final <- plottype
  }

  #format data depending on plottype
  if (plottype.final == 'ts') {
    data.plot <- data |>
      dplyr::filter(plot.type == 'ts') |>
      dplyr::group_by(Var.Long) |>
      dplyr::mutate(hline = mean(Value, na.rm = T))
  } else {
    data.plot <- data |>
      dplyr::filter(plot.type == 'bar')
  }

  #Generate Timeseries plots
  if (plottype.final == 'ts') {
    units.used <- unique(data.plot$Units)

    if (any(grepl(':', units.used))) {
      unit.direction <- strsplit(units.used, split = ':')[[1]][2] |>
        paste(collapse = '/')
      plot.ylab <- stringr::str_to_title(paste0('Mean (', unit.direction, ')'))
    } else {
      plot.ylab <- 'Mean'
    }

    p <- ggplot2::ggplot(
      data = data.plot,
      ggplot2::aes(x = as.numeric(Time), y = Value)
    ) +
      ggplot2::annotate(
        "rect",
        fill = setup$shade.fill,
        alpha = setup$shade.alpha,
        xmin = setup$x.shade.min,
        xmax = setup$x.shade.max,
        ymin = -Inf,
        ymax = Inf
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~Var.Long, scale = 'free_y', ncol = 1) +
      ggplot2::ylab(plot.ylab) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ecodata::geom_gls() +
      ecodata::geom_lm(n = n) +
      ecodata::theme_ts() +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline),
        linewidth = setup$hline.size,
        alpha = setup$hline.alpha,
        linetype = setup$hline.lty
      ) +
      ecodata::theme_title()
  }

  if (plottype.final == 'bar') {
    data.bar.group <- data.plot |>
      dplyr::distinct(Var.Long, Var.Answer) |>
      dplyr::group_by(Var.Long) |>
      dplyr::summarise(n.answer = dplyr::n())

    #If too many answer categories, group to top max.n + bundled other
    max.n <- 10
    if (any(data.bar.group$n.answer > max.n)) {
      data.plot <- data.plot |>
        dplyr::group_by(Var.Long) |>
        dplyr::mutate(
          Var.Answer = forcats::fct_lump_n(
            f = as.character(Var.Answer),
            n = max.n,
            w = Value,
            other_level = "Other"
          )
        ) |>
        dplyr::group_by(
          Time,
          EPU,
          Var,
          Var.Long,
          Var.Answer,
          Var.Group,
          Units,
          plot.type
        ) |>
        dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          # 1. Convert NA to "No Response" (using character conversion temporarily)
          Var.Answer = as.character(Var.Answer),
          Var.Answer = ifelse(is.na(Var.Answer), "No Response", Var.Answer),

          # 2. Convert back to factor and force "Other" and "No Response" to the end (bottom)
          Var.Answer = as.factor(Var.Answer),
          Var.Answer = forcats::fct_relevel(
            Var.Answer,
            "Other",
            "No Response",
            after = Inf
          )
        )
    }

    plot_list <- data.plot |>
      dplyr::group_split(Var.Long) |>
      purrr::map(
        ~ {
          question_name <- unique(.x$Var.Long)

          ggplot2::ggplot(
            .x,
            ggplot2::aes(x = Time, y = Value, fill = Var.Answer)
          ) +
            ggplot2::geom_col(position = "stack") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = question_name,
              x = '',
              y = "Proportion",
              fill = "Answer"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 10, face = "bold"),
              legend.position = "right",
              legend.text = ggplot2::element_text(size = 8)
            )
        }
      )

    p <- patchwork::wrap_plots(plot_list, ncol = 2)
  }

  return(p)
}


attr(plot_crew_survey, "report") <- c("MidAtlantic", "NewEngland")
attr(plot_crew_survey, "varName") <- c(
  'port',
  'fishery',
  'demographic',
  'personal_life',
  'history',
  'employment_history',
  'employment_conditions',
  'compensation',
  'expenses',
  'satisfaction',
  'perceptions'
)
attr(plot_crew_survey, 'plottype') <- c('ts', 'bar')
