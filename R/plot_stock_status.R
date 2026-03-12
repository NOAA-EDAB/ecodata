#' plot stock_Status
#'
#' Kobe plots of regional stock status
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return list of 2 items
#'
#' \item{p}{ggplot object}
#' \item{unknown}{data frame listing stocks with unklnown status}
#'
#'
#' @export
#'

plot_stock_status <- function(shadedRegion = NULL, report = "MidAtlantic") {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    councils <- c("MAFMC", "Both")
  } else {
    councils <- c("NEFMC", "Both")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::stock_status |>
    dplyr::mutate(
      Code = dplyr::recode(Code, "Dogfish" = "Sp. Dogfish"),
      Code = dplyr::recode(Code, "Mackerel" = "At. Mackerel")
    ) |>
    # add codes for stocks missing them
    dplyr::mutate(
      Code = dplyr::case_when(
        Stock == "Atlantic salmon - Gulf of Maine" ~ "Salmon",
        Stock == "Atlantic wolffish - Gulf of Maine / Georges Bank" ~
          "Wolffish",
        TRUE ~ Code
      )
    )
  fix <- tidyr::pivot_wider(fix, names_from = Var, values_from = Value) |>
    dplyr::filter(Council %in% councils) |>
    (\(.) {
      . ->> unfiltered
    })() |>
    dplyr::group_by(Stock) |>
    dplyr::mutate(
      score = dplyr::case_when(
        (F.Fmsy < 1 & B.Bmsy > 1.0) ~ "a",
        (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
        (F.Fmsy > 1 | B.Bmsy < 0.5) ~ "c",
        # color stocks with bbmsy only
        B.Bmsy > 1 & is.na(F.Fmsy) ~ "a",
        B.Bmsy > 0.5 & B.Bmsy < 1 & is.na(F.Fmsy) ~ "b",
      )
    ) |>
    dplyr::mutate(Council = dplyr::recode(Council, "Both" = "NEFMC/MAFMC")) |>
    # recode so we can plot stocks with missing ffmsy
    dplyr::mutate(
      F.Fmsy = dplyr::case_when(
        is.na(F.Fmsy) & !is.na(B.Bmsy) ~ 100,
        TRUE ~ F.Fmsy
      ),
      Cat = dplyr::case_when(F.Fmsy == 100 ~ FALSE, TRUE ~ TRUE)
    ) |>
    dplyr::filter(!is.na(B.Bmsy))

  # order stocks with no ffmsy -- for better plotting
  if (nrow(fix |> dplyr::filter(F.Fmsy == 100)) > 1) {
    # order <- fix |> dplyr::filter(F.Fmsy == 100) |>
    #   dplyr::arrange(B.Bmsy)
    num_bbmsy_only <- nrow(fix |> dplyr::filter(F.Fmsy == 100))
    num_both <- nrow(fix |> dplyr::filter(!F.Fmsy == 100))

    fix <- fix |>
      dplyr::arrange(F.Fmsy, B.Bmsy)

    fix$F.Fmsy <- fix$F.Fmsy +
      c(rep(0, num_both), seq(-0.3, by = 0.07, length.out = num_bbmsy_only))
  }

  unknown <- unfiltered |>
    dplyr::filter(is.na(F.Fmsy) & is.na(B.Bmsy)) |>
    dplyr::select(Stock, F.Fmsy, B.Bmsy)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  max_f <- max(fix$F.Fmsy[which(fix$F.Fmsy < 50)], na.rm = TRUE)

  # if there are many stocks with very low ffmsy, add a break at -1 to spread out the points and avoid overplotting
  # should be replaced with a better fix in the future
  min <- ifelse(
    fix |>
      dplyr::filter(F.Fmsy < 0.2) |>
      nrow() >
      4,
    -2,
    0
  )

  ybreaks <- seq(
    0,
    ifelse(max_f > 1, max_f, 1),
    by = 0.25
  )

  # --- Logic to force top panel to 1/4 height ---
  # Calculate the range of the bottom panel (Cat = TRUE)
  range_bottom <- max(ybreaks)

  # Calculate required range for top panel to be 1/3 of bottom (1:3 ratio = 1/4 total)
  req_range_top <- range_bottom / 3

  # Create dummy data centered at 100 to force this range
  # We include Council/score placeholders to satisfy the global mapping
  dummy_limits <- tibble::tibble(
    Cat = FALSE,
    B.Bmsy = 1, # Arbitrary valid X
    F.Fmsy = c(100 - (req_range_top / 2), 100 + (req_range_top / 2)),
    Council = "MAFMC", # Placeholder factor level
    score = "a" # Placeholder factor level
  )
  # ----------------------------------------------

  # offset_y <- fix |>
  #   dplyr::mutate(
  #     new_ffmsy = dplyr::case_when(
  #       F.Fmsy == 100 ~ F.Fmsy + rnorm(n = dplyr::n(), mean = 0, sd = 0.15),
  #       TRUE ~ F.Fmsy
  #     )
  #   )

  p <- fix |>
    ggplot2::ggplot() +
    # Add invisible dummy points to force panel sizing
    ggplot2::geom_blank(
      data = dummy_limits,
      ggplot2::aes(x = B.Bmsy, y = F.Fmsy)
    ) +
    # add more dummy points to make blank space for text at bottom of fig
    ggplot2::geom_blank(
      data = tibble::tibble(
        F.Fmsy = min,
        B.Bmsy = 1,
        Cat = TRUE,
        Council = "MAFMC", # Placeholder factor level
        score = "a" # Placeholder factor level
      ),
      ggplot2::aes(x = B.Bmsy, y = F.Fmsy)
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
    # don't plot hline for missing ffmsy
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = yval),
      linetype = "dashed",
      data = tibble::tibble(Cat = c(TRUE, FALSE), yval = c(1, NA))
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = B.Bmsy,
      y = F.Fmsy,
      shape = Council,
      color = score
    )) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        x = B.Bmsy, #geom_text_repel auto-jitters text around points
        y = F.Fmsy, # extra jittering for missing ffmsy points
        label = Code,
        color = score
      ),
      # data = offset_y,
      max.overlaps = 50,
      min.segment.length = 0.1,
      # direction = "y"
      # max.time = 5,
      # max.iter = 10^6#,
      force = 5,
      # force_pull = 0,
      # box.padding = 1
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(ybreaks, 100),
      labels = c(ybreaks, "No F/Fmsy\nEstimate")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(Cat),
      scales = "free_y",
      space = "free_y"
    ) +
    ggplot2::scale_color_manual(
      values = c("c" = "#D95F02", "b" = "#7570B3", "a" = "#1B9E77")
    ) +
    ggplot2::xlab(expression(~ B / B[msy])) +
    ggplot2::ylab(expression(~ F / F[msy])) +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle(paste0(report, ": stock status")) +
    ecodata::theme_ts() +
    ecodata::theme_title() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::guides(color = "none")

  return(list(p = p, unknown = unknown))
}

attr(plot_stock_status, "report") <- c("MidAtlantic", "NewEngland")

#plot_stock_status(report = "NewEngland")
#ggplot2::ggsave(
#  here::here("images/ne_stock_status_test3.png"),
#  width = 6.5,
#  height = 5
#)

#plot_stock_status(report = "MidAtlantic")
#ggplot2::ggsave(
#  here::here("images/mab_stock_status_test3.png"),
#  width = 6.5,
#  height = 5
#)
