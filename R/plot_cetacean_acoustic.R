plot_cetacean_acoustic <- function() {
  speciesColors <- data.frame(
    species = c('SPWH', 'HUWH', 'RIWH', 'SEWH', 'FIWH', 'BLWH'),
    color = c('#0D4C5B', '#CC79A7', '#E69F00', '#800000', '#009E73', '#08519c')
  )

  # get full species names to add to legend
  species_codes <- cetacean_acoustic |>
    dplyr::select(sound_source_code, sound_source_name) |>
    dplyr::distinct()
  species_codes_label <- species_codes$sound_source_name
  names(species_codes_label) <- species_codes$sound_source_code

  ## create plot
  plt <- PAMscapes::plotAcousticScene(
    cetacean_acoustic |>
      ## TODO: this is probably wrong, just trying to test plot abilities
      dplyr::rename(
        UTC = recording_usable_end_datetime,
        species = sound_source_code
      ),
    facet = "region",
    combineYears = TRUE,
    freqMap = speciesColors
  ) +
    ggplot2::scale_x_date(
      expand = c(0.02, -0.02),
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 10),
      strip.text.y = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(), #remove y axis values
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 0.75
      ),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab("Time (Months)") +
    # ggplot2::guides(fill = ggplot2::guide_legend(title = "Species")) +
    ggplot2::scale_fill_manual(
      labels = species_codes_label |> stringr::str_wrap(15),
      values = speciesColors$color
    ) +
    ggplot2::labs(title = "Monthly Cetacean Presence")
  # plt

  return(plt)
}
