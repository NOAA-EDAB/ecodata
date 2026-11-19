# Ingest and process Crew Survey data

# Define input path
raw.dir <- here::here("data-raw")
crew_survey_xlsx <- "2012_2024_Crew Survey SOE Dataset_01052026.xlsx"
crew_survey_index_file <- 'crew_survey_key.csv'
factor_levels <- here::here("data-raw/crew_survey_factor_levels.csv")

get_crew_survey <- function(save_clean = F) {
  # Read in original Crew Survey data file
  crew_survey_raw <- readxl::read_xlsx(
    path = file.path(raw.dir, crew_survey_xlsx)
  )

  crew_survey_key <- read.csv(file.path(raw.dir, crew_survey_index_file))

  #Key for city to EPU region
  port.names <- sort(unique(crew_survey_raw$`Primary Port Combined`))
  port.state.names <- sapply(port.names, function(x) {
    return(strsplit(x, split = ', ')[[1]][2])
  })

  port.region.key <- data.frame(port.state = sort(unique(port.state.names))) |>
    dplyr::mutate(
      EPU = dplyr::case_when(
        port.state %in% c('MD', 'NJ', 'NY', 'VA', 'NC') ~ 'MA',
        port.state %in% c('CT', 'MA', 'ME', 'NH', 'RI') ~ 'NE',
        TRUE ~ 'Other'
      )
    )
  colnames(port.region.key) <- c('Primary port state', 'EPU')

  #Key for Likehart questions

  crew_survey <- crew_survey_raw |>
    dplyr::mutate(
      ResponseID = c(1:nrow(crew_survey_raw)),
      Time = sub("-.*", "", `Survey wave`)
    ) |>
    dplyr::select(
      -`Subject number`,
      -`Case ID...2`,
      -`Case ID...3`,
      -`Survey wave`,
      -`Primary port state`,
      -`Primary port region`
    ) |>
    tidyr::separate(
      `Primary Port Combined`,
      into = c('port.city', 'Primary port state'),
      sep = ', ',
      remove = F
    ) |>
    dplyr::select(-port.city) |>
    dplyr::left_join(port.region.key) |>
    tidyr::pivot_longer(
      cols = -c(ResponseID, Time, EPU),
      names_to = "Var",
      values_to = "Value",
      values_transform = list(Value = as.character)
    ) |>
    dplyr::left_join(crew_survey_key) |>
    dplyr::filter(EPU %in% c('MA', 'NE'))

  #Loop through metric 'types' and calculate stats on state and region level
  var.todo <- crew_survey |>
    dplyr::filter(!is.na(metric)) |>
    dplyr::distinct(
      Var,
      var.short,
      plot.group,
      type,
      metric,
      binary.yes,
      binary.no,
      likehart.direction
    )

  #Likehart Key
  likehart.names <- crew_survey |>
    dplyr::filter(type == 'likehart') |>
    dplyr::pull(Value) |>
    unique()

  likehart.key <- data.frame(Value = likehart.names) |>
    dplyr::mutate(
      Value.key = dplyr::case_when(
        Value %in%
          c(
            '1',
            'Very Dissatisfied',
            'Strongly Disagree',
            'Very Difficult'
          ) ~
          1,
        Value %in% c('2', 'Dissatisfied', 'Disagree', 'Difficult') ~ 2,
        Value %in% c('3', 'Neutral', 'Neither easy nor difficult') ~ 3,
        Value %in% c('4', 'Satisfied', 'Agree', 'Easy') ~ 4,
        Value %in% c('5', 'Very Satisfied', 'Strongly Agree', 'Very Easy') ~ 5,
        TRUE ~ NA
      )
    )

  # unique(var.todo$metric)
  # unique(var.todo$type)
  #
  # i <- which(var.todo$type == 'numeric')[1]

  out.ls <- list()
  for (i in 1:nrow(var.todo)) {
    this.key <- var.todo[i, ]

    this.var <- crew_survey |>
      dplyr::filter(var.short == this.key$var.short)

    if (this.key$type == 'categorical') {
      out.ls[[i]] <- this.var |>
        dplyr::group_by(EPU, Time, Value) |>
        dplyr::summarise(N = dplyr::n()) |>
        dplyr::group_by(EPU, Time) |>
        dplyr::mutate(total = sum(N)) |>
        dplyr::ungroup() |>
        dplyr::rename(Var.Answer = 'Value') |>
        dplyr::mutate(
          Value = N / total,
          Units = 'proportion responses',
          Var.Long = this.key$Var,
          Var.Group = this.key$plot.group,
          Var = this.key$var.short
        ) |>
        dplyr::select(
          Time,
          EPU,
          Var.Long,
          Var,
          Var.Answer,
          Var.Group,
          Value,
          Units
        )
    } else if (this.key$type == 'binary') {
      out.ls[[i]] <- this.var |>
        dplyr::mutate(
          Value = dplyr::case_when(
            Value %in% c(this.key$binary.yes, this.key$binary.no) ~ Value,
            is.na(Value) ~ NA,
            TRUE ~ 'Unspecified'
          )
        ) |>
        dplyr::group_by(EPU, Time, Value) |>
        dplyr::summarise(N = dplyr::n()) |>
        dplyr::group_by(EPU, Time) |>
        dplyr::mutate(total = sum(N)) |>
        dplyr::ungroup() |>
        dplyr::rename(Var.Answer = 'Value') |>
        dplyr::mutate(
          Value = N / total,
          Units = 'proportion responses',
          Var.Long = this.key$Var,
          Var = this.key$var.short,
          Var.Group = this.key$plot.group,
        ) |>
        dplyr::select(
          Time,
          EPU,
          Var,
          Var.Long,
          Var.Answer,
          Var.Group,
          Value,
          Units
        )
    } else if (this.key$type == 'likehart') {
      this.var.mean <- this.var |>
        dplyr::left_join(likehart.key) |>
        dplyr::group_by(EPU, Time) |>
        dplyr::summarise(Value = mean(Value.key, na.rm = T)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          Var.Answer = NA,
          Units = paste0('mean:', this.key$likehart.direction)
        )
      this.var.prop <- this.var |>
        dplyr::group_by(EPU, Time, Value) |>
        dplyr::summarise(N = dplyr::n()) |>
        dplyr::group_by(EPU, Time) |>
        dplyr::mutate(total = sum(N)) |>
        dplyr::ungroup() |>
        dplyr::rename(Var.Answer = 'Value') |>
        dplyr::mutate(
          Value = N / total,
          Units = 'proportion responses',
        ) |>
        dplyr::select(EPU, Time, Var.Answer, Units, Value)

      out.ls[[i]] <- this.var.mean |>
        dplyr::bind_rows(this.var.prop) |>
        dplyr::mutate(
          Var.Long = this.key$Var,
          Var = this.key$var.short,
          Var.Group = this.key$plot.group,
        ) |>
        dplyr::select(
          Time,
          EPU,
          Var,
          Var.Long,
          Var.Answer,
          Var.Group,
          Value,
          Units
        )
    } else if (this.key$type == 'numeric') {
      out.ls[[i]] <- this.var |>
        dplyr::mutate(Value = as.numeric(Value)) |>
        dplyr::group_by(EPU, Time) |>
        dplyr::summarise(Value = mean(Value, na.rm = T)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          Var.Answer = NA,
          Units = 'mean response',
          Var.Long = this.key$Var,
          Var = this.key$var.short,
          Var.Group = this.key$plot.group,
        ) |>
        dplyr::select(
          Time,
          EPU,
          Var,
          Var.Long,
          Var.Answer,
          Var.Group,
          Value,
          Units
        )
    } else {
      warning(
        'Variable: ',
        this.key$Var,
        ' Does not have a comformable data type. Check key.'
      )
    }
  }

  crew_survey <- dplyr::bind_rows(out.ls) |>
    dplyr::select(
      Time,
      EPU,
      Var,
      Var.Long,
      Var.Answer,
      Var.Group,
      Value,
      Units
    ) |>
    # replace NAs with "No Response"
    dplyr::mutate(
      Var.Answer = ifelse(is.na(Var.Answer), "No Response", Var.Answer)
    )
  # Unify missing/unspecified categories into a single string
  crew_survey <- crew_survey |>
    dplyr::mutate(
      Var.Answer = dplyr::case_when(
        Var.Answer %in%
          c("DK/NA", "No Response", "Unspecified", "NA") ~ "NA/No Response",
        is.na(Var.Answer) ~ "NA/No Response",
        TRUE ~ Var.Answer
      )
    )
  # Extract all unique response categories dynamically
  unique_responses <- unique(crew_survey$Var.Answer)

  # Build a programmatic sorting key
  response_levels <- data.frame(Var.Answer = unique_responses) |>
    dplyr::mutate(
      # Intercept text-based numeric/range boundaries to force them into the correct order
      override_val = dplyr::case_when(
        grepl("(?i)under 25", Var.Answer) ~ 24.9, # Sorts just before "25 to..."
        grepl("(?i)55 or older", Var.Answer) ~ 55.1, # Sorts just after "45 to 54"
        grepl("(?i)less than 5", Var.Answer) ~ 4.9, # Sorts just before "5 to..."
        grepl("(?i)30 or more", Var.Answer) ~ 30.1, # Sorts just after ranges ending near 30
        grepl("(?i)one owner", Var.Answer) ~ 1,
        grepl("(?i)two owners", Var.Answer) ~ 2,
        grepl("(?i)three owners", Var.Answer) ~ 3,
        grepl("(?i)four or more", Var.Answer) ~ 4,
        TRUE ~ NA_real_
      ),

      # Extract the first number in the string
      parsed_num = suppressWarnings(
        as.numeric(gsub(
          "^[^0-9]*([0-9]+).*$",
          "\\1",
          gsub(",", "", Var.Answer)
        ))
      ),

      # Combine the manual overrides with the automatically parsed numbers
      num_val = dplyr::coalesce(override_val, parsed_num),

      # Map Likert scale strings to numeric weights (now case-insensitive to catch "Very difficult")
      likert_val = dplyr::case_when(
        tolower(Var.Answer) %in%
          c(
            '1',
            'very dissatisfied',
            'strongly disagree',
            'very difficult'
          ) ~ 1,
        tolower(Var.Answer) %in%
          c('2', 'dissatisfied', 'disagree', 'difficult') ~ 2,
        tolower(Var.Answer) %in%
          c('3', 'neutral', 'neither easy nor difficult') ~ 3,
        tolower(Var.Answer) %in% c('4', 'satisfied', 'agree', 'easy') ~ 4,
        tolower(Var.Answer) %in%
          c('5', 'very satisfied', 'strongly agree', 'very easy') ~ 5,
        TRUE ~ NA_real_
      ),

      # Group into sorting tiers
      sort_tier = dplyr::case_when(
        Var.Answer == "NA/No Response" ~ 5, # Pinned to the absolute end
        Var.Answer == "Other" ~ 4, # Pinned just before NA/No Response
        !is.na(likert_val) ~ 2, # Likert scales second
        !is.na(num_val) ~ 1, # Numeric values/ranges first
        TRUE ~ 3 # Standard characters third
      ),

      # Create unified columns for within-tier sorting
      sort_num = dplyr::coalesce(num_val, likert_val),
      sort_char = ifelse(sort_tier %in% c(1, 3), Var.Answer, "")
    ) |>
    # Sort by tier hierarchy -> numeric value -> alphabetical
    dplyr::arrange(sort_tier, sort_num, sort_char) |>
    dplyr::pull(Var.Answer)

  # Apply the programmatically sorted levels
  crew_survey$Var.Answer <- factor(
    crew_survey$Var.Answer,
    levels = response_levels
  )

  if (save_clean) {
    usethis::use_data(crew_survey, overwrite = T)
  } else {
    return(crew_survey)
  }
}
get_crew_survey(save_clean = T)
