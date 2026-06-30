#' Fit and Compare Long-Term Trend Models
#'
#' Fits generalized least squares (GLS) linear models with both normal error structures
#' and first-order autoregressive (AR1) error structures to evaluate long-term trends
#' in time-series data. Models are evaluated using Akaike Information Criterion
#' with correction for small sample sizes (AICc).
#'
#' @param data A data frame or tibble containing at least the columns \code{Time} (numeric/integer)
#'   and \code{Value} (numeric values to model).
#'
#' @return A data frame containing comparison statistics for the fitted models with the following columns:
#'   \file{model} The model specification type (e.g., "long-term linear (normal error)").
#'   \file{aicc} Calculated AICc for model selection.
#'   \file{trend} Character indicating the direction of the trend ("positive" or "negative") based on the intercept.
#'   \file{pval} P-value calculated via a maximum likelihood (ML) ANOVA comparison against a constant baseline.
#'   \file{sig} Logical indicating if the trend is statistically significant (p < 0.05).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Time = 1:50, Value = rnorm(50, mean = 10 + (1:50)*0.1))
#' long_term_trend(df)
#' }

long_term_trend <- function(data) {
  # rename x and y if present in data
  if ("x" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Time = x)
  }

  if ("y" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Value = y)
  }

  if (!("Value" %in% colnames(data) | "Time" %in% colnames(data))) {
    stop(
      "Expected to find 'Time' and 'Value' in colnames but they are not present"
    )
  }

  if (nrow(data |> tidyr::drop_na()) < 30) {
    output <- tibble::tibble(
      model = c("linear_norm", "linear_ar1"),
      n_years = "long-term models not run; less than 30 years of data",
      aicc = NA,
      trend = NA,
      pval = NA,
      sig = NA
    )
    return(output)
  }

  set.seed(123)

  constant_norm <- nlme::gls(Value ~ 1, data = data, na.action = na.omit)

  constant_ar1 <-
    try(nlme::gls(
      Value ~ 1,
      data = data,
      correlation = nlme::corAR1(form = ~Time),
      na.action = na.omit
    ))
  if (class(constant_ar1) == "try-error") {
    return(
      best_lm <- data.frame(
        model = NA,
        aicc = NA,
        coefs..Intercept = NA,
        coefs.time = NA,
        coefs.time2 = NA,
        pval = NA
      )
    )
  }

  # Linear model with normal error
  linear_norm <- nlme::gls(Value ~ Time, data = data, na.action = na.omit)

  # Linear model with AR1 error
  linear_ar1 <-
    try(nlme::gls(
      Value ~ Time,
      data = data,
      correlation = nlme::corAR1(form = ~Time),
      na.action = na.omit
    ))
  if (class(linear_ar1) == "try-error") {
    return(
      best_lm <- data.frame(
        model = NA,
        aicc = NA,
        coefs..Intercept = NA,
        coefs.time = NA,
        coefs.time2 = NA,
        pval = NA
      )
    )
  }

  # Calculate AICs for all models
  df_aicc <-
    data.frame(
      model = c("linear_norm", "linear_ar1"),
      aicc = c(AICcmodavg::AICc(linear_norm), AICcmodavg::AICc(linear_ar1)),
      coefs = rbind(c(coef(linear_norm), NA), c(coef(linear_ar1), NA)),
      # Calculate overall significance (need to use
      # ML not REML for this)
      pval = c(
        anova(
          update(constant_norm, method = "ML"),
          update(linear_norm, method = "ML")
        )$`p-value`[2],
        anova(
          update(constant_ar1, method = "ML"),
          update(linear_ar1, method = "ML")
        )$`p-value`[2]
      )
    )

  nyear <- data |>
    tidyr::drop_na() |>
    nrow()

  output <- df_aicc |>
    dplyr::mutate(
      model = dplyr::case_when(
        model == "linear_norm" ~ "long-term linear (normal error)",
        model == "linear_ar1" ~ "long-term linear (AR1 error)"
      ),
      n_year = nyear,
      trend = dplyr::case_when(
        coefs.Time > 0 ~ "positive",
        coefs.Time < 0 ~ "negative",
        TRUE ~ "model did not converge"
      )
    ) |>
    dplyr::select(model, aicc, trend, pval) |>
    dplyr::mutate(sig = pval < 0.05)

  return(output)
}


#' Fit Short-Term Trend Model using Boostrapped AR1
#'
#' Filters the time-series dataset to look strictly at a recent window of time
#' (the last 10 years, or the length of the data if fewer than 30 observations exist),
#' and utilizes bootstrap simulation to evaluate a short-term AR1 trend.
#'
#' @param data A data frame or tibble containing at least the columns \code{Time} (numeric/integer)
#'   and \code{Value} (numeric values to model).
#'
#' @return A single-row tibble with the following structural columns:
#'   \file{model} Name of the model configuration or error status if convergence fails.
#'   \file{aicc} Hardcoded as `NA` due to bootstrap-driven assessment.
#'   \file{trend} The estimated slope parameter estimate ($beta$) if successful, otherwise `NA`.
#'   \file{pval} The empirical p-value from bootstrap simulation.
#'   \file{sig} Logical indicating if the trend is statistically significant (p < 0.05).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Time = 1:15, Value = rnorm(15))
#' short_term_trend(df)
#' }

short_term_trend <- function(data) {
  # rename x and y if present in data
  if ("x" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Time = x)
  }

  if ("y" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Value = y)
  }

  if (!("Value" %in% colnames(data) | "Time" %in% colnames(data))) {
    stop(
      "Expected to find 'Time' and 'Value' in colnames but they are not present"
    )
  }

  set.seed(123)

  ## Geom removes NAs from data. arfit needs full timeseries with NAs
  # arfit pads the time series and returns this padded data set.
  # the returned data set is used in plotting the fitted model
  years <- data |>
    tidyr::drop_na()

  max_year_with_data <- max(years$Time)

  n <- ifelse(nrow(years) < 30, nrow(data), 10)

  data <- data |>
    dplyr::rename(x = Time, y = Value)

  dataUse <- data |>
    dplyr::arrange(x) |>
    # Select last n years
    dplyr::filter(x %in% (max_year_with_data - (n - 1)):max_year_with_data) |>
    dplyr::mutate(x = x - min(x) + 1)

  # xmax <- max(data$x)
  # xmin <- xmax - n + 1

  # Linear model with AR1 error
  linear_ar1 <-
    try(arfit::fit_real_data(dataUse, nBootSims = 499))
  #print(linear_ar1$pValue)
  if (is.na(linear_ar1$pValue)) {
    output <- tibble::tibble(
      model = "short-term AR1 failed to converge",
      aicc = NA,
      trend = NA,
      pval = NA,
      sig = NA
    )
  } else {
    miss <- ifelse(nrow(years) != nrow(data), " (some missing data)", "")

    output <- tibble::tibble(
      model = "short-term AR1",
      n_years = paste0(n, " years", miss),
      aicc = NA,
      trend = dplyr::case_when(
        linear_ar1$alt$betaEst[2, 1] > 0 ~ "positive",
        linear_ar1$alt$betaEst[2, 1] < 0 ~ "negative"
      ),
      pval = linear_ar1$pValue,
      sig = linear_ar1$pValue < 0.05
    )
  }

  return(output)
}

#' Compute Summary Statistics and Assess Recent Status
#'
#' Evaluates baseline historical summary statistics for a given time-series distribution and checks
#' whether the most recent observation is historically "below average", "above average",
#' or "near average" based on a 1 standard deviation threshold from the mean.
#'
#' @param data A data frame or tibble containing at least the columns \code{Time} (numeric/integer)
#'   and \code{Value} (numeric values).
#'
#' @return A single-row tibble featuring columns:
#'   \file{mean} Arithmetic mean of \code{Value}.
#'   \file{min} Minimum observed \code{Value}.
#'   \file{max} Maximum observed \code{Value}.
#'   \file{median} Median of \code{Value}.
#'   \file{sd} Standard deviation of \code{Value}.
#'   \file{upper} One standard deviation above the mean ($mean + sd$).
#'   \file{lower} One standard deviation below the mean ($mean - sd$).
#'   \file{recent_value} The numeric value corresponding to the highest \code{Time} index point.
#'   \file{status} Categorized character state: "below average", "above average", or "near average".
#'
#' @export
#'
#' @examples
#' df <- data.frame(Time = 2010:2020, Value = c(12, 11, 14, 15, 13, 12, 14, 15, 11, 10, 22))
#' summary_stats(df)

summary_stats <- function(data) {
  # rename x and y if present in data
  if ("x" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Time = x)
  }

  if ("y" %in% colnames(data)) {
    data <- data |>
      dplyr::rename(Value = y)
  }

  if (!("Value" %in% colnames(data) | "Time" %in% colnames(data))) {
    stop(
      "Expected to find 'Time' and 'Value' in colnames but they are not present"
    )
  }

  output <- tibble::tibble(
    mean = mean(data$Value, na.rm = TRUE),
    min = min(data$Value, na.rm = TRUE),
    max = max(data$Value, na.rm = TRUE),
    median = median(data$Value, na.rm = TRUE),
    sd = sd(data$Value, na.rm = TRUE),
    upper = mean + sd,
    lower = mean - sd
  )

  max_year <- data |>
    # drop missing years
    tidyr::drop_na(Value) |>
    dplyr::filter(Time == max(Time))

  # print(max_year)

  this_recent_value <- max_year |>
    dplyr::pull(Value)

  this_recent_year <- max_year |>
    dplyr::pull(Time)

  # if (length(this_recent_value) > 1) {
  #   message(
  #     data |>
  #       dplyr::filter(Time == max(Time))
  #   )
  # }

  this_status <- dplyr::case_when(
    this_recent_value < output$lower ~ "below average",
    this_recent_value > output$upper ~ "above average",
    TRUE ~ "near average"
  )

  output <- output |>
    dplyr::mutate(
      recent_year = this_recent_year,
      recent_value = this_recent_value,
      status = this_status
    )

  return(output)
}

#' Compile Long-Term and Short-Term Trend Summaries
#'
#' A wrapper function that executes both \code{\link{long_term_trend}} and
#' \code{\link{short_term_trend}} on the input dataset, binding their results
#' together into a unified data frame for easy trend comparison.
#'
#' @param data A data frame or tibble containing at least the columns \code{Time} (numeric/integer)
#'   and \code{Value} (numeric values to model).
#'
#' @return A data frame/tibble combining the rows of both trend analyses. It contains
#'   the following structured columns:
#'   \file{model} Character string indicating the model type (e.g., long-term linear models
#'     or the short-term AR1 model windows).
#'   \file{aicc} Calculated AICc for model selection (will be \code{NA} for the short-term bootstrap model).
#'   \file{trend} Character string or numeric coefficient indicating the direction/magnitude of the trend.
#'   \file{pval} P-value associated with the respective trend model's significance test.
#'   \file{sig} Logical indicating if the trend is statistically significant (p < 0.05).
#'
#' @importFrom dplyr bind_rows
#' @export
#'
#' @seealso
#' \code{\link{long_term_trend}}, \code{\link{short_term_trend}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Time = 1:50, Value = rnorm(50, mean = 10 + (1:50)*0.05))
#' trend_summaries(df)
#' }

trend_summaries <- function(data) {
  ## TODO: pad out NAs
  dat1 <- long_term_trend(data)
  dat2 <- short_term_trend(data)

  output <- dplyr::bind_rows(dat1, dat2) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = c(n_years, aicc, trend, pval, sig)
    ) |>
    janitor::clean_names()

  return(output)
}

# data <- ecodata::comdat |>
#   dplyr::filter(Var == "Apex Predator Landings", EPU == "GB")
#
# long_term_trend(data)
# short_term_trend(data)
# summary_stats(data)
