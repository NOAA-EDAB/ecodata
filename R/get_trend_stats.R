#' extracts trendline related stats from ggplot objects
#'
#' @param p ggplot object
#'

get_trend_stats <- function(p) {
  built_p <- ggplot2::ggplot_build(p)

  long_df <- NULL
  short_df <- NULL

  # Iterate safely through all layers to find the stat outputs
  for (layer_data in built_p$data) {
    # Identify Long-Term GLS stats
    if ("norm_aicc" %in% names(layer_data)) {
      raw_gls <- unique(layer_data[, c(
        "norm_aicc",
        "norm_pval",
        "norm_slope",
        "ar1_aicc",
        "ar1_pval",
        "ar1_slope",
        "is_autocorrelated"
      )])

      long_df <- data.frame(
        type = c("long", "long"),
        model = c("linear_norm", "linear_ar1"),
        aicc = c(raw_gls$norm_aicc, raw_gls$ar1_aicc),
        pval = c(raw_gls$norm_pval, raw_gls$ar1_pval),
        pval_chi2 = c(NA, NA), # Pad missing short-term metric
        slope = c(raw_gls$norm_slope, raw_gls$ar1_slope),
        is_winning_model = c(
          !raw_gls$is_autocorrelated,
          raw_gls$is_autocorrelated
        )
      )
    }

    # Identify Short-Term LM Boot stats
    if ("pval_chi2" %in% names(layer_data)) {
      raw_lm <- unique(layer_data[, c("pval", "pval_chi2", "slope")])

      short_df <- data.frame(
        type = "short",
        model = "linear_ar1_boot",
        aicc = NA, # Pad missing long-term metric
        pval = raw_lm$pval,
        pval_chi2 = raw_lm$pval_chi2,
        slope = raw_lm$slope,
        is_winning_model = TRUE # Short-term only tests one model
      )
    }
  }

  # Bind rows into a single dataframe, ignoring NULLs if one trend wasn't plotted
  trend_stats <- rbind(long_df, short_df)

  return(trend_stats)
}
