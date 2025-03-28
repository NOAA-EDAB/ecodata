% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bottom_temp_model_anom.R
\name{plot_bottom_temp_model_anom}
\alias{plot_bottom_temp_model_anom}
\title{plot bottom anomaly temperature time series}
\usage{
plot_bottom_temp_model_anom(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "seasonal",
  EPU = "MAB",
  plottype = "GLORYS",
  n = 0
)
}
\arguments{
\item{shadedRegion}{Numeric vector. Years denoting the shaded region of the plot (most recent 10)}

\item{report}{Character string. Which SOE report ("MidAtlantic", "NewEngland")}

\item{varName}{Character string. Which variable ("seasonal", "annual")}

\item{EPU}{Character string. Which EPU for New England report ("GB", "GOM") Mid will always be MAB}

\item{plottype}{character vector. Which source data should be plotted (e.g. 'GLORYS','MOM6')}

\item{n}{Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)}
}
\value{
ggplot object
}
\description{
plots bottom_temp_model_anom data set. Use GLORYS and PSY to supplement
}
