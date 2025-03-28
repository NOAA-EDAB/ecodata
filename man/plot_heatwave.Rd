% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_heatwave.R
\name{plot_heatwave}
\alias{plot_heatwave}
\title{plot heatwave}
\usage{
plot_heatwave(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Surface",
  n = 0
)
}
\arguments{
\item{shadedRegion}{Numeric vector. Years denoting the shaded region of the plot (most recent 10)}

\item{report}{Character string. Which SOE report ("MidAtlantic", "NewEngland")}

\item{varName}{Character string. Which Variable to plot ("Surface", "Bottom")}

\item{n}{Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)}
}
\value{
ggplot object
}
\description{
Time series plots of detrended temperature extreme event maximum intensity
and total days for either Surface or Bottom temperature.
}
