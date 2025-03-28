% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_species_dist.R
\name{plot_species_dist}
\alias{plot_species_dist}
\title{plot species_dist}
\usage{
plot_species_dist(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "along",
  n = 0
)
}
\arguments{
\item{shadedRegion}{Numeric vector. Years denoting the shaded region of the plot (most recent 10)}

\item{report}{Character string. Which SOE report ("MidAtlantic", "NewEngland")}

\item{varName}{Character string. Which variable to plot ("along","depth")}

\item{n}{Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)}
}
\value{
ggplot object
}
\description{
Plots species distribution along shelf and depth
}
