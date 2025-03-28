% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ches_bay_sst.R
\name{plot_ches_bay_sst}
\alias{plot_ches_bay_sst}
\title{plot ches_bay_sst}
\usage{
plot_ches_bay_sst(
  shadedRegion = NULL,
  report = "MidAtlantic",
  scale = "celsius"
)
}
\arguments{
\item{shadedRegion}{Numeric vector. Years denoting the shaded region of the plot (most recent 10)}

\item{report}{Character string. Which SOE report ("MidAtlantic" only, default)}

\item{scale}{character string. celsius or fahrenheit. Default = "celsius"}
}
\value{
ggplot object
}
\description{
Plot map of sea surface temperature for Chesapeake Bay.
}
