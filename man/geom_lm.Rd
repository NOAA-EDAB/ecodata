% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shortTermTrend.R
\name{geom_lm}
\alias{geom_lm}
\title{A geom to fit a LM model to a short time series}
\usage{
geom_lm(
  mapping = NULL,
  data = NULL,
  stat = "LM",
  position = "identity",
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  warn = TRUE,
  n = 10,
  nBootSamples = 499,
  pValThreshold = 0.05,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
assigns the top-level plotting \code{aes()} to the GLS geom.}

\item{data}{Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.}

\item{stat}{stat}

\item{position}{position}

\item{show.legend}{show legend}

\item{na.rm}{remove NAs}

\item{inherit.aes}{inherit aesthetics}

\item{warn}{Conditional. If \code{TRUE}, a warning message will be returned when N < 30.}

\item{n}{Numeric. Number of points to use for trend. Default = 10.}

\item{nBootSamples}{Numeric. Number of bootstrap samples used to test Null hypothesis. Default = 499}

\item{pValThreshold}{Numeric. Significance level of the test. Default = 0.05}

\item{...}{Other arguments may be passed to the stat, including fixed aesthetics.}
}
\value{
If slope is significantly different from 0 (p < 0.05), then a line of best fit is returned. Otherwise output
is \code{NULL}.
}
\description{
A geom to fit a LM model to a short time series
}
\examples{
library(ggplot2)

#Generate series

m <- 0.1
x <- 1:10
y <-  m*x + rnorm(10, sd = 0.35)

data <- data.frame(x = x,
                  y = y)

#Plot series with trend
ggplot(data = data) +
  geom_line(aes(x = x, y = y)) +
  geom_lm(aes(x = x, y = y), n = 10)
}
