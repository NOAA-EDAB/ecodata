# A geom to fit a LM model to a short time series

A geom to fit a LM model to a short time series

## Usage

``` r
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
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). By
  default `inherit.aes = TRUE`, which assigns the top-level plotting
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) to the GLS
  geom.

- data:

  Input series to be analyzed. If NULL, data is inherited from previous
  layer or `ggplot` call.

- stat:

  stat

- position:

  position

- show.legend:

  show legend

- na.rm:

  remove NAs

- inherit.aes:

  inherit aesthetics

- warn:

  Conditional. If `TRUE`, a warning message will be returned when N \<
  30.

- n:

  Numeric. Number of points to use for trend. Default = 10.

- nBootSamples:

  Numeric. Number of bootstrap samples used to test Null hypothesis.
  Default = 499

- pValThreshold:

  Numeric. Significance level of the test. Default = 0.05

- ...:

  Other arguments may be passed to the stat, including fixed aesthetics.

## Value

If slope is significantly different from 0 (p \< 0.05), then a line of
best fit is returned. Otherwise output is `NULL`.

## Examples

``` r
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
```
