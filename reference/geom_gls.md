# A geom to fit a GLS model to a time series

A geom to fit a GLS model to a time series

## Usage

``` r
geom_gls(
  mapping = NULL,
  data = NULL,
  stat = "GLS",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  warn = TRUE,
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

- na.rm:

  remove NAs

- show.legend:

  show legend

- inherit.aes:

  inherit aesthetics

- warn:

  Conditional. If `TRUE`, a warning message will be returned when N \<
  30.

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
x <- 1:30
y <-  m*x + rnorm(30, sd = 0.35)

data <- data.frame(x = x,
                  y = y)

#Plot series with trend
ggplot(data = data) +
  geom_line(aes(x = x, y = y)) +
  geom_gls(aes(x = x, y = y))
```
