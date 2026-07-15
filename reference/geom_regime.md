# A geom to fit a regime shifts to a time series

A geom to fit a regime shifts to a time series

## Usage

``` r
geom_regime(
  data = NULL,
  mapping = NULL,
  stat = "REGIME",
  geom = "vline",
  position = "identity",
  inherit.aes = TRUE,
  na.rm = FALSE,
  color = "red",
  show.legend = NA
)
```

## Arguments

- data:

  Input series to be analyzed. If NULL, data is inherited from previous
  layer or `ggplot` call.

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). By
  default `inherit.aes = TRUE`, which assigns the top-level plotting
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) to the GLS
  geom.

- stat:

  stat

- geom:

  geom

- position:

  position

- inherit.aes:

  inherit aesthetics

- na.rm:

  remove NAs

- color:

  color

- show.legend:

  show legend

- ...:

  Other arguments may be passed to the stat, including fixed aesthetics.

## Value

If regime shifts exists, then xintercept line(s) is returned. Otherwise
output is `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
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
  geom_regime()
} # }
```
