# A ggplot2 ggtitle formatting for SOE figure titles

A ggplot2 ggtitle formatting for SOE figure titles

## Usage

``` r
theme_title(...)
```

## Examples

``` r
library(ggplot2)
data <- data.frame(x = rep(1:10,2),
                   y = rnorm(20),
                   Var = rep(c("group 1","group 2"), each = 10))

#Plot series with trend and SOE plot theme
ggplot(data = data) +
  geom_line(aes(x = x, y = y)) +
  theme_title()


```
