# plot finfish traits

Plot time series of community level fish traits for spring and fall.

## Usage

``` r
plot_finfish_traits(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "fecundity",
  n = 0
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which variable:Trophic Level, Offspring Size, Age at
  Maturity, Length at Maturity, Fecundity, Maximum Theoretical Length,
  Growth Rate, Maximum Observed Length, and three Pricipal Components
  Axes Pace of Life (PC1), PC2, andPC3
  ("trophic_level","offspring_size","age_maturity","length_maturity","fecundity","l_inf","k",
  "max_obs_length","PC1","PC2","PC3")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
