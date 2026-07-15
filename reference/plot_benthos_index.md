# plot benthos index

Description should be here. This needs to be reworked to uncouple GB and
GOM

## Usage

``` r
plot_benthos_index(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Macrobenthos",
  plottype = "index",
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

  Character string. Benthos group name ("Macrobenthos", "Megabenthos")

- plottype:

  Character string. Benthos biomass index by region, or coastwide center
  of gravity ("index", "cog")

- n:

  Numeric scalar. Number of years used (from most recent year) to
  estimate short term trend . Default = 0 (No trend calculated)

## Value

ggplot object
