# plot hms_landings

Plot time series of Highly Migratory Species (HMS) commercial landings
or revenue.

## Usage

``` r
plot_hms_landings(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Landings"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland").

- varName:

  Character string. Which Variable to plot ("Landings", "Revenue") HMS
  landings/revenue are by Mid Atlantic or New England, not EPU

## Value

ggplot object
