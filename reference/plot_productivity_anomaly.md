# plot productivity_anomaly

plots the productivity anomaly dataset

## Usage

``` r
plot_productivity_anomaly(
  shadedRegion = NULL,
  report = "MidAtlantic",
  plottype = "region",
  varName = "anomaly",
  EPU = "MAB"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- plottype:

  Character string. "region" (legacy) or "council" (plot species managed
  by corresponding council)

- varName:

  Character string. Which variable to plot ("anomaly","assessment")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

## Value

ggplot object
