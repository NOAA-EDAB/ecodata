# plot thermal habitat area

plots thermal_habitat_area data set.

## Usage

``` r
plot_thermal_habitat_area(
  shadedRegion = NULL,
  report = "MidAtlantic",
  EPU = "MAB"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- EPU:

  Character string. Which EPU for New England report ("GB", "GOM") Mid
  will always be MAB

## Value

ggplot object
