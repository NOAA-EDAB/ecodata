# plot fishery engagement and community social vulnerability

Plot population relative engagement vs engagement by community,
recreational or commercial fishery. Provide table of social
vulnerability indicators for highly engaged communities. Indicator
definitions
https://www.fisheries.noaa.gov/national/socioeconomics/social-indicators-supporting-information

## Usage

``` r
plot_engagement(
  shadedRegion = NULL,
  report = "MidAtlantic",
  varName = "Commercial"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10)

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland")

- varName:

  Character string. Which Fishery to plot ("Commercial","Recreational")

## Value

ggplot object
