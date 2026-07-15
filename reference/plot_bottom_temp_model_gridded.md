# plot bottom temperature seasonal gridded data

plots bottom_temp_model_gridded data set

## Usage

``` r
plot_bottom_temp_model_gridded(
  shadedRegion = NULL,
  report = "MidAtlantic",
  scale = "celsius"
)
```

## Arguments

- shadedRegion:

  Numeric vector. Years denoting the shaded region of the plot (most
  recent 10), passed from plot function

- report:

  Character string. Which SOE report ("MidAtlantic", "NewEngland"),
  passed from plot function

- scale:

  character string. celsius or fahrenheit. Default = "celsius"

## Value

ggplot object
