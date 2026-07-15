# Create all variations of an ecodata plot function

This function can flexibly plot all possible variations given the
arguments and attributes of an ecodata plot function This function can
also print all of the code needed to run all possible variations This
may be useful for reviewing data submissions, creating catalog pages,
automating QA/QC protocols and comparing against other ecodata versions.

## Usage

``` r
create_all_plots(ecodata_name = NULL, write_only = FALSE, n = 0)
```

## Arguments

- ecodata_name:

  string. the data name of the target ecodata dataset or plot function
  (default = NULL)

- write_only:

  boolean. should the function output the plotting code, as text, for
  all plot variations (default = FALSE)

- n:

  numeric. number of data points for short term trend (default = 0)

## Value

list containing plots when write_only = FALSE and text when write_only =
TRUE. plots are also exported to the IDE plot pane.
