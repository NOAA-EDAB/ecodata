# Ichthyoplankton diversity

Data from NOAA NEFSC Oceans and Climate branch EcoMon public dataset
were used to examine changes in diversity of abundance among 45
ichthyoplankton taxa. The 45 taxa were established in Walsh et al. 2015,
and include the most abundant taxa from the 1970s to present that
represent consistency in the identification of larvae. Both species
counts and estimates of Shannon-Wiener diversity are included in this
data set.

## Usage

``` r
ichthyo_diversity
```

## Format

69 rows and 5 columns

- Var: Specifies variable of interest and sampling season.

- Time: Sampling year.

- Units: Units of variable `Var`.

- EPU: Ecological Production Units (EPU) where sampling occurred. `All`
  refers to data aggregated to the level of the Northeast Continental
  Shelf.

- Value: Value of variable `Var`.

## Details

`NaN` values were included in the data where there were too few sampling
stations to be included in diversity calculations. More information
about these data are available at
<https://noaa-edab.github.io/tech-doc/ichthyoplankton-diversity.html>.

## References

Walsh, H. J., Richardson, D. E., Marancik, K. E., & Hare, J. A. (2015).
Long-term changes in the distributions of larval and adult fish in the
northeast US shelf ecosystem. *PloS one*, 10(9), e0137382.
