# Recreational fishing indicators

Recreational fishing indicators derived from Marine Recreational
Information Program data queries.

## Usage

``` r
recdat
```

## Format

320 rows and 5 columns

- Var: Specifies variable name.

- Value: Value of variable `Var`.

- Units: Units of variable `Var`.

- Time: Year.

- EPU: Ecological Production Units where data originated. In this case,
  `MA` refers to Mid-Atlantic, and `NE` to New England.

## Source

Read more about the MRIP database and perform your own queries at
<https://www.st.nmfs.noaa.gov/recreational-fisheries/data-and-documentation/run-a-data-query>.

## Details

There are five indicator time series within this data set: recreational
effort (number of days fished), recreational seafood (number of fish
caught), recreational anglers (number of anglers), diversity of
recreational catch (effective Shannon index), and recreational fleet
effort diversity across modes (effective Shannon index).

More information about each of these indicators is available at
<https://noaa-edab.github.io/tech-doc/recreational-fishing-indicators.html>.
