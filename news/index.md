# Changelog

## ecodata v10.0.0

Date: 10/13/2026

### Breaking Changes

- Standardized the column names of the following datasets:
  - `aquaculture`
  - `bottom_temp_model_anom`
  - `cold_pool`
  - `crew_survey`
  - `dissolved_oxygen`
  - `effective_sweptarea`
  - `gom_salmon`
  - `gsi`
  - `habs`
  - `ne_inshore_survey`
  - `preyfield_energy`
  - `productivity_anomaly`
  - `seasonal_bt_anomaly_gridded`
  - `seasonal_oisst_anom_gridded`
  - `shoreside_support`
  - `storminess`
  - `wea_landings_rev`
  - `wind_dev_speed`
  - `wind_occupancy`

### Features

- Changed default `n` argument of `plot_wea_landings_rev` to `n = 0`
- Changed default `report` argument to `report = "NewEngland"` for:
  - `plot_habs`
  - `plot_mass_inshore_survey`
  - `plot_ne_inshore_survey`
  - `plot_sandlance`
- Added `plottype` argument to `plot_condition` with options `scaled`
  and `raw`
- Converted the following plot functions to `flextable`:
  - `plot_community_factors`
  - `plot_wea_landings_rev`
  - `plot_wind_occupancy`
- Removed faceted behavior from `plot_cold_pool`
- Added a seed to `geom_lm`

### Fixes

- Added explicit function calls to `plot_forage_anomaly`

## ecodata v9.1.0

Date: 09/02/2026

### Features

- Added `print` argument to `create_all_plots` function

## ecodata v9.0.0

Date: 08/20/2026

### Breaking Changes

- Incremented R dependency to 4.1.0 or higher

## ecodata v8.0.0

Date: 07/29/2026

### Improvements

- Renamed default branch from `master` to `main`
- Deployed suite of new and revised issue templates
- Deployed suite of new pull request templates
- Added new repository documentation (CONTRIBUTING, CODE_OF_CONDUCT)
- Revised README
- Adjusted workflow triggers and bumped versions of external actions
- Deployed Air reformatter workflows and style guide
- Reformatted codebase according to new style guide

## ecodata v7.0.0

Release includes data/plots used to build the 2026 NEFMC and MAFMC State
of the Ecosystem Reports submitted to both SSCs and councils (MAFMC &
NEFMC). Date: 07/15/2026

## ecodata 6.0.1

#### Fixes

- Resolved package installation errors on Windows machines related to
  long file paths
- Updated package documentation to reflect new instructions for package
  installation

## ecodata 6.0

Release includes data/plots used to build the 2025 NEFMC and MAFMC State
of the Ecosystem Reports submitted to both SSCs and councils (MAFMC &
NEFMC). Date: 3/25/2025

## ecodata 5.0.2

#### Fixes

- `plot_thermal_habitat_persistence` - incorrect year calculation

## ecodata 5.0.1

Release includes data/plots used to build the 2024 NEFMC and MAFMC State
of the Ecosystem Reports submitted to the MAFMC SSC. Date: 3/08/2024

## ecodata 5.0

Release includes data/plots used to build the 2024 NEFMC and MAFMC State
of the Ecosystem Reports submitted to RPTS for internal review. Date:
2/12/2024

#### Added Functions

- Plotting functions for all of the indicators taking the form
  `plot_indicatorName()`

## ecodata 4.0

Release includes data/plots used to build 2023 NEFMC and MAFMC State of
the Ecosystem Reports.

## ecodata 3.0

Release includes data/plots used to build 2022 NEFMC and MAFMC State of
the Ecosystem Reports.

## ecodata 2.0

Release includes data/plots used to build the 2021 NEFMC and MAFMC State
of the Ecosystem Reports.

## ecodata 1.1.0

Release includes data/plots for NEFMC and MAFMC SOEs.

## ecodata 0.1.0

This release includes data sets incorporated into the 2018-2019 NEFMC
and MAFMC State of the Ecosystem reports.
