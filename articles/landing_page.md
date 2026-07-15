# landing page

## Overview

[**`ecodata`**](https://github.com/NOAA-EDAB/ecodata) is an R data
package developed by the [Ecosystem Dynamics and Assessment Branch of
the Northeast Fisheries Science
Center](https://www.fisheries.noaa.gov/new-england-mid-atlantic/ecosystems/northeast-ecosystem-dynamics-and-assessment-our-research)
for use in State of the Ecosystem (SOE) reporting.

## Background

| DOIs |
|----|
| [MAFMC SOE 2020](https://doi.org/10.25923/1f8j-d564) |
| [NEFMC SOE 2020](https://doi.org/10.25923/4tdk-eg57) |
| [Technical Documentation SOE 2020](https://doi.org/10.25923/64pf-sc70) |
| [MAFMC SOE 2021](https://doi.org/10.25923/jd1w-dc26) |
| [NEFMC SOE 2021](https://doi.org/10.25923/6pww-mw45) |
| [Technical Documentation SOE 2021](https://repository.library.noaa.gov/view/noaa/29277) |
| [MAFMC SOE 2022](https://doi.org/10.25923/5s5y-0h81) |
| [NEFMC SOE 2022](https://doi.org/10.25923/ypv2-mw79) |
| [Technical Documentation SOE 2022](https://doi.org/10.25923/xq8b-dn10) |
| [MAFMC SOE 2023](https://doi.org/10.25923/vy6j-w454) |
| [NEFMC SOE 2023](https://doi.org/10.25923/9sb9-nj66) |
| [Technical Documentation SOE 2023](https://doi.org/10.25923/5scc-rm92) |
| [MAFMC SOE 2024](https://doi.org/10.25923/vz5a-d111) |
| [NEFMC SOE 2024](https://doi.org/10.25923/f8xc-hj17) |
| [Technical Documentation SOE 2024](https://doi.org/10.25923/rz23-fc61) |

The State of the Ecosystem reports are high-level overviews of ecosystem
indicator status and trends occurring on the Northeast Continental
Shelf. They are apart of a larger [Integrated Ecosystem Assessment
(IEA)](https://www.integratedecosystemassessment.noaa.gov/regions/northeast)
approach to ecosystem based management. The SOE reports are developed
for US Fishery Management Councils (FMCs) in New England and the
Mid-Atlantic. DOIs for SOEs and related documents are found in the table
to the right.

## How it works

The schematic below shows the workflow from contributor data submission
to developing the State of the Ecosystem and other products. The data,
once contributed, is cleaned to ensure the data is in the correct
format. These `indicator.rda` datasets are used to create draft
visualizations in categorized subgroup_visuals.Rmd. The subgroups
include \[Low Trophic/Climate/Habitat Mid-Atlantic Bight\], \[Low
Trophic/Climate/Habitat New England\], \[Macrofauna Mid-Atlantic
Bight\], \[Macrofauna New England\], \[Human Dimensions Mid-Atlantic
Bight\], and \[Human Dimensions New England \]. The code used to create
these visuals is stored so it can be grabbed for use in the State of the
Ecosystem reports and other products (Technical Documentation,
presentations, etc.).

## What is included

Each annual report cycle is developed with new and updated datasets.
Guidelines for formatting data for the State of the Ecosystem reports
are [linked here](https://noaa-edab.github.io/ecodata/data_guidelines).
This table shows which versions of all related products correspond to a
specific State of the Ecosystem report cycle.

| Year | Audience | State of the Ecosystem | Response Memo | ecodata | Technical Documentation |
|----|----|----|----|----|----|
| 2019 | MAMFC | [0.1.0](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/v0.1.0) | NA | [0.1.0](https://github.com/NOAA-EDAB/ecodata/tree/0.1.0) | [1.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/v1.0.0) |
| 2019 | MAFMC | [0.1.1](https://github.com/NOAA-EDAB/SOE-MAFMC/tree/v0.1.1) | NA | [0.1.0](https://github.com/NOAA-EDAB/ecodata/tree/0.1.0) | [1.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/v1.0.0) |
| 2019 | NEMFC | [0.1.0](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/v0.1.0) | NA | [0.1.0](https://github.com/NOAA-EDAB/ecodata/tree/0.1.0) | [1.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/v1.0.0) |
| 2019 | NEFMC | [0.1.1](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/v0.1.1) | NA | [0.1.0](https://github.com/NOAA-EDAB/ecodata/tree/0.1.0) | [1.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/v1.0.0) |
| 2020 | Internal Review MAFMC SOE | [2020.0.0](https://github.com/NOAA-EDAB/SOE-MAFMC/tree/2020.0.0) | [2020.0.0](https://github.com/NOAA-EDAB/memos/tree/2020.1.0) | [1.0.0](https://github.com/NOAA-EDAB/ecodata/tree/1.0.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2020 | Internal Review NEFMC SOE | [2020.0.0](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/2020.0.0) | [2020.0.0](https://github.com/NOAA-EDAB/memos/tree/2020.1.0) | [1.0.0](https://github.com/NOAA-EDAB/ecodata/tree/1.0.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2020 | MAFMC SSC | [2020.1.0](https://github.com/NOAA-EDAB/SOE-MAFMC/tree/2020.1.0) | [2020.0.0](https://github.com/NOAA-EDAB/memos/tree/2020.0.0) | [1.0.0](https://github.com/NOAA-EDAB/ecodata/tree/1.0.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2020 | NEFMC SSC | [2020.1.0](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/2020.1.0) | [2020.1.0](https://github.com/NOAA-EDAB/memos/tree/2020.1.0) | [1.0.0](https://github.com/NOAA-EDAB/ecodata/tree/1.0.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2020 | MAFMC | [2020.2.0](https://github.com/NOAA-EDAB/SOE-MAFMC/tree/2020.2.0) | [2020.1.0](https://github.com/NOAA-EDAB/memos/tree/2020.1.0) | [1.1.0](https://github.com/NOAA-EDAB/ecodata/tree/1.1.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2020 | NEFMC | [2020.2.0](https://github.com/NOAA-EDAB/SOE-NEFMC/tree/2020.2.0) | [2020.1.0](https://github.com/NOAA-EDAB/memos/tree/2020.1.0) | [1.1.0](https://github.com/NOAA-EDAB/ecodata/tree/1.1.0) | [2.0.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.0.0) |
| 2021 | Internal Review NAFMC SOE | [2021.0.0](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2021.0.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | Internal Review MAFMC SOE | [2021.0.0](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2021.0.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | NEFMC SSC | [2021.1.0](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2021.1.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | MAFMC SSC | [2021.1.0](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2021.1.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | NEFMC | [2021.2.0](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2021.2.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos/releases/tag/2021.1.0) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | MAFMC | [2021.2.0](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2021.2.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos/releases/tag/2021.1.0) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | NEFMC w/ Protected Species Revisions | [2021.3.0](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2021.3.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos/releases/tag/2021.1.0) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2021 | MAFMC w/ Protected Species Revisions | [2021.3.0](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2021.3.0) | [2021.1.0](https://github.com/NOAA-EDAB/memos/releases/tag/2021.1.0) | [2.0.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/2.0.0) | [2.1.0](https://github.com/NOAA-EDAB/tech-doc/tree/2.1.0) |
| 2022 | MAFMC SSC | [2022.1](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2022.1) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [3.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/3.0) | [3.0](https://github.com/NOAA-EDAB/tech-doc/tree/3.0) |
| 2022 | NEFMC SSC | [2022.1](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2022.1) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [3.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/3.0) | [3.0](https://github.com/NOAA-EDAB/tech-doc/tree/3.0) |
| 2022 | MAFMC | [2022.2](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2022.2) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [3.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/3.0) | [3.0](https://github.com/NOAA-EDAB/tech-doc/tree/3.0) |
| 2022 | NEFMC | [2022.2](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2022.2) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [3.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/3.0) | [3.0](https://github.com/NOAA-EDAB/tech-doc/tree/3.0) |
| 2023 | MAFMC | [2023](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2023) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [4.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/4.0) | [4.0](https://github.com/NOAA-EDAB/tech-doc/tree/4.0) |
| 2023 | NEFMC | [2023](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2023) | [2022.1](https://github.com/NOAA-EDAB/memos/releases/tag/2022.1) | [4.0](https://github.com/NOAA-EDAB/ecodata/releases/tag/4.0) | [4.0](https://github.com/NOAA-EDAB/tech-doc/tree/4.0) |
| 2024 | MAFMC | [2024](https://github.com/NOAA-EDAB/SOE-MAFMC/releases/tag/2024) | [2024](https://github.com/NOAA-EDAB/memos/releases/tag/2024) | [5.0.1](https://github.com/NOAA-EDAB/ecodata/releases/tag/5.0.1) | [5.0](https://github.com/NOAA-EDAB/tech-doc/releases/tag/5.0) |
| 2024 | NEFMC | [2024](https://github.com/NOAA-EDAB/SOE-NEFMC/releases/tag/2024) | [2024](https://github.com/NOAA-EDAB/memos/releases/tag/2024) | [5.0.1](https://github.com/NOAA-EDAB/ecodata/releases/tag/5.0.1) | [5.0](https://github.com/NOAA-EDAB/tech-doc/releases/5.0) |

Below is a table outlining the indicators included in `ecodata` and
which SOE year the indicator was included in the report(s). Further
information for each indicator can be found in the [Technical
Documentation: State of the
Ecosystem](https://noaa-edab.github.io/tech-doc/). This holds the
details on the methodologies used for all indicators included in the
reports including those not included in `ecodata`.

| Indicator | Data Name | Year added | Last Methods Update | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | Link to Methods |
|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| ABC or ACL for MA Managed Species | abc.acl | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/mafmc-abcacl-and-catch.html#mafmc-abcacl-and-catch> |
| Aggregate Group Biomass | aggregate_biomass | 2018 | 2020 | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/aggroups.html> |
| Aquaculture | aquaculture | 2017 | 2019 | Y | Y | Y | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/aquaculture.html> |
| Bennet Index | bennet | 2018 | 2018 | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/bennet-indicator.html> |
| Blue Runner Presense on NES | blue_runner | 2020 | 2020 | N | N | N | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/safmc-managed-spp.html> |
| Bottom heatwaves | bottom_heatwave | 2023 | 2023 | N | N | N | N | N | N | NA |  |
| Bottom Temperature | bottom_temp_insitu | 2019 | 2019 | N | N | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/bottom-temperatures.html> |
| Calanus Stage | calanus_stage | 2020 | 2020 | N | N | N | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/calanus-stage.html#calanus-stage> |
| Chesapeake Bay Sea Surface Temperature | ches_bay_sst | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/chesapeake-bay-seasonal-sst-anomalies.html#chesapeake-bay-seasonal-sst-anomalies> |
| Chesapeake Bay Temperature | ches_bey_temp | 2021 | 2021 | N | N | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity-and-temperature.html#chesapeake-bay-salinity-and-temperature> |
| Chesapeake Bay Water Quality | ches_bay_wq | 2019 | 2019 | N | N | Y | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/chesapeake-bay-water-quality-standards-attainment.html> |
| Chlorophyll and Primary Production | chl_pp | 2018 | 2018 | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/chl-pp.html> |
| Chesapeake Bay Salinity | ch_bay_sal | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/chesapeake-bay-salinity-and-temperature.html#chesapeake-bay-salinity-and-temperature> |
| Cold Pool Index | cold_pool | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/cold-pool-index.html> |
| Commercial Fisheries | comdat | 2017 | 2017 | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/comdat.html> |
| Commercial Fisheries Diversity | commercial_div | 2018 | 2018 | N | Y | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/catch-and-fleet-diversity.html> |
| Forage Fish Energy Density | energy_density | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/forage-fish-energy-density.html> |
| Community Engagement | engagement | 2019 | 2020 (shift to timeseries) | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/community-engagement.html> |
| Expected Number of Species per 1000 individuals | exp_n | 2021 | 2021 | N | N | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/expected-number-of-species.html#expected-number-of-species> |
| Forage Anomaly | forage_anomaly | 2022 | 2022 | N | N | N | N | N | Y | NA |  |
| Gray Seal Bycatch | grayseal | 2021 | 2021 | N | N | N | N | Y | N | NA | <https://noaa-edab.github.io/tech-doc/harbor-porpoise-and-gray-seal-bycatch.html#harbor-porpoise-and-gray-seal-bycatch> |
| Gulf Stream Index | gsi | 2019 | 2020 (shift to Perez-Hernandez and Joyce 2014 methodology) | N | N | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/gulf-stream-index.html> |
| Habitat Vulnerability Analysis | habitat_vulnerability | 2021 | 2021 | N | N | N | N | Y | N | NA | <https://noaa-edab.github.io/tech-doc/habitat-vulnerability.html#habitat-vulnerability> |
| Harbor Porpoise Abundance | harborporpoise | 2018 | 2018 | N | Y | Y | N | Y | N | NA | <https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html> |
| Marine Heatwave | heatwave | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/marine-heatwave.html> |
| Highly Migratory Species Catch per Unit Effort | hms_cpue | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/atlantic-hms-pop-cpue.html#atlantic-hms-pop-cpue> |
| Highly Migratory Species Landings | hms_landings | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/highly-migratory-species-landings.html> |
| Highly Migratory Species Stock Status | hms_stock_status | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/atlantic-highly-migratory-species-stock-status.html#atlantic-highly-migratory-species-stock-status> |
| Harbor Porpoise Density | hp_density | 2018 | 2018 | N | Y | Y | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/harbor-porpoise-bycatch.html> |
| Hudson River Flow | hudson_river_flow | 2022 | 2022 | N | N | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/hudson-river-flow.html#hudson-river-flow> |
| Ichthyoplankton Diversity | ichthyo_diversity | 2018 | 2018 | N | Y | Y | N | Y | N | NA | <https://noaa-edab.github.io/tech-doc/ichthyoplankton-diversity.html> |
| Long Term Sea Surface Temperature | long_term_sst | 2017 | 2017 | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/long-term-sea-surface-temperature.html> |
| Large Pelagic Survey | lps_sharks | 2021 | 2021 | N | N | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html#recreational-shark-fishing-indicators> |
| Mid-Atlantic Inshore Survey | mab_inshore_survey | 2019 | 2019 | N | N | Y | Y | N | Y | NA | <https://noaa-edab.github.io/tech-doc/inshoresurvdat.html> |
| Massachusettes Inshore Survey | mass_inshore_survey | 2019 | 2019 | N | N | Y | Y | N | Y | NA | <https://noaa-edab.github.io/tech-doc/inshoresurvdat.html> |
| North Atlantic Right Whale | narw | 2017 | 2020 (Includes calf birth) | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/right-whale-abundance.html> |
| NEFSC Bottom Trawl Survey | nefsc_survey | 2017 | 2020 (map to EPU to get uncertainty) | Y | Y | Y | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/survdat.html> |
| NEFSC Bottom Trawl Survey dissaggregated | nefsc_survey_disaggrated | 2017 | 2020 (map to EPU to get uncertainty) | Y | Y | Y | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/survdat.html> |
| New England Inshore Survey | ne_inshore_survey | 2019 | 2019 | N | N | Y | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/inshoresurvdat.html> |
| New England Wind Index | ne_wind | NA |  | N | N | Y | N | N | N | NA |  |
| Northeast Fisheries Observed Sharks | observed_sharks | 2021 | 2021 | N | N | N | N | Y | N | NA | <https://noaa-edab.github.io/tech-doc/atlantic-hms-pop-cpue.html#atlantic-hms-pop-cpue> |
| Phytoplankton Size Class | phyto_size | 2021 | 2021 | N | N | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/phytoplankton-size-class.html#phytoplankton-size-class> |
| Primary Production Required | ppr | 2020 | 2020 | N | N | N | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/primary-production-required.html> |
| Recruitment/Productivity | productivity_anomaly | 2017 | 2017 | Y | Y | Y | Y | N | Y | NA | <https://noaa-edab.github.io/tech-doc/fish-productivity-indicator.html> |
| Recreational Fisheries | recdat | 2017 | 2017 | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/recreational-fishing-indicators.html> |
| Recreational Highly Migratory Species | rec_hms | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/recreational-shark-fishing-indicators.html#recreational-shark-fishing-indicators> |
| Submerged Aquatic Vegetation | SAV | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/submerged-aquatic-vegetation.html#submerged-aquatic-vegetation> |
| Mid-Atlantic Waterbirds | seabird_mab | 2020 | 2020 | N | N | N | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/ma-waterbird-productivity.html> |
| New England Seabirds | seabird_ne | 2019 | 2019 | N | N | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/ne-seabird-diet-and-productivity.html> |
| Gray Seal Pup Counts | seal_pups | 2021 | 2021 | N | N | N | N | Y | N | NA |  |
| Seasonal Sea Surface Temperature Anomaly | seasonal_oisst_anom | 2018 | 2018 | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html> |
| Seasonal Sea Surface Temperature Spatial Anomaly | seasonal_oisst_anom_gridded | 2018 | 2018 | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html> |
| Slopewater | slopewater | 2019 | 2019 | N | N | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/slopewater-proportions.html> |
| Species Distribution | species_dist | 2017 | 2017 | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/species-distribution-indicators.html> |
| Species Groups | species_groupings | 2018 | 2020 (Squid moved to piscivores) | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/aggroups.html> |
| Stock Status | stock_status | 2017 | 2017 | Y | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/stockstatus.html> |
| Stomach Fullness | stom_fullness | 2020 | 2020 | N | N | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/stomach-fullness.html> |
| Bottom Trawl Survey Shannon Diversity | survey_shannon | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/survdat.html#survdat> |
| Warm Core Rings | wcr | 2020 | 2020 | N | N | N | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/warm-core-rings.html> |
| Wind Lease Area Landings Revenue | wea_landings_revenue | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/wea-fishing-port-landings.html#wea-fishing-port-landings> |
| Wind - Habitat Occupancy | wind_occupancy | 2020 | 2020 | N | N | N | Y | N | N | NA | <https://noaa-edab.github.io/tech-doc/wind-lease-areas-and-habitat-occupancy-overlap.html> |
| Wind Lease Areea Development Speed | wind_dev_speed | 2021 | 2021 | N | N | N | N | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/wind-energy-delvelopment-timeline.html#wind-energy-delvelopment-timeline> |
| Wind Lease Area Port Impacts | wind_port | 2022 | 2022 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/wea-fishing-port-landings.html#wea-fishing-port-landings> |
| Wind Lease Areas Revenue | wind_revenue | 2021 | 2021 | N | N | N | N | N | Y | NA | <https://noaa-edab.github.io/tech-doc/fisheries-revenue-in-wind-development-areas.html#fisheries-revenue-in-wind-development-areas> |
| Zooplankton Abundance | zoo_abund | 2017 | 2017 | Y | Y | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/zooabund.html> |
| Zooplankton Diversity | zoo_div | 2017 | 2017 | Y | Y | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/zooabund.html> |
| Zooplankton Optimal Interpolation | zoo_oi | 2017 | 2017 | Y | Y | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/zooabund.html> |
| Zooplankton Small-Large Index | zoo_sli_anom | 2017 | 2017 | Y | Y | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/zooabund.html> |
| Zooplankton Stratified Abundance | zoo_strat_abun | 2017 | 2017 | Y | Y | Y | Y | Y | N | NA | <https://noaa-edab.github.io/tech-doc/zooabund.html> |
| Fish Condtion | NA | NA | NA | N | Y | Y | Y | Y | Y | NA | <https://noaa-edab.github.io/tech-doc/fish-condition-indicator.html> |
| Annual SST Cycles | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/annual-sst-cycles.html> |
| Mid-Atlantic Harmful Algal Bloom | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/mid-atlantic-harmful-algal-bloom-indicator.html> |
| New England Harmful Algal Bloom | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/new-england-harmful-algal-bloom-indicator.html> |
| Verified Records of Southern Kingfish | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/verified-records-of-southern-kingfish.html> |
| Habitat Occupancy Models | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/hab-occu.html> |
| Species Density | NA | NA | NA | N | Y | Y | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/species-density-estimates.html> |
| Thermal Habitat projections | NA | NA | NA | N | Y | N | N | N | N | NA | <https://noaa-edab.github.io/tech-doc/thermal-habitat-projections.html> |
