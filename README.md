# Methadone availability in Mexico, 2018–2025

This repository reproduces the figures and tables for the revised manuscript on Mexico's methadone supply disruption. The analysis starts with a cleaned COFEPRIS intermediate file. It does not contain the original prescription extract or the files used to clean medicine descriptions and quantities.

## Run the analysis

Use R from the repository root:

```sh
Rscript run_all.R
```

The scripts run in order:

1. `1_prepare_data.R` checks the release file and records the data flow.
2. `2_descriptive_tables.R` builds national, state, municipality, and CIJ summaries.
3. `3_interrupted_time_series.R` fits the primary model and sensitivity analyses.
4. `4_population_estimation.R` calculates national and state dose-based scenarios.
5. `5_figures.R` draws Figures 1–3 and Supplementary Figure S1.
6. `6_supplementary_tables.R` writes Supplementary Tables S1–S6.

Outputs go to `outputs/tables/` and `outputs/figures/`. The four figures are written as editable vector PDFs and 300 dpi PNGs. Generated outputs are ignored by Git; running the code recreates them.

Install these R packages before running: tidyverse, lubridate, sandwich, lmtest, tseries, MASS, gridExtra, sf, ggrepel, and scales. The scripts stop with a package list if anything is missing. They do not install packages or download data.

## Release input

`data/analysis_ready_prescriptions.csv` has one row per cleaned prescription-capture record from January 2018 through August 2025. It contains only:

| Column | Meaning |
| --- | --- |
| `capture_date` | Date recorded by the surveillance system; this is not an independently verified dispensing date. |
| `state` | Recorded Mexican state. |
| `municipality` | Recorded municipality. |
| `formulation` | Tablet or oral solution. |
| `total_mg` | Derived oral methadone volume in milligrams. |

The file has no patient, prescriber, establishment, licence, or source-record identifiers; no capture timestamps; and no original medicine or quantity descriptions. It retains 13 records with unavailable derived milligrams so the analysis flow can be checked. Fourteen otherwise valid records have missing geography and remain in national totals.

The first five record-flow counts describe processing of the restricted source files and are fixed in `1_prepare_data.R`. The public run validates the cleaned intermediate file and reproduces every downstream estimate, figure, and table from it. CIJ annual counts and locality-level site counts are held in `R/reference_data.R`; the map boundaries are in `reference/mexico_states.geojson`.

## Interpretation

COFEPRIS volume and CIJ clinic counts are separate sources. They are never added together. The population calculations translate recorded methadone volume into scenarios using daily doses of 40, 80, or 120 mg and monthly coverage of 75%, 90%, or 95%. They are not observed patient counts or statistical confidence intervals.

The additional state estimates cover the 19 states with at least one record in the COFEPRIS extract. A zero in a state-month means no volume was recorded for that state-month; it does not establish that no one received treatment. States absent from the extract are not assigned a population estimate.
