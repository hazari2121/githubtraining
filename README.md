# EU-ForDAM cross-species correlation analysis

PhD project (CZU Prague). Quantifies how informative each forest insect species'
damage record is about every other species, as groundwork for gap-filling.

## Layout
- `data/raw/` – raw EU-ForDAM workbook (read-only, **not committed**)
- `data/processed/` – derived data (not committed; rebuild with the pipeline)
- `R/` – functions used by the pipeline
- `_targets.R` – the [targets](https://docs.ropensci.org/targets/) pipeline
- `outputs/tables/`, `outputs/figures/` – results
- `renv.lock` – package versions

## Run
Put `EUForDam_REW3.xlsx` in `data/raw/`, then in R: `targets::tar_make()`.

Status: Step 1 (data inspection) only.
