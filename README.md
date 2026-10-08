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

- `data/lookup/species_guild_TEMPLATE.csv` – fill in guild/host, save as `species_guild.csv`
- `reports/report.qmd` – summary report (rendered: `reports/report.html`)

## Run
Put `EUForDam_REW3.xlsx` in `data/raw/`, then:

```
Rscript -e 'targets::tar_make()'
quarto render reports/report.qmd
```

Status: Steps 1-7 done (inspection, missingness, transforms, pairwise
correlations, temporal vs spatial, transferability, guild structure), version 2
aligned with Hlasny et al. (2025, Global Change Biology): series with < 6
non-zero values removed, CZ spruce bark-borer group treated as I. typographus,
year-anomaly version added, and the paper's Table 6 replicated
(`R/paper.R`). No gap-filling or imputation yet.
