# EU-ForDAM pipeline

Automated cleaning, quality checks and summaries for the EU Forest Damage Database (EU-ForDAM).
It replaces the manual Excel pivot (`kotg_EUForDam_REW`) and summary sheet (`List1`).

## Run

```bash
Rscript run_pipeline.R data/EUForDam_REW.xlsx output
```

Then check the results:

```bash
Rscript tests/test_pipeline.R data/EUForDam_REW.xlsx
```

Requires R with the tidyverse, `readxl`, `writexl` and `testthat`.

## Data rules

| Situation | Meaning | Handling |
|---|---|---|
| `value1 = 0` | Monitored, no damage found | Kept as real zero |
| `value1` blank | Not recorded | Row dropped, never filled with 0 |
| No row for an agent/region/year | Not recorded | Left out, never filled with 0 |
| Repeated rows (same agent/host/year/region/unit) | Valid separate records | Kept, all counted in totals |
| `value2` | Same damage as `value1` in the other unit | Kept as `value_alt` with `unit_alt` (m3 <-> ha) |

## Outputs (`output/`)

- `eufordam_clean.csv`: one clean long table (snake_case columns, logical `grp_*` group flags).
- `eufordam_summaries.xlsx`:
  - `group_totals`: Europe-wide totals per group and unit (replaces `List1`).
  - `group_totals_country`: the same totals per country.
  - `group_totals_year`: the same totals per year.
  - `country_shares`: each country's share of total damage per unit.
  - `region_totals`: country × unit × tree type × region (replaces the pivot, for every country).
- `eufordam_qc.xlsx`: quality report. Covers dropped rows, repeated rows (listed for information; they are valid and kept), NUTS levels, coverage, value2/value1 ratios, category conflicts and odd labels. The report only flags; it changes nothing.

## Code

- `R/eufordam_pipeline.R`: functions (read, clean, QC, summaries).
- `run_pipeline.R`: command-line entry point.
- `tests/test_pipeline.R`: checks the data rules and reproduces the Excel pivot.
