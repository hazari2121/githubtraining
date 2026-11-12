# EU-ForDAM pipeline

Automated cleaning, quality checks and summaries for the EU Forest Damage Database (EU-ForDAM).
It replaces the manual Excel pivot (`kotg_EUForDam_REW`) and summary sheet (`List1`).

## Run

```bash
Rscript run_pipeline.R data/EUForDam_REW.xlsx output data/Data_Klim_Tree_NUTS.xlsx
```

The third file, with host amount and climate per region, is optional.

Then check the results:

```bash
Rscript tests/test_pipeline.R data/EUForDam_REW.xlsx data/HOST_tables_borers_defoliators.xlsx data/Data_Klim_Tree_NUTS.xlsx
```

The second file is optional. If given, the generated host tables are compared with it cell by cell.

Requires R with the tidyverse, `readxl`, `writexl` and `testthat`.

## Data rules

| Situation | Meaning | Handling |
|---|---|---|
| `value1 = 0` | Monitored, no damage found | Kept as real zero |
| `value1` blank | Not recorded | Row dropped, never filled with 0 |
| No row for an agent/region/year | Not recorded | Left out, never filled with 0 |
| Repeated rows (same agent/host/year/region/unit) | Valid separate records | Kept, all counted in totals |
| `value2` | Same damage as `value1` in the other unit | Kept as `value_alt` with `unit_alt` (m3 <-> ha) |

### *Ips typographus*

*Ips typographus* records include the combined code `CambXyl_con&BB_con&SM` together with `ips_typ`, through the "Ips typographus" group column. This gives 77 regions in 16 countries and 586.7 million m³, matching Tables 1 and 2 of the paper. `ips_typ` alone gives 69 regions, 15 countries and 472 million m³.

Limitation: the combined code covers all spruce bark beetles, so the CZ and some DE values slightly overstate *I. typographus* itself.

## Guideline paper

Hlásny et al. 2025, *Divergent Trends in Insect Disturbance Across Europe's Temperate and Boreal Forests*, Global Change Biology 31:e70580. It was produced from this database, and the project follows its definitions.

**Time series.** A time series is one insect code × host tree × region × unit. The paper removed series with fewer than 6 years of non-zero damage. The pipeline does not delete them; it marks them in `eufordam_model_data.csv`:
- `series_id`;
- `series_nonzero_years`;
- `in_paper_series`: TRUE when the series has at least 6 years with damage.

**Table 2 check.** The QC report sheet `paper_table2` compares group totals with Table 2 (`reference/paper_table2.csv`), with and without the filter. With the filter:
- 11 of 18 groups match within 0.01%;
- all groups are within 2%;
- the largest gaps are borers on broadleaves (−1.9%) and borers on conifers (+1.6%).

## Host amount and climate

Input: `Data_Klim_Tree_NUTS_*.xlsx`, sheet `DATA_CLEAN`. Sources are documented in `Appendix_NUTS_Forests` (Appendix D). The file gives, per spatial unit:
- area (ha) and growing stock (m³) of spruce, pine, oak, fir, all conifers and all broadleaves;
- mean VPD, temperature and precipitation.

Fixes applied so the units match the damage data:

| Issue | Fix |
|---|---|
| `-` in host columns | Not available → NA, never 0 |
| Poland: code and name columns swapped | Swapped back (`BIA`, `GDA`, …) |
| Slovenia coded `SL` | → `SI` |
| Czechia host data at NUTS3 (CZ010…), damage at NUTS2 (CZ01…) | Host summed to NUTS2 (checked against the appendix); climate weighted by forest area |
| Berlin (DE3) lumped with Brandenburg (DE4); no data for Bremen (DE5) and Hamburg (DE6) | Host amount NA for these three |

Added to `eufordam_model_data.csv`:
- `host_amount`: the insect's host in that unit, following `host_amount_from`. Aggregate codes use all conifers or all broadleaves.
- `host_amount_columns`;
- `damage_rel` = `value / host_amount`, the fraction of host damaged that year;
- `vpd`, `tas`, `pr`.

`reference/group_host_columns.csv` maps each group to its host column(s), as in the paper's host matrix.

QC sheets:
- `units_without_host_data`;
- `host_density_suspicious`: m³/ha outside 20–1000;
- `damage_exceeds_host`: `damage_rel` > 1;
- `paper_table2_host_share`: Table 2 % column, reproduced for the spruce groups.

Known issues:
- BG and LT spruce/pine/oak m³ look like they are in thousands (0.03–0.7 m³/ha).
- *L. monacha* in Poland is matched to spruce area but mostly defoliates pine, so `damage_rel` > 1 there.
- The climate columns are labelled "2020-2022", while the paper describes 2000–2022 means.

## Outputs (`output/`)

- `eufordam_clean.csv`: one clean long table (snake_case columns, logical `grp_*` group flags).
- `eufordam_summaries.xlsx`:
  - `group_totals`: Europe-wide totals per group and unit (replaces `List1`).
  - `group_totals_country`: the same totals per country.
  - `group_totals_year`: the same totals per year.
  - `country_shares`: each country's share of total damage per unit.
  - `region_totals`: country × unit × tree type × region (replaces the pivot, for every country).
- `host_tables_borers_defoliators.xlsx`: host tables built automatically.
  - Four insect × host-tree 0/1 matrices: borers / defoliators on conifers / broadleaves.
  - `insects`: for each insect, its guild, host trees, host amount and number of regions/countries.
  - `insect_hosts`: the same as a long table, one row per insect × host.
- `eufordam_model_data.csv`: damage records joined with insect name, guild, common host name, host type and host amount. Aggregate codes are marked with `is_aggregate`. This is the starting table for modelling.
- `eufordam_qc.xlsx`: quality report. Covers dropped rows, repeated rows (listed for information; they are valid and kept), NUTS levels, coverage, value2/value1 ratios, category conflicts and odd labels. The report only flags; it changes nothing.

## Making your own tables

`R/make_table.R` builds any rows × columns table from the two base tables:
- `model_data`: one row per damage record;
- `insect_hosts`: one row per insect × host tree.

```r
source("R/eufordam_pipeline.R"); source("R/host_tables.R"); source("R/make_table.R")
res <- run_pipeline("data/EUForDam_REW.xlsx")
md  <- res$model_data
ih  <- res$host$insect_hosts

make_table(ih, rows = "host", cols = "insect", filter = guild == "borer" & host_type == "conifers")
make_table(md, rows = "insect", cols = "country", value = "n_regions")
make_table(md, rows = "country", cols = "year", value = "sum", filter = guild == "borer")
```

`value` can be:

| `value` | What each cell holds |
|---|---|
| `presence` | 1 if any record, else 0 |
| `any_damage` | 1 if any damage > 0, 0 if only zeros |
| `sum` | total damage |
| `mean` | mean damage per record |
| `n_records`, `n_regions`, `n_countries`, `n_years` | counts |

Cells with no record stay blank, never 0 (except for `presence`). Sums are only allowed within one unit. Combined codes are left out unless `include_aggregates = TRUE`.

`examples/example_tables.R` writes seven example tables to `output/example_tables.xlsx`.

## Reference files (`reference/`)

Expert knowledge that cannot be derived from the data. Edit these files, never the outputs.

- `insects.csv`: insect name per code, plus an optional host-amount override (e.g. *Melolontha*: all forest).
- `supervisor_host_matrix.csv`: host trees and host amount from the supervisor's matrix, and the group column used to count each insect's regions.
- `host_trees.csv`: Latin → common host name, tree type, and whether the forest data has its own column for that tree.

Derived from the data:
- guild: m3 records = borer, ha records = defoliator;
- host trees in the data file;
- region and country counts: any record counts, including zeros.

A new insect code shows up as a warning until it is added to `insects.csv`.

## Code

- `R/eufordam_pipeline.R`: functions (read, clean, QC, summaries).
- `run_pipeline.R`: command-line entry point.
- `R/host_tables.R`: host tables and modelling table.
- `R/host_climate.R`: host amount and climate per spatial unit.
- `R/make_table.R`: general table builder.
- `examples/example_tables.R`: example tables.
- `tests/test_pipeline.R`: checks the data rules and reproduces the Excel pivot.
