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

Requires R with the tidyverse, `readxl`, `writexl`, `testthat` and `openxlsx2` (for the PivotTable workbook).

Cloud sessions on the **githubtraining** environment install these automatically at startup. The modelling packages (`gllvm`, `brms`, `targets`, …) are installed when modelling starts. This takes about 15 minutes:

```bash
bash setup/install_modelling_packages.sh
```

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
- *L. monacha* is matched to spruce area, as in the paper (decision: keep). In Poland it mostly defoliates pine, so `damage_rel` > 1 there.
- The climate columns are labelled "2020-2022", while the paper describes 2000–2022 means.

## Outputs (`output/`)

- `EUForDAM_explore.xlsx`: explore without code.
  - **`query` (quickest):** pick rows, columns, measure, unit and filters from dropdowns, and the table recalculates instantly with plain Excel formulas (no refresh).
    - Rows/columns: guild, L1, L2, L3, insect, host_type, host_tree, country, region_code, year, in any combination.
    - Measures: total, records, average, any damage, lives on (1/0).
    - Up to 100 × 80 cells.
  - Eight ready-made PivotTables with slicers: insect × year, insect × country, insect × tree, tree × year, tree × country, country × year, relative damage, record counts.
  - Each table is split by unit (m³ / ha).
  - Drag fields to make any other combination.
  - Green tabs are the host matrices (1 = the insect lives on that tree, 0 = not), with Latin tree names:
    - `host matrix`: all insects × all trees, filterable and sortable by 1/0;
    - the four borer/defoliator × conifer/broadleaf boxes from the HOST_tables example;
    - `guild matrix`: insect × the paper's 18 feeding-guild/host groups (L1–L3), 1 = counted in that group;
    - `guilds (Table 1)`: the paper's Table 1 rebuilt from the data and compared with the paper (`reference/paper_table1.csv`).
  - Needs Excel (slicers) or LibreOffice.
- `EUForDAM_drilldown.xlsx`: the two drill-down sheets, in their own file.
  - **`drill-down`:** the whole hierarchy on one sheet.
    - Rows: guild → L1 → L2 → L3 → insect. Columns: years.
    - Expand or collapse with + / −.
    - Filter from the slicers on the side or the dropdowns above the table: guild, insect, host tree, country, region (NUTS), damage recorded (yes/no), single species (yes/no), in paper dataset (yes/no).
  - **`host drill-down`:** the same insect hierarchy × host trees (conifers/broadleaves → tree).
    - 1 = lives on that tree; at group level, 1 = at least one insect of the group does.
    - Filter from the slicers on the side or the dropdowns above the table: lives on (yes/no), guild, insect, host tree, country, region (NUTS), single species (yes/no).
    - Choosing a country or region shows only the insects recorded there.
  - Open in Excel, then Data → Refresh All once.
- `EUForDAM_pairs.xlsx` / `eufordam_pairs.csv`: every pair of insects that share regions.
  - Shared regions and region-years, years both caused damage, and damage overlap.
  - Correlation of yearly damage within regions (median, number of regions, share positive).
  - 1-year lag correlations in both directions.
  - Shared host trees, same guild / L1 / L2.
  - The `what the columns mean` sheet explains each column.
- `EUForDAM_automated_datasheet.xlsx`: the main deliverable. Sheets: `about`, `changes` (every change vs the raw sheet, with counts), `columns` (what each column means), `data` (the modelling table), `spatial_units` (host and climate).
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

## Web explorer

`web/index.html` is an interactive page. The pipeline writes its data to `output/web/` (`meta.json`, `records.json`, `pairs.json`). Tabs:
- **Explore:** any rows × columns table with filters.
- **Compare:** pick any two insects or groups (insect, L3, L2, L1, guild), each on all its host trees or one tree, plus where, years and filters. Shows yearly damage side by side, shared regions and years, damage overlap, correlation, 1-year lags and region-by-region results.
- **Pairs:** the pairs table, sortable and filterable.
- **Map:** colour regions by total or relative damage, years recorded, years with damage, share of zero years, or records, for any insect or group on any host tree. Hover or tap a region for its numbers; a region table sits below the map.
- **Coverage:** which insects are recorded where and when.

The Explore and Map tabs also offer **damage as % of host per year**, computed as follows:
- Damage is divided by the region's growing stock (borers, m³) or area (defoliators, ha) of the record's host tree. That is spruce, pine, fir or oak where the forest data has the tree, otherwise all conifers or all broadleaves.
- Each region-year's host is counted once.
- Bulgarian and Lithuanian species volumes (suspected thousands of m³) are left out.

Map boundaries:
- They come from GADM 4.1 (free for academic use), built once with `Rscript tools/build_map.R <gadm folder>` into `reference/map/map_geometry.json`.
- The lookup from region codes to boundaries is `reference/map/map_regions.csv`.
- Polish forestry districts have no official boundaries; they are drawn as approximate areas around each district seat.

It is published as a private claude.ai artifact. After a data update, rerun the pipeline and republish the page together with the three data files.

## Data on request

Write each question as one row of `requests.xlsx` (dropdowns show the allowed values; the `fields` sheet explains each column), then run:

```bash
Rscript run_requests.R
```

`run_pipeline.R` also does this automatically at the end. The answers go to `output/requests_output.xlsx`:
- one sheet per request;
- an `index` sheet showing which requests ran;
- a `vocabulary` sheet listing every valid name per level.

The hierarchies, coarse → fine:

| Dimension | Levels |
|---|---|
| insect | `guild` → `L1` (guild × conifers/broadleaves) → `L2` (guild × host genus) → `L3` (paper species) → `insect` (every code) |
| space | `europe` → `country` → `region` |
| time | `all` → `period` (5-year blocks) → `year` |
| host | `none` → `host_type` → `host_tree` |

L1–L3 are the paper's Table 1 groups. Every record is in exactly one group per level; records outside an L2/L3 group show as "other …". Totals therefore add up identically at every level, and a test checks this.

**Measures:** `total`, `mean_per_year`, `mean_per_record`, `max`, `relative_pct` (% of host damaged per year), `n_records`, `n_regions`, `n_countries`, `n_series`, `any_damage`.

**Filters:** insects (any level, `;`-separated), countries, years (`2010-2022`), unit, host trees, in paper dataset only, include mixed-species codes.

**Layout:** `columns` spreads one dimension across columns. Leave it at `none` for a long table with coverage columns.

**Rules:**
- Only recorded values are aggregated. A cell with no record is blank, never 0.
- m³ and ha are never added together.

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
- `aggregate_codes.csv`: readable names for codes that combine several insects.
  - The suffixes are Czech tree names: SM = smrk (spruce), BO = borovice (pine), MD = modřín (larch), DB = dub (oak).
  - Example: `Def_dec` → "Defoliators on broadleaves (unspecified)". It occurs only on horse chestnut in DE.
- `host_trees.csv`: Latin host name as in EU-ForDAM → common name and display label (e.g. *Picea abies* (spruce)), tree type, and whether the forest data has its own column for that tree.

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
- `R/web_data.R` + `web/index.html`: the web explorer.
- `R/pairs.R`: the insect pairs table.
- `R/query_sheet.R`: the dropdown-driven `query` sheet.
- `R/pivot_workbook.R`: the PivotTable workbook.
- `R/datasheet.R`: the human-readable datasheet workbook.
- `R/request.R` + `run_requests.R`: hierarchical data on request.
- `R/make_table.R`: general table builder.
- `examples/example_tables.R`: example tables.
- `tests/test_pipeline.R`: checks the data rules and reproduces the Excel pivot.

## Filter names in the workbooks

| Filter | yes | no |
|---|---|---|
| `single_species` | one named species | a mixed code covering several insects, e.g. "Bark beetles on spruce (unspecified)" |
| `in_paper_dataset` | part of the data the paper analysed (series with at least 6 years of damage) | left out by the paper |
| `damage_recorded` (damage drill-down) | damage > 0 in that record | monitored, zero damage |
| `lives_on` (host drill-down) | the insect lives on that tree | it does not |
