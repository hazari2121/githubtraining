# EU-ForDAM correlation metrics

Correlation metrics between forest damage agents in the EU Forest Damage
Database (EU-ForDAM, `EUForDam_REW3.xlsx`). This is the first step towards
latent factor models: co-fluctuation between agents is the information those
models use to fill gaps.

```
pip install pandas openpyxl scipy matplotlib
python correlation_analysis.py path/to/EUForDam_REW3.xlsx output
```

The raw spreadsheet is not in the repo. Results are in
`output/EUForDam_correlation_metrics.xlsx`; its `README` sheet describes every
sheet. Heatmaps are in `output/figures/`.

## Data preparation

Follows Hlasny et al. (2025, *Global Change Biology*, doi:10.1111/gcb.70580):

- Values are `value1`, transformed with log(x + 1). `value2` is not used.
- A zero is monitored zero damage. A missing `value1` (39 rows) is "not recorded".
- A time series (region × agent or group) with fewer than 6 non-zero values is dropped.
- Czech rows that share NUTS × year × agent are summed (14 regions mapped to 8 NUTS2 codes).
- The 18 groups of the paper's Table 1 come from the spreadsheet's indicator columns.
  `SP_CATEGORY` labels three spruce defoliators (`pri_abi`, `zei_gri`, `cep_spp`)
  as `CambXyl_con`, so it is not used for grouping.
- m³ (borers) and ha (defoliators) are never added together.

**Check:** after these steps the species-level totals match the paper's Table 2
almost exactly (sheet `group_totals`; e.g. *Ips acuminatus* 6,845,704 m³,
*Tomicus* 5,432,690 m³, *P. chalcographus* 28,145,602 m³).

## Correlation variants

Each answers a different question:

| Variant | What is correlated | What it captures |
|---|---|---|
| `europe_totals` | log annual totals summed over regions (n = 23 years) | Europe-wide synchrony. Closest to the paper's Tables 4–6. |
| `pooled_log` | log values over all region-years | Spatial **and** temporal co-variation: regions with a lot of one agent tend to have a lot of others |
| `within_region` | log values minus each region's mean | Year-to-year co-fluctuation within a region, with spatial level differences removed |
| `first_diff` | year-to-year change in log values | Same, but also removes shared trends and multi-year outbreak persistence. The strictest test. |
| `lag1` (agents) | agent A in year t vs agent B in year t+1 | Candidate lead/lag links (e.g. predisposition). Inflated by persistence. |
| `cooccurrence` (agents) | Jaccard index of damage > 0 | Whether agents occur together, regardless of magnitude |
| `sync` | mean region × region r of one agent's series | Spatial synchrony of each agent |

Pairs need ≥ 30 overlapping region-years (≥ 10 years for Europe totals).
Agent-level pair tables include Benjamini–Hochberg q-values.

## First observations

- **Comparison with the paper** (r between their Table 4–6 values and ours):
  `europe_totals` agrees well at levels 1–2 (r = 0.69 and 0.81), only partly at
  species level (0.43). The borer synchrony across conifer hosts reproduces:
  Picea–Pinus 0.91 (paper 0.88), Abies–Pinus 0.76 (0.73), Abies–Picea 0.73 (0.63).
- **Correlations shrink once trends are removed**: the median agent-pair r is
  0.18 pooled, 0.17 within regions and 0.07 for first differences. Much of the
  co-variation is shared trends and multi-year outbreaks (e.g. the post-2015
  bark-beetle wave), not year-to-year co-movement.
- **Strongest robust links** (still high after differencing): *Ips typographus*
  – *Pityogenes chalcographus* (0.73), *I. cembrae* – *P. chalcographus* (0.53) and
  *Tortrix viridana* – *Operophtera* spp. (0.45). All within one guild (bark beetles or oak defoliators), in line with the paper.
- **Reporting artefacts**: `CambXyl_con&BB_con&SM` (aggregate spruce bark
  beetles) is negatively correlated with `ips_typ` and `pit_cha` after
  differencing. In Germany, regions report either the species or the aggregate,
  rarely both, so the negative r reflects reporting categories, not ecology.
  Harmonization needs to handle this.
- *Ips typographus* has low spatial synchrony across its 60 regions
  (mean r ≈ 0.07), while *Ips acuminatus* is high (0.52).
